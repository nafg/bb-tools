package converse

/** The local Qwen3-TTS speech service the plugin starts on demand: Qwen3-TTS
  * 1.7B VoiceDesign, run by `uv` with its dependencies declared inline (PEP
  * 723). It speaks a word between *asterisks* with stress, which is why it is
  * offered: Pocket TTS and the others have no way to stress a word. A voice is
  * a written description, built anew for every request.
  *
  * The model's weights are quantized to nf4 once, on the first start, and
  * saved under `~/.cache/bb-converse/models`; generation is a handwritten
  * decode loop with one CUDA graph per codec frame, which on an RTX 3070 runs
  * at about 2.2x real time with first audio after about a quarter of a
  * second. It streams 24 kHz 16-bit WAV, half a second at a time, and stops
  * generating when the client goes away.
  *
  * It needs about 2.3 GB of the GPU, which the 8 GB card shares with the
  * desktop (about 3.1 GB) and which has frozen with about 1.45 GB free; so
  * it loads only when enough is free to keep 2 GB free, exits if free memory
  * falls below that later, and Parakeet runs on the CPU beside it.
  */
private object QwenServer:
  val Port = 8791

  /** The voice the provider's settings start with. */
  val DefaultVoice = "A clear, warm adult male voice, speaking at a natural conversational pace."

  /** Where the script is written on the host before it is started. */
  val ScriptRelativePath = ".cache/bb-converse/qwen_tts_server.py"

  def startCommand(home: String): String =
    s"uv run --script $home/$ScriptRelativePath --port $Port --version $Version"

  val Script: String =
    """# /// script
      |# requires-python = ">=3.12"
      |# dependencies = [
      |#   "qwen-tts",
      |#   "bitsandbytes",
      |#   "accelerate",
      |#   "fastapi",
      |#   "uvicorn",
      |#   "numpy",
      |# ]
      |# [tool.uv]
      |# exclude-newer = "2026-10-25T00:00:00Z"
      |# ///
      |# A local speech synthesis service around Qwen3-TTS 1.7B VoiceDesign, built
      |# for an 8 GB GPU shared with the desktop: the talker is quantized to nf4 and
      |# generated with a handwritten decode loop (static KV caches, one CUDA graph
      |# per codec frame), about 2.2x real time on an RTX 3070, streaming 24 kHz PCM
      |# as it goes. A word between *asterisks* in the text is stressed.
      |import argparse
      |import asyncio
      |import json
      |import os
      |import queue
      |import shutil
      |import struct
      |import sys
      |import threading
      |import time
      |
      |import numpy as np
      |import torch
      |import torch.nn.functional as F
      |import uvicorn
      |from fastapi import FastAPI, HTTPException, Request
      |from fastapi.responses import StreamingResponse
      |from starlette.concurrency import run_in_threadpool
      |
      |parser = argparse.ArgumentParser()
      |parser.add_argument("--port", type=int, required=True)
      |parser.add_argument("--version", required=True)
      |parser.add_argument("--checkpoint", default=os.path.expanduser("~/.cache/bb-converse/models/qwen3-tts-1.7b-voicedesign-nf4"))
      |args = parser.parse_args()
      |
      |MODEL = "Qwen/Qwen3-TTS-12Hz-1.7B-VoiceDesign"
      |LANGUAGE = "English"
      |dev = torch.device("cuda:0")
      |
      |# The desktop has frozen with about 1.45 GB of GPU memory free, so this
      |# process never lets free memory fall below the floor: it refuses to load
      |# unless what it still has to take (about 2.3 GB beyond the CUDA context it
      |# holds by the time it measures) leaves the floor, and exits if free memory
      |# drops below the floor later.
      |FLOOR_MIB = 2000
      |NEEDED_TO_LOAD_MIB = 4300
      |
      |
      |def gpu_free_mib():
      |    free, _ = torch.cuda.mem_get_info(dev)
      |    return free // 2**20
      |
      |
      |if not torch.cuda.is_available():
      |    sys.exit("no CUDA GPU; Qwen3-TTS runs on the GPU only")
      |free = gpu_free_mib()
      |if free < NEEDED_TO_LOAD_MIB:
      |    sys.exit(f"not enough free GPU memory to load Qwen3-TTS: {free} MiB free, {NEEDED_TO_LOAD_MIB} needed "
      |             f"so that at least {FLOOR_MIB} MiB stay free")
      |
      |
      |def guard():
      |    while True:
      |        f = gpu_free_mib()
      |        if f < FLOOR_MIB:
      |            print(f"free GPU memory fell to {f} MiB, below the {FLOOR_MIB} MiB floor; exiting", flush=True)
      |            os._exit(3)
      |        time.sleep(0.25)
      |
      |
      |threading.Thread(target=guard, daemon=True).start()
      |
      |# ---------------------------------------------------------------- checkpoint
      |from qwen_tts import Qwen3TTSModel  # noqa: E402
      |from transformers import BitsAndBytesConfig  # noqa: E402
      |from transformers.utils.hub import cached_file  # noqa: E402
      |
      |
      |def prepare_checkpoint(out):
      |    # Once: load the published bf16 weights, quantizing the talker to nf4 as they
      |    # load, and save the quantized weights, so later loads stream 4-bit weights
      |    # with no bf16 transient on the GPU.
      |    from safetensors.torch import save_file
      |    print(f"preparing the nf4 checkpoint in {out} (first start only)", flush=True)
      |    quantization = BitsAndBytesConfig(load_in_4bit=True, bnb_4bit_quant_type="nf4", bnb_4bit_compute_dtype=torch.bfloat16,
      |                                      llm_int8_skip_modules=["lm_head", "codec_head", "text_projection"],
      |                                      llm_int8_enable_fp32_cpu_offload=True)
      |    m = Qwen3TTSModel.from_pretrained(MODEL, device_map="cuda:0", dtype=torch.bfloat16, quantization_config=quantization)
      |    torch.cuda.synchronize()
      |    partial = out + ".partial"
      |    shutil.rmtree(partial, ignore_errors=True)
      |    os.makedirs(partial)
      |    snapshot = os.path.dirname(cached_file(MODEL, "config.json"))
      |    state = {k: v.detach().contiguous().cpu() for k, v in m.model.state_dict().items()}
      |    save_file(state, os.path.join(partial, "model.safetensors"), metadata={"format": "pt"})
      |    # save_pretrained trips over this nested config, so the config is written by
      |    # hand: the published one plus the quantization block it would add.
      |    config = json.load(open(os.path.join(snapshot, "config.json")))
      |    config["quantization_config"] = m.model.hf_quantizer.quantization_config.to_dict()
      |    config["dtype"] = "bfloat16"
      |    json.dump(config, open(os.path.join(partial, "config.json"), "w"), indent=2)
      |    m.processor.save_pretrained(partial)
      |    shutil.copyfile(os.path.join(snapshot, "generation_config.json"), os.path.join(partial, "generation_config.json"))
      |    # The speech tokenizer stays bf16; a copy keeps the checkpoint whole on its own.
      |    shutil.copytree(os.path.join(snapshot, "speech_tokenizer"), os.path.join(partial, "speech_tokenizer"), symlinks=False)
      |    os.rename(partial, out)
      |    print("checkpoint ready", flush=True)
      |
      |
      |if not os.path.exists(os.path.join(args.checkpoint, "model.safetensors")):
      |    prepare_checkpoint(args.checkpoint)
      |    # The weights loaded to prepare it do not all leave the GPU when released,
      |    # so the service starts over in a fresh process to load the checkpoint.
      |    sys.stdout.flush()
      |    os.execv(sys.executable, [sys.executable] + sys.argv)
      |
      |t0 = time.time()
      |m = Qwen3TTSModel.from_pretrained(args.checkpoint, device_map="cuda:0", dtype=torch.bfloat16)
      |torch.cuda.synchronize()
      |core = m.model
      |talker = core.talker
      |
      |
      |# The 151936 x 2048 text embedding table (620 MiB in bf16) is read only while
      |# building a prompt, a few dozen tokens, so it lives on the CPU.
      |class CpuEmbedding(torch.nn.Module):
      |    def __init__(self, embedding):
      |        super().__init__()
      |        self.embedding = embedding.cpu()
      |
      |    def forward(self, ids):
      |        return self.embedding(ids.cpu()).to(dev)
      |
      |
      |talker.model.text_embedding = CpuEmbedding(talker.model.text_embedding)
      |torch.cuda.empty_cache()
      |print(f"loaded in {time.time() - t0:.1f} s, free GPU memory {gpu_free_mib()} MiB", flush=True)
      |
      |tcfg = talker.config
      |cp = talker.code_predictor
      |ccfg = cp.config
      |dec = core.speech_tokenizer.model
      |vocoder = dec.decoder
      |UPSAMPLE = int(dec.decode_upsample_rate)
      |SAMPLE_RATE = int(dec.get_output_sample_rate())
      |NCG = tcfg.num_code_groups
      |EOS = tcfg.codec_eos_token_id
      |VOCAB = tcfg.vocab_size
      |HEAD_DIM = tcfg.head_dim
      |# The talker's KV cache holds the prompt and up to this many frames (12 per
      |# second of speech).
      |MAX_LEN = 1536
      |MAX_FRAMES = 1024
      |# Frames decoded to audio at a time (half a second), and the frames of left
      |# context given to the vocoder, as the package's own chunked decode does.
      |CHUNK_FRAMES = 6
      |CONTEXT_FRAMES = 25
      |
      |
      |# ---------------------------------------------------------------- prompt
      |class _Captured(Exception):
      |    pass
      |
      |
      |@torch.inference_mode()
      |def build_prefill(text, voice):
      |    # The package's own prompt assembly, stopped where it would hand the prompt
      |    # to the talker's generate.
      |    input_ids = m._tokenize_texts([m._build_assistant_text(text)])
      |    instruct_ids = [m._tokenize_texts([m._build_instruct_text(voice)])[0]]
      |    captured = {}
      |    original = talker.generate
      |
      |    def capture(**kw):
      |        captured.update(kw)
      |        raise _Captured
      |
      |    talker.generate = capture
      |    try:
      |        core.generate(input_ids=input_ids, instruct_ids=instruct_ids, languages=[LANGUAGE], non_streaming_mode=True)
      |    except _Captured:
      |        pass
      |    finally:
      |        talker.generate = original
      |    return captured["inputs_embeds"], captured["tts_pad_embed"]
      |
      |
      |# ---------------------------------------------------------------- decoder stacks over static caches
      |def rotate_half(x):
      |    x1, x2 = x[..., : x.shape[-1] // 2], x[..., x.shape[-1] // 2:]
      |    return torch.cat((-x2, x1), dim=-1)
      |
      |
      |def rope_table(rotary, n):
      |    pos = torch.arange(n, device=dev, dtype=torch.float32)
      |    freqs = torch.outer(pos, rotary.inv_freq.float())
      |    emb = torch.cat((freqs, freqs), dim=-1)
      |    return (emb.cos() * rotary.attention_scaling).to(torch.bfloat16), (emb.sin() * rotary.attention_scaling).to(torch.bfloat16)
      |
      |
      |class Stack:
      |    # A static KV cache and rope table for one decoder stack (the talker or the
      |    # code predictor).
      |    def __init__(self, layers, norm, rotary, cfg, maxlen):
      |        self.layers, self.norm, self.maxlen = layers, norm, maxlen
      |        nkv = cfg.num_key_value_heads
      |        self.k = [torch.zeros(1, nkv, maxlen, HEAD_DIM, dtype=torch.bfloat16, device=dev) for _ in layers]
      |        self.v = [torch.zeros(1, nkv, maxlen, HEAD_DIM, dtype=torch.bfloat16, device=dev) for _ in layers]
      |        self.cos, self.sin = rope_table(rotary, maxlen)
      |        self.positions = torch.arange(maxlen, device=dev)
      |
      |    def forward(self, x, pos):
      |        # x: (1, T, H) bf16; pos: (T,) cache positions. Returns the normed hidden state (1, T, H).
      |        T = x.shape[1]
      |        cos = self.cos[pos][None, None]
      |        sin = self.sin[pos][None, None]
      |        mask = (self.positions[None, :] <= pos[:, None])[None, None]
      |        for i, layer in enumerate(self.layers):
      |            a = layer.self_attn
      |            h = layer.input_layernorm(x)
      |            q = a.q_norm(a.q_proj(h).view(1, T, -1, HEAD_DIM)).transpose(1, 2)
      |            k = a.k_norm(a.k_proj(h).view(1, T, -1, HEAD_DIM)).transpose(1, 2)
      |            v = a.v_proj(h).view(1, T, -1, HEAD_DIM).transpose(1, 2)
      |            q = q * cos + rotate_half(q) * sin
      |            k = k * cos + rotate_half(k) * sin
      |            self.k[i].index_copy_(2, pos, k)
      |            self.v[i].index_copy_(2, pos, v)
      |            o = F.scaled_dot_product_attention(q, self.k[i], self.v[i], attn_mask=mask, scale=a.scaling, enable_gqa=True)
      |            x = x + a.o_proj(o.transpose(1, 2).reshape(1, T, -1))
      |            x = x + layer.mlp(layer.post_attention_layernorm(x))
      |        return self.norm(x)
      |
      |
      |talker_stack = Stack(talker.model.layers, talker.model.norm, talker.model.rotary_emb, tcfg, MAX_LEN)
      |predictor_stack = Stack(cp.model.layers, cp.model.norm, cp.model.rotary_emb, ccfg, NCG + 1)
      |predictor_positions = [torch.tensor([0, 1], device=dev)] + [torch.tensor([i], device=dev) for i in range(2, NCG + 1)]
      |
      |# ---------------------------------------------------------------- sampling, as the package's generate does it
      |TEMPERATURE, TOP_K, REPETITION_PENALTY = 0.9, 50, 1.05
      |suppressed = torch.zeros(VOCAB, dtype=torch.bool, device=dev)
      |suppressed[VOCAB - 1024:] = True
      |suppressed[EOS] = False
      |eos_only = torch.zeros(VOCAB, dtype=torch.bool, device=dev)
      |eos_only[EOS] = True
      |NEG = float("-inf")
      |
      |
      |def sample(logits):
      |    logits = logits / TEMPERATURE
      |    kth = torch.topk(logits, TOP_K, dim=-1).values[..., -1:]
      |    logits = logits.masked_fill(logits < kth, NEG)
      |    probs = torch.softmax(logits, dim=-1)
      |    noise = torch.empty_like(probs).exponential_()
      |    return (probs / noise).argmax(-1)
      |
      |
      |def talker_logits(h_last, history, eos_allowed):
      |    logits = talker.codec_head(h_last).float()
      |    penalized = torch.where(logits > 0, logits / REPETITION_PENALTY, logits * REPETITION_PENALTY)
      |    logits = torch.where(history[None], penalized, logits)
      |    logits = logits.masked_fill(suppressed[None], NEG)
      |    return torch.where(eos_only[None] & ~eos_allowed, torch.full_like(logits, NEG), logits)
      |
      |
      |# ---------------------------------------------------------------- one codec frame, as a CUDA graph
      |# The static tensors the graph reads and writes.
      |S = dict(
      |    h=torch.zeros(1, 1, tcfg.hidden_size, dtype=torch.bfloat16, device=dev),
      |    t=torch.zeros(1, dtype=torch.long, device=dev),
      |    pos=torch.zeros(1, dtype=torch.long, device=dev),
      |    history=torch.zeros(VOCAB, dtype=torch.bool, device=dev),
      |    eos_allowed=torch.zeros((), dtype=torch.bool, device=dev),
      |    codes=torch.zeros(NCG, dtype=torch.long, device=dev),
      |    pad=torch.zeros(1, 1, tcfg.hidden_size, dtype=torch.bfloat16, device=dev),
      |)
      |
      |
      |def frame_step():
      |    # The code predictor's 15 codes for the frame, then the talker's step and
      |    # the next frame's first code.
      |    t = S["t"]
      |    first = talker.model.codec_embedding(t)[None]
      |    x = torch.cat((S["h"], first), dim=1)
      |    summed = first.clone()
      |    codes = [t]
      |    for i in range(NCG - 1):
      |        hidden = predictor_stack.forward(cp.small_to_mtp_projection(x), predictor_positions[i])
      |        code = sample(cp.lm_head[i](hidden[:, -1]).float())
      |        codes.append(code)
      |        embedded = cp.model.codec_embedding[i](code)[None]
      |        summed = summed + embedded
      |        x = embedded
      |    S["codes"].copy_(torch.cat(codes))
      |    h = talker_stack.forward(summed + S["pad"], S["pos"])
      |    next_token = sample(talker_logits(h[:, -1], S["history"], S["eos_allowed"]))
      |    S["h"].copy_(h)
      |    S["t"].copy_(next_token)
      |    S["history"].index_fill_(0, next_token, True)
      |    S["pos"].add_(1)
      |
      |
      |@torch.inference_mode()
      |def capture_graph():
      |    S["pos"].fill_(100)
      |    side = torch.cuda.Stream()
      |    side.wait_stream(torch.cuda.current_stream())
      |    with torch.cuda.stream(side):
      |        for _ in range(3):
      |            frame_step()
      |    torch.cuda.current_stream().wait_stream(side)
      |    graph = torch.cuda.CUDAGraph()
      |    with torch.cuda.graph(graph):
      |        frame_step()
      |    torch.cuda.synchronize()
      |    return graph
      |
      |
      |def decode_chunk(all_codes, start, end):
      |    context = min(CONTEXT_FRAMES, start)
      |    codes = all_codes[start - context:end].clamp(min=0).T[None]
      |    return vocoder(codes)[0, 0][context * UPSAMPLE:]
      |
      |
      |# One generation owns the GPU and the static state at a time.
      |gpu_lock = threading.Lock()
      |
      |
      |@torch.inference_mode()
      |def generate(embeds, pad, max_frames, emit, cancelled):
      |    # Generates speech for the prompt, handing each decoded chunk of 16-bit PCM
      |    # to `emit` as it is ready, until the end of speech, `max_frames`, or
      |    # `cancelled` is set. Returns a report of the run.
      |    with gpu_lock:
      |        started = time.perf_counter()
      |        L = embeds.shape[1]
      |        S["pad"].copy_(pad)
      |        S["history"].zero_()
      |        h = talker_stack.forward(embeds, torch.arange(L, device=dev))
      |        S["eos_allowed"].fill_(False)
      |        first = sample(talker_logits(h[:, -1], S["history"], S["eos_allowed"]))
      |        S["h"].copy_(h[:, -1:])
      |        S["t"].copy_(first)
      |        S["history"].index_fill_(0, first, True)
      |        S["pos"].fill_(L)
      |        frames = []
      |        decoded = 0
      |        n = 0
      |        first_audio_at = None
      |        while n < max_frames and not cancelled.is_set():
      |            if n == 1:
      |                S["eos_allowed"].fill_(True)
      |            step()
      |            frames.append(S["codes"].clone())
      |            n += 1
      |            done = S["t"].item() == EOS
      |            if n - decoded >= CHUNK_FRAMES or done:
      |                audio = decode_chunk(torch.stack(frames), decoded, n)
      |                pcm = (audio.float().clamp(-1, 1) * 32767).to(torch.int16).cpu().numpy().tobytes()
      |                if first_audio_at is None:
      |                    first_audio_at = time.perf_counter()
      |                emit(pcm)
      |                decoded = n
      |            if done:
      |                break
      |        elapsed = time.perf_counter() - started
      |        return dict(prompt_tokens=L, frames=n, audio_s=round(n / 12, 1), total_s=round(elapsed, 2),
      |                    first_audio_ms=None if first_audio_at is None else round((first_audio_at - started) * 1000),
      |                    frames_per_s=round(n / elapsed, 1), cancelled=cancelled.is_set(), free_mib=gpu_free_mib())
      |
      |
      |step = capture_graph().replay
      |# A first run warms the vocoder, whose cold first chunk takes ten times as long.
      |embeds, pad = build_prefill("Ready.", "A calm adult voice.")
      |report = generate(embeds, pad, 60, lambda pcm: None, threading.Event())
      |print(f"warmed up: {report}", flush=True)
      |
      |
      |# ---------------------------------------------------------------- HTTP
      |def wav_header():
      |    # A 16-bit mono WAV whose sizes are placeholders, as a stream's must be;
      |    # the samples run to the end of the stream.
      |    placeholder = 2_000_000_000
      |    return struct.pack("<4sI4s4sIHHIIHH4sI", b"RIFF", placeholder + 36, b"WAVE", b"fmt ", 16, 1, 1,
      |                       SAMPLE_RATE, SAMPLE_RATE * 2, 2, 16, b"data", placeholder)
      |
      |
      |app = FastAPI()
      |
      |
      |# Speaks `text` (a word between *asterisks* stressed) with the voice `voice`
      |# describes, streaming 16-bit 24 kHz WAV from the first half second on.
      |@app.post("/tts")
      |async def tts(request: Request):
      |    body = await request.json()
      |    text = (body.get("text") or "").strip()
      |    voice = (body.get("voice") or "").strip()
      |    if not text:
      |        raise HTTPException(400, "text is required")
      |    if not voice:
      |        raise HTTPException(400, "voice, a description of the voice to speak with, is required")
      |    embeds, pad = await run_in_threadpool(lambda: build_prefill(text, voice))
      |    max_frames = min(MAX_FRAMES, MAX_LEN - embeds.shape[1])
      |    if max_frames < 5 * 12:
      |        raise HTTPException(413, f"the text is too long to speak in one piece ({embeds.shape[1]} prompt tokens)")
      |    chunks = queue.Queue()
      |    cancelled = threading.Event()
      |
      |    def run():
      |        try:
      |            report = generate(embeds, pad, max_frames, chunks.put, cancelled)
      |            print(json.dumps(dict(words=len(text.split()), **report)), flush=True)
      |            chunks.put(None)
      |        except BaseException as error:
      |            chunks.put(error)
      |
      |    threading.Thread(target=run, daemon=True).start()
      |
      |    async def stream():
      |        loop = asyncio.get_running_loop()
      |        try:
      |            yield wav_header()
      |            while True:
      |                item = await loop.run_in_executor(None, chunks.get)
      |                if item is None:
      |                    return
      |                if isinstance(item, BaseException):
      |                    raise item
      |                yield item
      |        finally:
      |            # The client went away (barge-in), or the stream ended.
      |            cancelled.set()
      |
      |    return StreamingResponse(stream(), media_type="audio/wav")
      |
      |
      |@app.get("/version")
      |def version():
      |    return {"version": args.version}
      |
      |
      |uvicorn.run(app, host="127.0.0.1", port=args.port)
      |""".stripMargin

  /** Identifies this script and its start command, so a running server
    * started from an earlier one can be recognized and replaced.
    */
  lazy val Version: String = Script.hashCode.toHexString
