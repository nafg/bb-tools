package converse

/** The local Chatterbox Nano speech service the plugin starts on demand, run
  * by `uv` with its dependencies declared inline (PEP 723). Chatterbox Nano
  * (110M parameters) runs on the GPU: on an RTX 3070 a request takes about
  * 0.12 s plus 0.13 s per second of audio, so a reply's sentences follow one
  * another with no gap, where on the CPU (8 threads at low priority) each took
  * 1.5 to 2.8 s plus 0.87 to 0.97 s per second of audio. It speaks about 185
  * words a minute, word-perfect when transcribed back. It cannot stress a
  * marked word, so the speech route sends it the text without the marks.
  *
  * The model takes about 2.1 GB of the GPU, which the 8 GB card shares with
  * the desktop (about 3.1 to 3.6 GB) and which has frozen with about 1.45 GB
  * free; so it loads on the GPU only when enough is free to keep 2 GB free,
  * exits if free memory falls below that later, and Parakeet runs on the CPU
  * beside it (see [[conversecore.GpuTtsProvider]]). When the GPU cannot take it
  * at load time (no CUDA device, or too little free memory) it runs on the
  * CPU instead, at low priority, as the first version of this service did:
  * slower than the GPU but faster than real time, which beats a speech
  * provider that fails to start while the desktop is busy. Once started on
  * either device it stays there; it reports which in `/version`.
  *
  * It has no streaming API: a request is spoken whole, which is why the
  * player's one-sentence requests suit it, each generated while the one
  * before plays. A voice is [[BuiltInVoice]], the checkpoint's own, or the
  * path on the host of a WAV clip, over five seconds long, of a voice to
  * clone; a clip's conditioning is computed the first time it is used and
  * kept.
  *
  * Nano is not in the chatterbox-tts release on PyPI, so the script installs
  * the package from the GitHub commit that has it. It takes PyTorch's CUDA
  * build from PyPI (torch 2.6.0, which chatterbox-tts pins), 6 GB with its CUDA
  * libraries.
  */
private object ChatterboxServer:
  val Port = 8792

  /** The id of the checkpoint's own voice, the one the provider's settings
    * start with.
    */
  val BuiltInVoice = "built-in"

  /** CPU threads for the work around generation on the GPU, which launches
    * the GPU's work and so needs few.
    */
  val GpuThreads = 4

  /** CPU threads for generation when it falls back to the CPU: 8 of this
    * machine's 24, so that bb and the desktop stay responsive.
    */
  val CpuThreads = 8

  /** Where the script is written on the host before it is started. `uv` keeps
    * the environment it built for a script path and reuses it while it still
    * satisfies the script's requirements, and the unpinned `torch` is satisfied
    * by the CPU build the earlier path's environment holds. So the path changed
    * with the move to the GPU, to get an environment with the CUDA build; it
    * has to change again if the requirements ever need a different build.
    */
  val ScriptRelativePath = ".cache/bb-converse/chatterbox_nano_gpu_server.py"

  def startCommand(home: String): String =
    s"uv run --script $home/$ScriptRelativePath --port $Port --version $Version " +
      s"--gpu-threads $GpuThreads --cpu-threads $CpuThreads --built-in-voice $BuiltInVoice"

  val Script: String =
    """      |# /// script
      |# requires-python = ">=3.12,<3.14"
      |# dependencies = [
      |#   "chatterbox-tts @ git+https://github.com/resemble-ai/chatterbox@5de7a54aa4e5e2baadb0182dde554908b48b85c2",
      |#   "torch",
      |#   "fastapi",
      |#   "uvicorn",
      |# ]
      |# [tool.uv]
      |# exclude-newer = "2026-10-25T00:00:00Z"
      |# ///
      |# A local speech synthesis service around Chatterbox Nano, run on the GPU when
      |# it can take it and otherwise on the CPU: it speaks one sentence per request,
      |# whole, as 24 kHz 16-bit WAV. A voice is the model's built-in one or a
      |# reference clip of a voice to clone.
      |import argparse
      |import json
      |import os
      |import struct
      |import sys
      |import threading
      |import time
      |
      |# Lets the allocator grow segments instead of reserving whole new blocks, which
      |# keeps reserved-but-unused GPU memory down on this tight card.
      |os.environ.setdefault("PYTORCH_CUDA_ALLOC_CONF", "expandable_segments:True")
      |import torch
      |import uvicorn
      |from fastapi import FastAPI, HTTPException, Request
      |from fastapi.responses import Response
      |
      |parser = argparse.ArgumentParser()
      |parser.add_argument("--port", type=int, required=True)
      |parser.add_argument("--version", required=True)
      |parser.add_argument("--gpu-threads", type=int, required=True)
      |parser.add_argument("--cpu-threads", type=int, required=True)
      |parser.add_argument("--built-in-voice", required=True)
      |args = parser.parse_args()
      |
      |# The desktop shares this GPU and has frozen with about 1.45 GB of its memory
      |# free, so this process never lets free memory fall below the floor: it
      |# loads on the GPU only if what it still has to take (about 2.1 GB beyond
      |# the CUDA context it holds by the time it measures) leaves the floor, and
      |# exits if free memory drops below the floor later. When the GPU cannot
      |# take it, the CPU can: slower, but it still speaks.
      |FLOOR_MIB = 2000
      |NEEDED_TO_LOAD_MIB = 4200
      |
      |
      |def gpu_free_mib():
      |    free, _ = torch.cuda.mem_get_info()
      |    return free // 2**20
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
      |device = "cpu"
      |if not torch.cuda.is_available():
      |    print("using the CPU: no CUDA GPU", flush=True)
      |else:
      |    free = gpu_free_mib()
      |    if free < NEEDED_TO_LOAD_MIB:
      |        print(f"using the CPU: {free} MiB of GPU memory free, {NEEDED_TO_LOAD_MIB} needed "
      |              f"so that at least {FLOOR_MIB} MiB stay free", flush=True)
      |    else:
      |        device = "cuda"
      |        print(f"using the GPU: {free} MiB free", flush=True)
      |        threading.Thread(target=guard, daemon=True).start()
      |
      |if device == "cpu":
      |    os.nice(15)  # the desktop and bb come first
      |torch.set_num_threads(args.gpu_threads if device == "cuda" else args.cpu_threads)
      |from chatterbox.tts_turbo import ChatterboxTurboTTS  # noqa: E402  (after the thread count is set)
      |
      |t0 = time.time()
      |model = ChatterboxTurboTTS.from_pretrained(device=device, nano=True)
      |if model.conds is None:
      |    sys.exit("the Chatterbox Nano checkpoint has no built-in voice")
      |taken = f", {torch.cuda.memory_allocated() // 2**20} MiB allocated" if device == "cuda" else ""
      |print(f"loaded in {time.time() - t0:.1f} s on the {device}, torch {torch.__version__}{taken}", flush=True)
      |
      |# The conditionals of each voice spoken so far, by voice id: the built-in
      |# voice's, and those computed from a reference clip the first time it is used.
      |voices = {args.built_in_voice: model.conds}
      |# One generation at a time: a second would only slow both.
      |generating = threading.Lock()
      |
      |
      |def conditionals(voice):
      |    if voice in voices:
      |        return voices[voice]
      |    path = os.path.expanduser(voice)
      |    if not os.path.isfile(path):
      |        raise HTTPException(400, f"no voice clip at {voice}")
      |    try:
      |        model.prepare_conditionals(path)
      |    except AssertionError as error:  # the clip is too short
      |        raise HTTPException(400, f"{voice}: {error}") from error
      |    voices[voice] = model.conds
      |    return model.conds
      |
      |
      |def speak(text, voice):
      |    with generating:
      |        model.conds = conditionals(voice)
      |        wav = model.generate(text)
      |    return (wav[0].clamp(-1, 1) * 32767).to(torch.int16).cpu().numpy().tobytes()
      |
      |
      |def wav_file(pcm):
      |    return struct.pack("<4sI4s4sIHHIIHH4sI", b"RIFF", 36 + len(pcm), b"WAVE", b"fmt ", 16, 1, 1,
      |                       model.sr, model.sr * 2, 2, 16, b"data", len(pcm)) + pcm
      |
      |
      |started = time.time()
      |speak("Ready.", args.built_in_voice)
      |print(f"warmed up in {time.time() - started:.1f} s", flush=True)
      |
      |app = FastAPI()
      |
      |
      |# Speaks `text` with `voice` (the built-in voice's id, or the path of a
      |# reference clip over five seconds long) as a whole 16-bit 24 kHz WAV file.
      |# A plain def runs in a worker thread, so /version answers during a job.
      |@app.post("/tts")
      |def tts(body: dict):
      |    text = (body.get("text") or "").strip()
      |    voice = (body.get("voice") or "").strip() or args.built_in_voice
      |    if not text:
      |        raise HTTPException(400, "text is required")
      |    started = time.perf_counter()
      |    pcm = speak(text, voice)
      |    elapsed = time.perf_counter() - started
      |    audio_s = len(pcm) / 2 / model.sr
      |    print(json.dumps(dict(words=len(text.split()), audio_s=round(audio_s, 1), total_s=round(elapsed, 2),
      |                          rtf=round(elapsed / audio_s, 2) if audio_s else None)), flush=True)
      |    return Response(content=wav_file(pcm), media_type="audio/wav")
      |
      |
      |@app.get("/version")
      |def version():
      |    return {"version": args.version, "device": device}
      |
      |
      |uvicorn.run(app, host="127.0.0.1", port=args.port)
      |""".stripMargin

  /** Identifies this script and its start command, so a running server
    * started from an earlier one can be recognized and replaced.
    */
  lazy val Version: String = (Script, ScriptRelativePath, GpuThreads, CpuThreads, BuiltInVoice).hashCode.toHexString
