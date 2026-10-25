package converse

/** The local Chatterbox Nano speech service the plugin starts on demand, run
  * by `uv` with its dependencies declared inline (PEP 723). Chatterbox Nano
  * (110M parameters) runs on the CPU faster than real time, so it leaves the
  * GPU alone: Parakeet keeps the GPU beside it, and the desktop its memory.
  * Measured on this machine with 8 of 24 threads at low priority, it
  * generates at 0.6 to 0.95 of real time and speaks about 185 words a
  * minute, word-perfect when transcribed back. It cannot stress a marked word,
  * so the speech route sends it the text without the marks.
  *
  * It has no streaming API: a request is spoken whole, which is why the
  * player's one-sentence requests suit it, each generated while the one
  * before plays. A voice is [[BuiltInVoice]], the checkpoint's own, or the
  * path on the host of a WAV clip, over five seconds long, of a voice to
  * clone; a clip's conditioning is computed the first time it is used and
  * kept.
  *
  * Nano is not in the chatterbox-tts release on PyPI, so the script installs
  * the package from the GitHub commit that has it.
  */
private object ChatterboxServer:
  val Port = 8792

  /** The id of the checkpoint's own voice, the one the provider's settings
    * start with.
    */
  val BuiltInVoice = "built-in"

  /** CPU threads for generation: 8 of this machine's 24, so that bb and the
    * desktop stay responsive.
    */
  val Threads = 8

  /** Where the script is written on the host before it is started. */
  val ScriptRelativePath = ".cache/bb-converse/chatterbox_nano_server.py"

  def startCommand(home: String): String =
    s"nice -n 15 uv run --script $home/$ScriptRelativePath --port $Port --version $Version " +
      s"--threads $Threads --built-in-voice $BuiltInVoice"

  val Script: String =
    """      |# /// script
      |# requires-python = ">=3.12,<3.14"
      |# dependencies = [
      |#   "chatterbox-tts @ git+https://github.com/resemble-ai/chatterbox@5de7a54aa4e5e2baadb0182dde554908b48b85c2",
      |#   "torch",
      |#   "torchaudio",
      |#   "fastapi",
      |#   "uvicorn",
      |# ]
      |# [tool.uv]
      |# exclude-newer = "2026-10-25T00:00:00Z"
      |# # torch and torchaudio are named above, though chatterbox-tts needs them
      |# # anyway, because a source applies only to a dependency the script names:
      |# # without that, uv takes PyTorch's CUDA build from PyPI, 6 GB with its CUDA
      |# # libraries, for a model that runs on the CPU.
      |# [[tool.uv.index]]
      |# name = "pytorch-cpu"
      |# url = "https://download.pytorch.org/whl/cpu"
      |# explicit = true
      |# [tool.uv.sources]
      |# torch = { index = "pytorch-cpu" }
      |# torchaudio = { index = "pytorch-cpu" }
      |# ///
      |# A local speech synthesis service around Chatterbox Nano, run on the CPU: it
      |# speaks one sentence per request, whole, as 24 kHz 16-bit WAV. A voice is the
      |# model's built-in one or a reference clip of a voice to clone.
      |import argparse
      |import json
      |import os
      |import struct
      |import sys
      |import threading
      |import time
      |
      |import torch
      |import uvicorn
      |from fastapi import FastAPI, HTTPException, Request
      |from fastapi.responses import Response
      |
      |parser = argparse.ArgumentParser()
      |parser.add_argument("--port", type=int, required=True)
      |parser.add_argument("--version", required=True)
      |parser.add_argument("--threads", type=int, required=True)
      |parser.add_argument("--built-in-voice", required=True)
      |args = parser.parse_args()
      |
      |torch.set_num_threads(args.threads)
      |from chatterbox.tts_turbo import ChatterboxTurboTTS  # noqa: E402  (after the thread count is set)
      |
      |t0 = time.time()
      |model = ChatterboxTurboTTS.from_pretrained(device="cpu", nano=True)
      |if model.conds is None:
      |    sys.exit("the Chatterbox Nano checkpoint has no built-in voice")
      |print(f"loaded in {time.time() - t0:.1f} s on {args.threads} CPU threads", flush=True)
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
      |    return (wav[0].clamp(-1, 1) * 32767).to(torch.int16).numpy().tobytes()
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
      |    return {"version": args.version}
      |
      |
      |uvicorn.run(app, host="127.0.0.1", port=args.port)
      |""".stripMargin

  /** Identifies this script and its start command, so a running server
    * started from an earlier one can be recognized and replaced.
    */
  lazy val Version: String = (Script, Threads, BuiltInVoice).hashCode.toHexString
