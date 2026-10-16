package converse

/** The local Parakeet transcription service the plugin starts on demand: a
  * small OpenAI-compatible HTTP wrapper around onnx-asr, run by `uv` with its
  * dependencies declared inline (PEP 723). It transcribes each upload whole,
  * never split into chunks: splitting made Parakeet invent words in testing.
  * It uses the GPU when the CUDA runtime loads, otherwise the CPU.
  */
private object ParakeetServer:
  val Port  = 8790
  val Model = "nemo-parakeet-tdt-0.6b-v3"

  /** Where the script is written on the host before it is started. */
  val ScriptRelativePath = ".cache/bb-converse/parakeet_server.py"

  def startCommand(home: String): String =
    s"uv run --script $home/$ScriptRelativePath --port $Port --model $Model"

  val Script: String =
    """# /// script
      |# requires-python = ">=3.12"
      |# dependencies = [
      |#   "onnx-asr[hub]",
      |#   "onnxruntime-gpu[cuda,cudnn]",
      |#   "fastapi",
      |#   "uvicorn",
      |#   "python-multipart",
      |#   "numpy",
      |# ]
      |# ///
      |import argparse
      |import os
      |import subprocess
      |
      |import numpy as np
      |import onnxruntime as ort
      |import uvicorn
      |from fastapi import FastAPI, HTTPException, UploadFile
      |from huggingface_hub import snapshot_download
      |
      |REPOSITORIES = {"nemo-parakeet-tdt-0.6b-v3": "istupakov/parakeet-tdt-0.6b-v3-onnx"}
      |
      |parser = argparse.ArgumentParser()
      |parser.add_argument("--port", type=int, required=True)
      |parser.add_argument("--model", required=True)
      |args = parser.parse_args()
      |
      |# Loads the CUDA and cuDNN libraries the onnxruntime-gpu extras installed.
      |ort.preload_dlls()
      |import onnx_asr  # noqa: E402  (after the CUDA libraries are loaded)
      |
      |# A plain directory rather than the Hugging Face cache, whose symlinks
      |# onnxruntime refuses to load.
      |model_dir = os.path.expanduser(f"~/.cache/bb-converse/models/{args.model}")
      |snapshot_download(REPOSITORIES[args.model], local_dir=model_dir)
      |providers = (
      |    ["CUDAExecutionProvider", "CPUExecutionProvider"]
      |    if "CUDAExecutionProvider" in ort.get_available_providers()
      |    else ["CPUExecutionProvider"]
      |)
      |model = onnx_asr.load_model(args.model, model_dir, providers=providers)
      |print(f"loaded {args.model} with {providers[0]}", flush=True)
      |
      |SAMPLE_RATE = 16000
      |app = FastAPI()
      |
      |
      |@app.post("/v1/audio/transcriptions")
      |async def transcribe(file: UploadFile):
      |    decoded = subprocess.run(
      |        ["ffmpeg", "-nostdin", "-loglevel", "error", "-i", "pipe:0",
      |         "-ac", "1", "-ar", str(SAMPLE_RATE), "-f", "f32le", "pipe:1"],
      |        input=await file.read(),
      |        capture_output=True,
      |    )
      |    if decoded.returncode != 0:
      |        raise HTTPException(400, f"could not decode the audio: {decoded.stderr.decode().strip()}")
      |    audio = np.frombuffer(decoded.stdout, dtype=np.float32)
      |    text = model.recognize(audio, sample_rate=SAMPLE_RATE) if len(audio) else ""
      |    return {"text": text, "duration": len(audio) / SAMPLE_RATE}
      |
      |
      |uvicorn.run(app, host="127.0.0.1", port=args.port)
      |""".stripMargin
