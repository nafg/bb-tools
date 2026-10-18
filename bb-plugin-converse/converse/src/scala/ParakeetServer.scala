package converse

/** The local Parakeet transcription service the plugin starts on demand: a
  * small OpenAI-compatible HTTP wrapper around onnx-asr, run by `uv` with its
  * dependencies declared inline (PEP 723). It transcribes each upload whole,
  * never split into chunks: splitting made Parakeet invent words in testing.
  * It uses the GPU when the CUDA runtime loads and loading the model would
  * still leave a safe margin of free GPU memory (this machine's desktop has
  * frozen with about 1.45 GB free), otherwise the CPU, on four threads so
  * bb's window stays responsive.
  *
  * It answers 413 for audio it cannot take whole: longer than `MaxSeconds`,
  * or more than its GPU memory cap allows at the moment. The cap keeps a long
  * recording from leaving gigabytes reserved after it is done. Measured on an
  * 8 GB RTX 3070 under the 4 GB cap: 120 seconds fit and 135 did not, so
  * `MaxSeconds` spares longer recordings an attempt that would fail. (Without
  * the cap, 210 fit and 240 did not; and this ONNX export fails outright on
  * 10 minutes even on the CPU.)
  */
private object ParakeetServer:
  val Port  = 8790
  val Model = "nemo-parakeet-tdt-0.6b-v3"
  val MaxSeconds = 120

  /** Parakeet transcribed two minutes of audio in under a second on the GPU,
    * so a request still unanswered after this has hung.
    */
  val AnswerWithinMs = 30_000.0

  /** Where the script is written on the host before it is started. */
  val ScriptRelativePath = ".cache/bb-converse/parakeet_server.py"

  def startCommand(home: String): String =
    s"uv run --script $home/$ScriptRelativePath --port $Port --model $Model --max-seconds $MaxSeconds --version $Version"

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
      |import threading
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
      |parser.add_argument("--max-seconds", type=float, required=True)
      |parser.add_argument("--version", required=True)
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
      |GPU_MEMORY_LIMIT = 4 * 1024**3
      |# The model takes about 3 GB on the GPU; loading it must leave at least 2 GB.
      |GPU_FREE_NEEDED_MIB = 5000
      |
      |
      |def gpu_free_mib():
      |    try:
      |        out = subprocess.run(["nvidia-smi", "--query-gpu=memory.free", "--format=csv,noheader,nounits"],
      |                             capture_output=True, text=True, timeout=10)
      |        return int(out.stdout.split()[0])
      |    except Exception:
      |        return None
      |
      |
      |free = gpu_free_mib()
      |use_gpu = "CUDAExecutionProvider" in ort.get_available_providers() and free is not None and free >= GPU_FREE_NEEDED_MIB
      |if not use_gpu:
      |    print(f"using the CPU (free GPU memory: {free} MiB, need {GPU_FREE_NEEDED_MIB})", flush=True)
      |providers = (
      |    [("CUDAExecutionProvider", {"gpu_mem_limit": GPU_MEMORY_LIMIT, "arena_extend_strategy": "kSameAsRequested"}),
      |     "CPUExecutionProvider"]
      |    if use_gpu
      |    else ["CPUExecutionProvider"]
      |)
      |cpu_options = ort.SessionOptions()
      |cpu_options.intra_op_num_threads = 4
      |try:
      |    if use_gpu:
      |        model = onnx_asr.load_model(args.model, model_dir, providers=providers)
      |    else:
      |        model = onnx_asr.load_model(args.model, model_dir, providers=providers, sess_options=cpu_options)
      |except Exception as error:
      |    if not use_gpu:
      |        raise
      |    # Typically other programs took GPU memory after the check.
      |    print(f"could not load {args.model} on the GPU ({str(error)[:200]}); using the CPU", flush=True)
      |    providers = ["CPUExecutionProvider"]
      |    model = onnx_asr.load_model(args.model, model_dir, providers=providers, sess_options=cpu_options)
      |print(f"loaded {args.model} with {providers[0]}", flush=True)
      |
      |
      |def out_of_memory(error):
      |    return "BFCArena" in str(error) or "Failed to allocate" in str(error)
      |
      |SAMPLE_RATE = 16000
      |app = FastAPI()
      |# One recording on the GPU at a time, so two cannot exhaust its memory together.
      |recognizing = threading.Lock()
      |
      |
      |# A plain def runs in a worker thread, so /version answers during a long job.
      |@app.post("/v1/audio/transcriptions")
      |def transcribe(file: UploadFile):
      |    decoded = subprocess.run(
      |        ["ffmpeg", "-nostdin", "-loglevel", "error", "-i", "pipe:0",
      |         "-ac", "1", "-ar", str(SAMPLE_RATE), "-f", "f32le", "pipe:1"],
      |        input=file.file.read(),
      |        capture_output=True,
      |    )
      |    if decoded.returncode != 0:
      |        raise HTTPException(400, f"could not decode the audio: {decoded.stderr.decode().strip()}")
      |    audio = np.frombuffer(decoded.stdout, dtype=np.float32)
      |    seconds = len(audio) / SAMPLE_RATE
      |    if seconds > args.max_seconds:
      |        raise HTTPException(413, f"{seconds:.0f} s is longer than the {args.max_seconds:.0f} s this server transcribes")
      |    try:
      |        with recognizing:
      |            text = model.recognize(audio, sample_rate=SAMPLE_RATE) if len(audio) else ""
      |    except Exception as error:
      |        if out_of_memory(error):
      |            raise HTTPException(413, f"not enough GPU memory for {seconds:.0f} s of audio") from error
      |        raise
      |    return {"text": text, "duration": seconds}
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
  lazy val Version: String = (Script, Model, MaxSeconds).hashCode.toHexString
