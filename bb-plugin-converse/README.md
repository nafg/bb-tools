# bb-plugin-converse

Two-way voice conversations with bb agent threads. A mic toggle in the thread
composer starts a voice session: the browser listens continuously, detects
utterances with Silero V5 speech detection, transcribes them through bb's voice
transcription, sends the text into the thread (steering the agent if it is
mid-turn), and speaks the agent's reply when the thread goes idle. Speaking
over the reply (barge-in) stops playback immediately.

The conversation logic is a port of
[claude-converse](https://github.com/nafg/claude-converse)'s core (sentence
chunking, markdown cleanup, generation-based cancellation)
onto browser audio APIs and the bb plugin SDK.

## Layout

- `converse/` — plugin backend (Scala.js): RPC (session lease, utterance
  transcription + delivery), `thread.idle`/`thread.failed` handlers that
  publish speak signals, and an optional server-side TTS proxy route.
- `converse-app/` — frontend core (Scala.js): microphone/audio-graph
  ownership, Silero VAD driving, preview capture, upload, playback queue with
  prefetch, and barge-in. Exports `createController` consumed by `app.tsx`.
- `converse-core/` — pure shared logic (cross-built JVM/JS): speech-detector
  segment state and `Speech` (markdown cleanup + sentence chunking).
- `converse-core-test/` — munit tests for the pure logic (JVM).
- `app.tsx` — thin React shell: the composer-action toggle with a live mic
  level meter, wired to the Scala.js controller via
  `useRpc`/`useRealtime`/`useComposerView`/`useSettings`.

## Build and install

```sh
npm run build          # bleep-links server.js and app-core.cjs
bb plugin install .    # from this directory
```

## Settings

- **Speech synthesis** — `browser` (speechSynthesis, zero config, default);
  `pocket-tts` (a local Pocket TTS the plugin starts and stops); `qwen3-tts`
  (a local Qwen3-TTS 1.7B VoiceDesign the plugin starts and stops: voices are
  written descriptions, and a word the agent puts between *asterisks* is
  stressed; it takes about 2.3 GB of the GPU, so Parakeet runs on the CPU
  while it is the provider); or an OpenAI-compatible `/v1/audio/speech`
  endpoint (`kokoro`, e.g. a local Kokoro at
  `http://127.0.0.1:8880/v1/audio/speech`, or `openai-compatible` with an API
  key).
- **TTS endpoint / model / voice / API key** — used in `server` mode.
- **Minimum speech probability** — Silero probability from 0 to 1 (default
  `0.3`); raise it if background noise triggers recording.
- **End-of-speech silence** — milliseconds of quiet before an utterance is
  submitted (default `3000`). Increase it to allow longer thinking pauses.
- **Continuation wait** — maximum time the agent's
  `converse_wait_for_more_speech` tool blocks for the next utterance (default
  `8000` milliseconds). Speech received during the wait becomes part of the
  current turn instead of a steer.

## v1 behavior and limits

- One voice session per bb server; starting a session elsewhere (another
  thread, tab, or window) takes over and hard-stops the previous one,
  dropping any not-yet-spoken reply.
- The session stays bound to the thread it was started in and keeps running
  while you navigate around the app in the same tab; other threads' composers
  show that voice is active elsewhere. It does not follow pane focus (the
  plugin SDK has no focused-pane signal yet).
- Replies are spoken only when the thread reaches idle (no incremental
  speech), matching how Converse's Claude/Pi adapters behave.
