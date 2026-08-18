# bb-plugin-converse

Two-way voice conversations with bb agent threads. A mic toggle in the thread
composer starts a voice session: the browser listens continuously, detects
utterances with an energy VAD, transcribes them through bb's voice
transcription, sends the text into the thread (steering the agent if it is
mid-turn), and speaks the agent's reply when the thread goes idle. Speaking
over the reply (barge-in) stops playback immediately.

The conversation logic is a port of
[claude-converse](https://github.com/nafg/claude-converse)'s core (VAD
semantics, sentence chunking, markdown cleanup, generation-based cancellation)
onto browser audio APIs and the bb plugin SDK.

## Layout

- `converse/` — plugin backend (Scala.js): RPC (session lease, utterance
  transcription + delivery), `thread.idle`/`thread.failed` handlers that
  publish speak signals, and an optional server-side TTS proxy route.
- `converse-app/` — frontend core (Scala.js): microphone/audio-graph
  ownership, VAD driving, MediaRecorder capture, upload, playback queue with
  prefetch, and barge-in. Exports `createController` consumed by `app.tsx`.
- `converse-core/` — pure shared logic (cross-built JVM/JS): `EnergyVad` and
  `Speech` (markdown cleanup + sentence chunking).
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

- **Speech synthesis** — `browser` (speechSynthesis, zero config, default) or
  `server` (the backend proxies an OpenAI-compatible `/v1/audio/speech`
  endpoint — e.g. a local Kokoro at `http://127.0.0.1:8880/v1/audio/speech`
  with model `kokoro`, or OpenAI with an API key).
- **TTS endpoint / model / voice / API key** — used in `server` mode.
- **Voice detection threshold** — normalized RMS (default `0.01`); raise it if
  background noise triggers recording.

## v1 behavior and limits

- One voice session per bb server; starting a session elsewhere (another
  thread, tab, or window) takes over and hard-stops the previous one,
  dropping any not-yet-spoken reply.
- The session stays bound to the thread it was started in and keeps running
  while you navigate around the app in the same tab; other threads' composers
  show that voice is active elsewhere. It does not follow pane focus (the
  plugin SDK has no focused-pane signal yet).
- Utterances are capped at 60 seconds (forced boundary, then listening
  resumes).
- Replies are spoken only when the thread reaches idle (no incremental
  speech), matching how Converse's Claude/Pi adapters behave.
