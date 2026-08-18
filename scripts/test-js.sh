#!/usr/bin/env bash
# Runs the cross-built pure-logic test suites under real Scala.js/Node.
#
# Bleep M10's Scala.js test runner is broken (uTest-only, and silently runs
# zero tests even then), so bleep runs these suites on the JVM only — which
# cannot catch JS-runtime divergences like regex flags java.util.regex
# accepts but Scala.js rejects. This sidecar covers that gap; delete it when
# bleep can run Scala.js tests itself.
#
# Versions are pinned to match bleep.yaml; if they stop resolving, fail
# loudly rather than drift.
set -euo pipefail
cd "$(dirname "$0")/.."
exec scala-cli test \
  bb-plugin-converse/converse-core/src \
  bb-plugin-converse/converse-core-test/src \
  --js \
  --scala 3.8.3 \
  --js-version 1.22.0 \
  --dependency "org.scalameta::munit::1.1.1"
