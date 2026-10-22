#!/usr/bin/env bash
# Checks the live subscriptions plugin: that it is running, that what it last told each thread still matches GitHub
# and GitLab, and that threads got their messages exactly once.
#
# Usage: verify.sh [days]   (default 7: how far back to check delivered messages)
# Needs bb, gh, glab, jq and sqlite3, and network access, so run it outside any sandbox.
# Exits 1 if it found a problem. A stored state that differs from the live one is reported but is only a problem
# if it is still there on a rerun two minutes later: the plugin checks every minute.

set -uo pipefail
DB=${SUBSCRIPTIONS_DB:-$HOME/.bb/plugins/subscriptions/data.db}
DAYS=${1:-7}
problems=0
problem() { echo "  PROBLEM: $*"; problems=$((problems + 1)); }

echo "== Plugin"
status=$(bb plugin list 2>&1 | awk '/^subscriptions@/ {p = 1; print; next} p && /^[^ ]/ {p = 0} p')
echo "$status"
grep -q ' running' <<<"$status" || problem "the plugin is not running"
grep -q 'last: ok' <<<"$status" || problem "the poll schedule's last run did not succeed"

echo
echo "== Plugin log: errors and warnings"
log=$(bb plugin logs subscriptions 2>&1)
flagged=$(grep -i -E 'error|warn' <<<"$log")
if [[ -n $flagged ]]; then
  echo "$flagged" | tail -20
  problem "the plugin logged $(wc -l <<<"$flagged") error or warning lines (last 20 above)"
else
  echo "  none"
fi

# Live state, worked out independently of the plugin's code, as "Lifecycle Ci sha".
github_live() {
  gh pr view "$1" --json state,headRefOid,statusCheckRollup --jq '
    def latest: group_by(if .__typename == "StatusContext" then .context else "\(.workflowName)/\(.name)" end)
      | map(max_by(.startedAt // ""));
    def result:
      if .__typename == "StatusContext" then
        {SUCCESS: "Passed", FAILURE: "Failed", ERROR: "Failed", PENDING: "Pending", EXPECTED: "Pending"}[.state] // "Unknown:\(.state)"
      elif .status != "COMPLETED" then "Pending"
      else {SUCCESS: "Passed", NEUTRAL: "Passed", SKIPPED: "Passed", FAILURE: "Failed", TIMED_OUT: "Failed",
            ACTION_REQUIRED: "Failed", STARTUP_FAILURE: "Failed", STALE: "Failed", CANCELLED: "Canceled"}[.conclusion]
           // "Unknown:\(.conclusion)"
      end;
    ([.statusCheckRollup // [] | latest[] | result]) as $r
    | (if ($r | length) == 0 then "NoCi"
       elif ($r | index("Failed")) then "Failed"
       elif ($r | index("Canceled")) then "Canceled"
       elif ($r | index("Pending")) then "Pending"
       elif ($r | all(. == "Passed")) then "Passed"
       else "Unknown:\($r | unique | join(","))" end) as $ci
    | "\({OPEN: "Open", MERGED: "Merged", CLOSED: "Closed"}[.state] // "Unknown:\(.state)") \($ci) \(.headRefOid)"'
}

gitlab_live() {
  local host project number
  read -r host project number < <(sed -E 's#^https?://([^/]+)/(.+)/-/merge_requests/([0-9]+).*#\1 \2 \3#' <<<"$1")
  glab api --hostname "$host" "projects/$(jq -rn --arg p "$project" '$p | @uri')/merge_requests/$number" | jq -r '
    .head_pipeline as $p
    | (if $p == null then "NoCi"
       elif $p.sha != .sha and ($p.ref | test("^refs/merge-requests/.*/(merge|train)$") | not) then "Pending"
       else {success: "Passed", failed: "Failed", canceled: "Canceled", canceling: "Canceled", skipped: "NoCi",
             created: "Pending", waiting_for_resource: "Pending", preparing: "Pending", pending: "Pending",
             running: "Pending", scheduled: "Pending", manual: "Pending", waiting_for_callback: "Pending"}[$p.status]
            // "Unknown:\($p.status)"
       end) as $ci
    | "\({opened: "Open", locked: "Open", merged: "Merged", closed: "Closed"}[.state] // "Unknown:\(.state)") \($ci) \(.sha)"'
}

echo
echo "== Active subscriptions: stored state against live state"
active=$(sqlite3 -separator ' ' "$DB" "SELECT id, thread_id, url, lifecycle, ci, head_sha, ifnull(check_error, '') FROM subscriptions WHERE ended_at IS NULL ORDER BY id")
[[ -z $active ]] && echo "  none"
while read -r id thread url lifecycle ci sha check_error; do
  [[ -z ${id:-} ]] && continue
  if [[ $url == *"/-/merge_requests/"* ]]; then live=$(gitlab_live "$url" 2>&1); else live=$(github_live "$url" 2>&1); fi
  stored="$lifecycle $ci $sha"
  echo "  #$id $url (thread $thread)"
  echo "    stored: $stored"
  echo "    live:   $live"
  [[ -n $check_error ]] && problem "#$id is currently failing to check: $check_error"
  [[ $live == *Unknown:* ]] && problem "#$id has a state this script doesn't recognize"
  if [[ $live != "$stored" ]]; then
    echo "    DIFFERS. Expected to be delivered within a minute; rerun in two minutes to confirm."
    # Pending CI is never reported, so a stored final result with live Pending on a new commit is expected.
    read -r l_life l_ci l_sha <<<"$live"
    if [[ $l_life == "$lifecycle" && $l_ci == Pending ]]; then echo "    (CI is running on a newer commit; nothing to deliver yet)"; fi
  fi
done <<<"$active"

echo
echo "== Messages delivered in the last $DAYS days"
since=$(($(date +%s) - DAYS * 86400))
rows=$(sqlite3 -separator ' ' "$DB" "SELECT id, thread_id, lifecycle, ifnull(ended_at, '') FROM subscriptions WHERE created_at >= $since ORDER BY id")
declare -A logs
while read -r id thread lifecycle ended; do
  [[ -z ${id:-} ]] && continue
  if [[ -z ${logs[$thread]+x} ]]; then
    logs[$thread]=$(bb thread log "$thread" --all --json 2>/dev/null |
      jq -r '.[] | select(.type == "client/turn/requested") | .data.input[]?.text // empty
             | select(startswith("[bb subscription #")) | gsub("\n"; " ")')
  fi
  msgs=$(grep -F "[bb subscription #$id]" <<<"${logs[$thread]}")
  echo "  #$id (thread $thread):"
  if [[ -n $msgs ]]; then sed 's/^/    /' <<<"$msgs"; else echo "    no messages"; fi
  dups=$(sort <<<"$msgs" | uniq -d)
  [[ -n $msgs && -n $dups ]] && problem "#$id delivered the same message more than once: $dups"
  if [[ -n $ended && $lifecycle != Open ]] && ! grep -q 'This subscription has ended' <<<"$msgs"; then
    problem "#$id ended as $lifecycle but its thread never got the ending message"
  fi
done <<<"$rows"

echo
if ((problems == 0)); then echo "OK: no problems found"; else echo "$problems problem(s) found"; exit 1; fi
