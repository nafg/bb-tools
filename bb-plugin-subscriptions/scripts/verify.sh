#!/usr/bin/env bash
# Checks the live subscriptions plugin: that it is running, that what it last told each thread still matches GitHub
# and GitLab, and that threads got their messages exactly once. Covers PRs and MRs, branches and tags.
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

# Live state, worked out independently of the plugin's code, in the plugin's stored form (see WatchState.encode).
github_change() {
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
    | "change \({OPEN: "Open", MERGED: "Merged", CLOSED: "Closed"}[.state] // "Unknown:\(.state)") \($ci) \(.headRefOid)"'
}

gitlab_change() { # host project-uri number
  glab api --hostname "$1" "projects/$2/merge_requests/$3" | jq -r '
    .head_pipeline as $p
    | (if $p == null then "NoCi"
       elif $p.sha != .sha and ($p.ref | test("^refs/merge-requests/.*/(merge|train)$") | not) then "Pending"
       else {success: "Passed", failed: "Failed", canceled: "Canceled", canceling: "Canceled", skipped: "NoCi",
             created: "Pending", waiting_for_resource: "Pending", preparing: "Pending", pending: "Pending",
             running: "Pending", scheduled: "Pending", manual: "Pending", waiting_for_callback: "Pending"}[$p.status]
            // "Unknown:\($p.status)"
       end) as $ci
    | "change \({opened: "Open", locked: "Open", merged: "Merged", closed: "Closed"}[.state] // "Unknown:\(.state)") \($ci) \(.sha)"'
}

uri() { jq -rn --arg p "$1" '$p | @uri'; }

live_state() {
  local url=$1 host path
  host=$(sed -E 's#^https?://([^/]+).*#\1#' <<<"$url")
  path=$(sed -E 's#^https?://[^/]+/##; s#/$##' <<<"$url")
  case $path in
    */-/merge_requests/*) gitlab_change "$host" "$(uri "${path%%/-/*}")" "${path##*/}" ;;
    */pull/*) github_change "$url" ;;
    */-/tree/*)
      glab api --hostname "$host" "projects/$(uri "${path%%/-/*}")/repository/branches/$(uri "${path#*/-/tree/}")" |
        jq -r '"branch \(.commit.id)"' ;;
    */-/tags)
      glab api --hostname "$host" "projects/$(uri "${path%%/-/*}")/repository/tags?order_by=updated&per_page=100" |
        jq -r '"tags " + (map(.name) | join(" ")) | rtrimstr(" ")' ;;
    */tree/*)
      local repo=${path%%/tree/*}
      gh api --hostname "$host" "repos/$repo/branches/${path#*/tree/}" --jq '"branch \(.commit.sha)"' ;;
    */tags)
      local repo=${path%/tags}
      gh api --hostname "$host" graphql -F o="${repo%%/*}" -F r="${repo#*/}" -f query='query($o: String!, $r: String!) {
        repository(owner: $o, name: $r) { refs(refPrefix: "refs/tags/", first: 100,
          orderBy: {field: TAG_COMMIT_DATE, direction: DESC}) { nodes { name } } } }' \
        --jq '"tags " + ([.data.repository.refs.nodes[].name] | join(" ")) | rtrimstr(" ")' ;;
    *) echo "Unknown:url" ;;
  esac
}

# Maven Central targets are stored by their central.sonatype.com URL.
maven_live() {
  local rest=${1#https://central.sonatype.com/artifact/} g a v base
  IFS=/ read -r g a v <<<"$rest"
  base="https://repo1.maven.org/maven2/${g//.//}/$a"
  if [[ -n $v ]]; then
    case $(curl -s -o /dev/null -w '%{http_code}' -I "$base/$v/$a-$v.pom?bb=$(date +%s)") in
      200) echo "maven-version available" ;; 404) echo "maven-version pending" ;; *) echo "Unknown:http" ;;
    esac
  else
    echo "maven-versions $(curl -s "$base/maven-metadata.xml?bb=$(date +%s)" | grep -o '<version>[^<]*</version>' | sed 's/<[^>]*>//g' | tr '\n' ' ' | sed 's/ $//')"
  fi
}

echo
echo "== Active subscriptions: stored state against live state"
active=$(sqlite3 -separator $'\t' "$DB" "SELECT id, thread_id, url, state, ifnull(check_error, '') FROM subscriptions WHERE ended_at IS NULL ORDER BY id")
[[ -z $active ]] && echo "  none"
while IFS=$'\t' read -r id thread url stored check_error; do
  [[ -z ${id:-} ]] && continue
  if [[ $url == https://central.sonatype.com/* ]]; then live=$(maven_live "$url" 2>&1); else live=$(live_state "$url" 2>&1); fi
  if [[ $stored == change\ * ]]; then
    # The stored form also holds the updated time and the reviews and comments already reported; compare the rest.
    read -r k l c sha updated seen <<<"$stored"
    stored="$k $l $c $sha"
    seen_count=$([[ $seen == seen:* ]] && tr ',' '\n' <<<"${seen#seen:}" | grep -c . || echo "not yet read")
  fi
  echo "  #$id $url (thread $thread)"
  [[ $stored == change\ * ]] && echo "    reviews and comments already reported: $seen_count"
  echo "    stored: ${stored:0:160}"
  echo "    live:   ${live:0:160}"
  [[ -n $check_error ]] && problem "#$id is currently failing to check: $check_error"
  [[ $live == *Unknown:* ]] && problem "#$id has a state this script doesn't recognize"
  if [[ $live != "$stored" ]]; then
    read -r s_kind s_life _ <<<"$stored"
    read -r l_kind l_life l_ci _ <<<"$live"
    if [[ $s_kind == change && $l_life == "$s_life" && $l_ci == Pending ]]; then
      # Pending CI is never reported, so the stored result stays until the new commit's CI finishes.
      echo "    differs only because CI is running on a newer commit; nothing to deliver yet"
    elif [[ $s_kind == tags ]] && [[ -z $(comm -13 <(tr ' ' '\n' <<<"${stored#tags}" | sort) <(tr ' ' '\n' <<<"${live#tags}" | sort) | grep .) ]]; then
      echo "    differs only by tags that were deleted or pushed out of the newest 100; nothing to deliver"
    else
      echo "    DIFFERS. Expected to be delivered within a minute; rerun in two minutes to confirm."
    fi
  fi
done <<<"$active"

echo
echo "== Messages delivered in the last $DAYS days"
since=$(($(date +%s) - DAYS * 86400))
rows=$(sqlite3 -separator ' ' "$DB" "SELECT id, thread_id, ifnull(ended_at, ''), state FROM subscriptions WHERE created_at >= $since ORDER BY id")
declare -A logs
while read -r id thread ended kind lifecycle _; do
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
  if [[ -n $ended && $kind == change && $lifecycle != Open ]] && ! grep -q 'This subscription has ended' <<<"$msgs"; then
    problem "#$id ended as $lifecycle but its thread never got the ending message"
  fi
done <<<"$rows"

echo
if ((problems == 0)); then echo "OK: no problems found"; else echo "$problems problem(s) found"; exit 1; fi
