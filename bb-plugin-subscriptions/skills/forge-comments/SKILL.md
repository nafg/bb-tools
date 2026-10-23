---
name: forge-comments
description: Mark comments and reviews this thread posts on a GitHub pull request or GitLab merge request, so its own subscription doesn't report them back. Use whenever posting a PR or MR comment, review, or reply, with gh, glab or any other way.
---

# Posting PR and MR comments

End the text of every comment, review or reply you post on a GitHub pull request or GitLab merge request with this
line, using this thread's id from `$BB_THREAD_ID`:

```
<!-- bb-thread: <thread id> -->
```

GitHub and GitLab don't display HTML comments, so readers don't see it. The subscriptions plugin uses it to skip
reporting your own comment back to this thread. Other threads subscribed to the same PR or MR still hear about it.

Example:

```sh
gh pr comment <url> --body-file - <<EOF
Rebased onto main; CI should pass now.

<!-- bb-thread: $BB_THREAD_ID -->
EOF
```

If you forget the line, nothing breaks: this thread just gets a message about its own comment.
