# bb-plugin-local-git

A source for the subscriptions plugin (`bb-plugin-subscriptions`): threads subscribe to `git:/absolute/path#branch`
and hear each time that branch gets new commits, with their subjects, or is rewritten. It runs `git` on the machine
running the bb server, so the repository has to be there.

Build and install: `npm run build && bb plugin install . --yes`; after changes, `npm run reload`.
