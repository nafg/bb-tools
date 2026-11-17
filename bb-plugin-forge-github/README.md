# bb-plugin-forge-github

A source for the subscriptions plugin (`bb-plugin-subscriptions`): threads subscribe to GitHub pull requests, branches and tags, and this plugin
checks them through the `gh` CLI, using its existing login. The checking logic is shared with the
other forge plugin, in `bb-plugin-forge-shared`.

Build and install: `npm run build && bb plugin install . --yes`; after changes, `npm run reload`.
