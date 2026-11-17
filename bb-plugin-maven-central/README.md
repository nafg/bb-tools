# bb-plugin-maven-central

A source for the subscriptions plugin (`bb-plugin-subscriptions`): threads subscribe to `maven:group:artifact:version`
and hear when that version can be resolved, or to `maven:group:artifact` and hear of each new version. It reads
repo1.maven.org directly, bypassing the CDN's cache, since the search site lags far behind what builds can resolve.

Build and install: `npm run build && bb plugin install . --yes`; after changes, `npm run reload`.
