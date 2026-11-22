# Datastar, vendored

`datastar-1.0.4.js` is Datastar v1.0.4's browser bundle, unchanged, as
published at
`https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.4/bundles/datastar.js`
(sha256 `727844adfc825ee651fb93c544a2a739986f9a21820a94524b35f0cac470cf91`,
33,553 bytes, fetched 2026-11-22).  `LICENSE.md` is the project's licence
(MIT, Copyright (c) Star Federation) from the same tag.

The lab serves it from its own static files
(`<prefix>/static/vendor/datastar/datastar-1.0.4.js`) and points Gendl's
`gwl:*datastar-url*` at it when it publishes (page.lisp,
`publish-prompt-lab!`), so the sheet and every app or deployment the lab
serves load it from the lab's own origin, never a third party's.

To move to a newer Datastar: add its bundle and licence here under the new
version's name, change `*datastar-file*` in page.lisp, and prove the sheet
and a streaming app on it.
