# Notes: a browser side-panel chat for pai (not built yet)

Goal: talk to a pai session from a side panel in Edge, with the current page as
context, like the Hermes browser extension does.

## How Hermes does it

Source: review-agent-kit `docs/browser-integration.md` §2B, §8–9, §12 and
`docs/component-map.md`.

- MV3 extension (min Chrome 116): service worker `background.js`, side-panel
  HTML, content scripts. Permissions include `debugger`, `scripting`, `tabs`,
  `sidePanel`, `storage`, `contextMenus`; host access to loopback and all
  http(s).
- Chat and page context: the side panel talks to the Hermes API/Gateway over
  an authenticated WebSocket. Page context is collected by content scripts
  (with redaction), and context-menu actions send a selection or page into the
  chat.
- Browser control is a separate *controller* protocol:
  1. `POST /v1/browser-control/register` to an existing session. Protocol v1
     plus controller, browser-profile and session IDs.
  2. The server returns a single-use ticket that lives about 30 s.
  3. The panel opens a WebSocket to `/v1/browser-control/ws`, passing the
     ticket in a subprotocol, never in the URL.
  4. After that, commands, results, cancels and heartbeats flow over this one
     connection.
- The server decides identity; the client never claims it. Capabilities are
  negotiated and fail closed. Privileged `browser_evaluate` / `browser_cdp`
  need an explicit developer mode.
- Browser-side safety:
  - Tab leases (30 min TTL, max 32) bound to a document generation.
  - Single-use approvals for consequential actions.
  - Password, OTP and payment fields are blocked.
  - Another owner's debugger is reported as `debugger_conflict`, never
    force-detached.

## Proposed architecture for pai

```
Edge MV3 extension (side panel + service worker)
   ⇅  ws://127.0.0.1:<port>/pai  (subprotocol "pai-panel-v1.<token>")
Emacs: small server (make-network-process :server t) inside a pai-panel extension
   ⇅  pai session buffer (pai-send-message / render hooks)
```

- **Transport**: Emacs listens on loopback only. The WebSocket handshake needs
  about 150 lines of elisp (`websocket.el` from MELPA works too). Plain HTTP
  plus SSE is a simpler fallback.
- **Auth**: a random token in `~/.pai/browser/panel-token` (mode 600). The user
  pastes it into the extension's options once. It travels in the subprotocol.
  Also check the `Origin: chrome-extension://<id>` header.
- **Messages** (JSON):
  - panel → Emacs:
    - `hello {token, version}`
    - `list-sessions`
    - `attach {session}`
    - `send {text, context?: {url, title, selection, annotations}}`
    - `abort`
  - Emacs → panel:
    - `sessions [...]`
    - `delta {text}` for streamed assistant output
    - `tool {name, status}`
    - `done`
    - `error {message}`
    - `approval {id, title, body}` (the panel answers `approve {id, decision}`)
- **Page context**: reuse the `/tab` and `/annotate` block formats
  (`<browser-tab>`, `<browser-annotations>`), so the agent sees the same thing
  from either entry point.
- **Browser control** stays with pai-browser (Playwright/CDP). The panel does
  not need `debugger` permission, which keeps the extension small: `sidePanel`,
  `activeTab`, `scripting` (selection/context only), `storage`.

## Open questions

- Which pai hooks stream assistant deltas to a non-buffer consumer? (A render
  hook or `message-update` event may be needed from pai core.)
- Should the panel stay bound to one session, or follow the most recently used
  pai buffer?
- Where do approvals appear when the user is in the browser: only in the
  panel, or mirrored in Emacs?
- Packaging: load unpacked from this directory, or publish privately to the
  Edge add-ons store?
- Multi-profile or multi-window Edge: one token per profile?
