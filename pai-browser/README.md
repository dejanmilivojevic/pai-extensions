# pai-browser

A real Chromium browser for pai, driven through an existing MCP browser server
run by the `pai-mcp` extension:

- **Playwright MCP** (`@playwright/mcp`, default) — launches Edge headed or
  headless, or attaches to your own Edge via the *Playwright MCP Bridge*
  extension.
- **chrome-devtools-mcp** — launches Edge headed or headless, or attaches over
  CDP (`--remote-debugging-port`).

JavaScript runs through CDP evaluation, so page CSP does not block it; the
annotation overlay uses only CSSOM styles and event listeners.

## Tools

| Tool | What it does |
|---|---|
| `browser_page_info` | URL, title, selection, backend/mode, tab list |
| `browser_exec` | Run a JS function/expression in the page, JSON result, timeout |
| `browser_fetch` | Read-only **same-origin** GET/HEAD with the page's session (e.g. Azure DevOps REST from a logged-in tab); `save_to` writes the full body and returns its SHA-256 |
| `browser_screenshot` | Viewport / full page / element (Playwright) as an image; updates the live view |
| `browser_act` | navigate, back, reload, click, dblclick, hover, type, press, select, drag, scroll, tap, wait, new_tab, select_tab, close_tab |
| `browser_annotations` | Elements the user marked with `/annotate` |

With `direct-tools` on, the backend's complete tool set is also registered
(e.g. `pai_playwright_browser_run_code_unsafe`, `pai_chrome_devtools_performance_start_trace`)
once the server has connected at least once.

On chrome-devtools, `hover`, `drag` and `tap` are not available through
`browser_act` (the server only targets snapshot uids); use its direct tools.

## Commands

- `/browser [status|backend B|mode M|restart|stop|view|live]`
- `/tab` — attach the current tab (URL, title, selected text) to the next message; `/tab clear`
- `/annotate` — click elements in the page, type a note per element, Esc to stop;
  `/annotate done` attaches them (plus a screenshot file with the numbered
  badges) to the next message; `/annotate cancel` discards them.
- `M-x pai-browser-set-backend`, `pai-browser-set-mode`, `pai-browser-restart`,
  `pai-browser-stop`, `pai-browser-live-view`, `pai-browser-view-show`.

`*pai-browser*` shows the latest screenshot; `g` refreshes, `l` toggles live
refresh (only while the buffer is visible, never overlapping captures).

## Settings

Global `~/.pai/settings.json` key `pai-browser` (a project `.pai/settings.json`
may override single keys). Also editable in the settings UI, section *Browser bridge*.

| Key | Default | Meaning |
|---|---|---|
| `backend` | `playwright` | `playwright` or `chrome-devtools` |
| `mode` | `headed` | `headed`, `headless`, `attach` |
| `playwright-browser` | `msedge` | Playwright browser/channel |
| `executable` | Edge binary | Browser launched by chrome-devtools-mcp |
| `cdp-url` | `http://127.0.0.1:9222` | chrome-devtools attach endpoint |
| `profile-root` | `~/.pai/browser/` | Dedicated profiles, outputs, logs (never your main profile) |
| `viewport` | `1280x800` | Initial viewport |
| `packages` | `{playwright: "@playwright/mcp@0.0.82", chrome-devtools: "chrome-devtools-mcp@latest"}` | npm specs run with `npx -y` |
| `extra-args` | `{}` | Per-backend extra argv (list or shell-quoted string) |
| `npx` | found on `exec-path` | npx path |
| `direct-tools` | `true` | Expose the backend's own MCP tools |
| `auto-approve` | `false` | `true` sets `approveTools: false` (never ask); `false` keeps pai-mcp's own approval settings |
| `bypass-csp` | `true` | Playwright `contextOptions.bypassCSP` (attach: CDP `Page.setBypassCSP`, from the next load) |
| `live-view` | `false` | Start the live-view timer |
| `live-view-interval` | `2` | Seconds between captures |
| `screenshot-format` | `jpeg` | `jpeg`, `png`, `webp` |
| `screenshot-max-width` | `1280` | chrome-devtools downscale |
| `request-timeout-ms` | `180000` | Per MCP request (first launch can take ~60 s) |

The headed and headless modes share one persistent profile per backend under
`profile-root`, so log in once (headed) and headless runs reuse the session.
Server stderr goes to `profile-root/<backend>.log`.

## Attach to your own Edge

- **Playwright**: install *Playwright MCP Bridge* from the Chrome Web Store in
  Edge (Edge accepts Chrome extensions), then `/browser mode attach`; the
  bridge then asks in the browser which tab to connect (not tested here).
- **chrome-devtools**: start Edge with a debug port and a *separate* profile
  (recent Chromium versions refuse remote debugging on the default profile):
  `"/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge" --remote-debugging-port=9222 --user-data-dir="$HOME/.pai/browser/edge-attach"`,
  then `/browser backend chrome-devtools` and `/browser mode attach`.

## Notes

- Requires Node (`npx`) and Edge. Everything is async; stopping closes the
  server's stdin first so it can shut its browser down.
- pai-mcp's output guard (50 KB) applies to the *direct* tools: their
  screenshots may be saved to a file instead of returned inline.
  `browser_screenshot` returns the image directly.
- Tests: `test/pai-browser-test.el` (ERT, no browser).
- Future work: a browser side-panel chat, see `docs/side-panel-chat.md`.
