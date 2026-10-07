# pai-web

Monitor and control every pai instance from a browser, phone first. A small
web server runs inside Emacs (no external program) and serves a page that
does what a pai buffer does:

- **Instances**: every pai chat with its status, model, thinking level,
  context use and cost; subagents nested under their parent; a badge when
  one finished while you were away, asks a question or waits on a prompt.
  New instance (pick a project), close, `/resume`, `/tree`, `/new`.
- **Chat**: the transcript as it streams (Markdown, collapsible thinking and
  tool calls with their arguments, results and diffs; code highlighted by
  Emacs' own major modes), prompts, steering while a run goes on, interrupt,
  slash commands and `!shell`, model and thinking pickers. Completion is the
  chat buffer's own: commands and their arguments at every level, `@file`
  and `*buffer` mentions, extension providers.
- **Panels**: what the buffer shows above the prompt (todo panel, memory
  status, activity, footer), the header statuses and the provider usage.
- **Attachments**: photos and images go to the model as images (scaled to
  1568 px in the browser); other files are saved and inserted as `@path`
  mentions.
- **Questions**: `ask_user_question` is answered in the page.
- **Minibuffer prompts** (`completing-read`, `y-or-n-p`, `yes-or-no-p`,
  `read-string`, `read-passwd`) are shown in the page too and whichever side
  answers first wins: prompts opened by something you did in the page, and
  prompts a pai buffer opens on its own (an extension's confirm while the
  agent works). Prompts you open by typing in Emacs stay in Emacs.
- **Other pai screens** (`/menu`, `/todo edit`, memory review, the
  ask_user dialog, compose, MCP approval...) open as a live view of the
  Emacs buffer: tap buttons and links, tap text to move point, edit widget
  fields, and use the key bar (RET, TAB, C-g, C-c C-c, q, n/p, arrows,
  sticky Ctrl/Meta for chords such as `C-c C-k`) or type: each key runs its
  binding as in Emacs. Only buffers a page action displayed and pai-related
  buffers (a `pai-` mode, a `*...pai...*` or `*MCP...*` name) are shown.

## Using it

```
/web start | stop | restart | status | open | password [clear] | logout-all
```

Settings are in `/menu` → **Web**: start automatically (with the first pai
session; off by default), port (8765), allowed host names, password, where
attached files go (`~/.pai/web/uploads/DATE/` or a folder of the project).
At most one server runs per Emacs.

In the message box, **Ctrl+Enter** (Cmd+Enter on macOS) always sends.  On a
desktop a plain Enter sends too and Shift+Enter starts a new line; on a
touch device Enter starts a new line.

**Without a password** the server listens on 127.0.0.1 only and needs no
login. **With a password** (`/web password`, at least 8 characters, stored
only as a salted hash) it listens on every interface and a browser logs in
once (a 180-day `HttpOnly`, `SameSite=Strict` cookie; changing the password
or `/web logout-all` logs every browser out). Failed logins are slowed down
and an address is locked out after five.

Requests must name an allowed host (an IP address, `localhost`, or a name
under "Allowed host names", e.g. your VPN name): that blocks DNS rebinding.
Requests that change something need the `X-Pai` header, which other sites
cannot send. The page only runs its own scripts (a strict CSP).

There is no TLS: use it on your machine, your LAN or through your VPN.
Browsers only allow system notifications on `https` or `localhost`; over
plain `http` from a phone you still get the badges, the page title and the
sound.

### WSL2

Under WSL2's default NAT networking the server is reachable from Windows at
`localhost:PORT`, but not from other devices. Either switch WSL to mirrored
networking (`networkingMode=mirrored` under `[wsl2]` in `%UserProfile%\.wslconfig`,
then `wsl --shutdown`) or forward the port from an elevated PowerShell:

```powershell
netsh interface portproxy add v4tov4 listenport=8765 listenaddress=0.0.0.0 connectport=8765 connectaddress=<WSL IP>
New-NetFirewallRule -DisplayName "pai-web" -Direction Inbound -LocalPort 8765 -Protocol TCP -Action Allow
```

(the WSL IP is `hostname -I` inside WSL and changes on reboot).

## How it stays out of Emacs' way

- Network I/O is asynchronous (`make-network-process` server, incremental
  parsing in the filter).
- `process-send-string` does not return until a socket took every byte, so a
  phone that drops off the network mid-response could freeze Emacs. Every
  response is therefore small (at most 48 KB, events at most 16 KB per poll)
  and only written in answer to a request that just arrived. Bigger payloads
  are "blobs" the page downloads in 24 KB pieces, one request at a time.
- Updates reach the page by long polling with acknowledgements; a page that
  stops polling is capped (4 MB queued, then one `resync`) and forgotten
  after 90 s.
- Everything a page asks for runs as a command of Emacs' command loop (an
  event in `unread-command-events`, not recorded in macros), never from a
  timer: a timer can fire inside other code's wait, where opening a
  minibuffer would hold that code up and answering it would unwind it.
- Change detection is a 0.5 s tick that does nothing while no page is
  connected; the transcript log is fed by advice on the chat buffer's
  render functions, installed only while the server runs.

## Files

| File | |
|---|---|
| `pai-web.el` | entry: lifecycle, routes, change detection, `/web`, settings |
| `pai-web-http.el` | the HTTP/1.1 server |
| `pai-web-bus.el` | pages, event queues, long polling, blobs |
| `pai-web-auth.el` | settings, password, logins, host check |
| `pai-web-instances.el` | instance list, chrome, transcript logs |
| `pai-web-prompt.el` | minibuffer prompts, `ask_user_question` |
| `pai-web-buffers.el` | remote buffers |
| `pai-web-actions.el` | the command-loop action queue, send, completion, uploads |
| `static/` | the page (plain HTML/CSS/JS, no build step) |

Tests: `test/pai-web-test.el`; `pai-web-prompt-forwarding-in-a-terminal`
runs a child Emacs in a pseudo-terminal (needs `script` and `curl`) to drive
a real minibuffer.
