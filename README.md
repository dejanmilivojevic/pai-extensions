# pai-extensions — extensions for pai

> [!WARNING]
> **This project was 100% vibe coded.** Every line was written by an AI agent,
> and it may contain a lot of bugs. Use at your own risk.

Optional extensions for [pai](https://github.com/dejanmilivojevic/pai), the Pi
Agent for Emacs. The core works without any of them; each one adds tools,
commands, providers or UI through pai's extension API (see *Extensions* in the
core README).

## Extensions

| Extension | What it adds |
|-----------|--------------|
| [`pai-anthropic/`](pai-anthropic/) | Anthropic hosted provider (API key; OAuth handler for `/login anthropic`) |
| [`pai-openrouter/`](pai-openrouter/) | OpenRouter hosted provider |
| [`pai-ask-user/`](pai-ask-user/) | `ask_user_question`: the agent asks you a question and waits for the answer |
| [`pai-context/`](pai-context/) | Context-usage report and full-context dump |
| [`pai-dap/`](pai-dap/) | DAP debugger tool |
| [`pai-dashboard/`](pai-dashboard/) | A welcome dashboard at the top of new conversations |
| [`pai-interactive-subagents/`](pai-interactive-subagents/) | Live subagent sessions in sibling buffers |
| [`pai-subagents/`](pai-subagents/) | Older headless subagent delegation (use this or the interactive one, not both) |
| [`pai-learn/`](pai-learn/) | A teaching system with graded quizzes and in-Emacs diagrams |
| [`pai-lsp/`](pai-lsp/) | Language-server code intelligence tool |
| [`pai-mcp/`](pai-mcp/) | Token-efficient MCP proxy tool and `/mcp` |
| [`pai-memory/`](pai-memory/) | Learning memory: observers, long-term memory, skill learning and curation |
| [`pai-prompt-snippets/`](pai-prompt-snippets/) | Mix-and-match prompt rules toggled per message |
| [`pai-shake/`](pai-shake/) | Mechanical context reduction (`/shake`) |
| [`pai-todo/`](pai-todo/) | A phased todo list the agent keeps while it works |
| [`xwidget-browser/`](xwidget-browser/) | A real browser for the agent via xwidget-webkit |

`pai-todo/` and `pai-prompt-snippets/` have their own, more detailed READMEs.

## Installation

The repository holds one **subdirectory per extension**; each is loaded as a
self-contained unit (entry file `<name>/<name>.el`, with the directory on
`load-path` so multi-file extensions can `require` their own siblings). Loose
top-level `.el` files load too, for simple single-file extensions.

Clone it into `extensions/` of your pai checkout and enable everything
home-wide by linking it as `~/.pai/extensions`:

```sh
git clone https://github.com/dejanmilivojevic/pai
git clone https://github.com/dejanmilivojevic/pai-extensions pai/extensions
mkdir -p ~/.pai
ln -sT "$PWD/pai/extensions" ~/.pai/extensions
```

Create the symlink only when `~/.pai/extensions` is absent. The command above
refuses to replace an existing path; if you already maintain that directory,
copy or link the individual extension directories into it instead. No provider
forms or manual extension loading in `init.el` are required: each pai instance
automatically loads the home extensions.

For project-only use, put the desired extensions in `<project>/.pai/extensions/`
instead; only instances for that trusted project load them. Extensions are
switched on and off under *Extensions* in `/menu` (globally or per project;
changes apply on `/reload`).

**Hosted providers.** Set `ANTHROPIC_API_KEY` and `OPENROUTER_API_KEY`, or use
`/login PROVIDER`. The Anthropic extension also installs the OAuth handler for
`/login anthropic`; subscription authentication has not been verified.

## Interactive subagents

The `pai-interactive-subagents/` extension makes a subagent a **live agent
session in its own buffer, opened next to yours**, instead of a headless run
hidden inside the parent. Through one `subagent` tool the agent opens a child
session, gives it a task, and keeps talking to it; meanwhile you can read that
buffer, scroll it, and **type into it yourself** — it is a normal chat session
with its own prompt, model, and session file. The child can talk back to the
agent that spawned it at any time with its `reply_to_parent` tool (you can do
the same by hand with `/parent TEXT`).

Nothing blocks: a launch returns a receipt immediately and the child's answer
arrives as a follow-up parent turn (queued as steering when the parent is
mid-run); with `async: false` only that one tool call stays pending until the
child finishes the turn. A session stays alive between turns, so the parent
keeps delegating to the same child instead of starting over.

| `subagent` action | Effect |
|-------------------|--------|
| `launch` (default) | Open a session for a role and give it a task |
| `say` | Send another message to a live session (`id`) |
| `read` | Read the tail of a session's transcript |
| `status` | List this parent's sessions (also shown above the prompt) |
| `stop` | Interrupt the current turn; the session stays open |
| `close` | End a session and close its buffer |
| `list` | List the available roles |

Builtin roles: `scout` (codebase recon), `researcher`, `evidence-auditor`,
`worker` (implementation), `reviewer`, `oracle` (second opinion), `delegate`.
Ask in plain language ("open a reviewer on this diff", "ask oracle to
challenge this plan") — the model calls the tool.

Slash commands: `/subagents [id]` (list), `/subagents-open [id]` (show its
buffer), `/subagents-stop [id]`,
`/subagents-close [id]`, `/subagents-models [role]`, `/subagents-model [role]`,
`/subagents-backend [role]`, `/subagents-roles`, `/subagents-reload`, and — in
a child buffer — `/parent TEXT`.

**Which model fills which role** (strongest first): per-run tool arg →
`:interactive-subagents` `:overrides` setting → role frontmatter →
`:default-model` → the parent session model (`inherit`). The settings screen
(`/menu` → *Interactive subagents*) edits the defaults and one row per role
(model · thinking · backend); `/subagents-model reviewer` does the same from
the prompt.

**Backends.** How a subagent session is created and driven is pluggable: the
`"pai"` backend (a pai chat buffer) ships with the extension, and another one —
an `agent-shell` buffer, a remote session, a comint process — is a plist
registered with `pai-isub-register-backend` providing `:start`/`:send` (plus
optional `:busy-p`, `:interrupt`, `:close`, `:metrics`, `:transcript`). The
child reports upstream through the `:emit` callback it is handed
(`ready`/`busy`/`turn-end`/`reply`/`exit`); roles, model resolution, delivery
into the parent turn and the status block are backend-agnostic. Pick the
backend per role (`backend:` frontmatter, `/subagents-backend`) or globally
(`:default-backend`).

**Defining your own roles**: drop a markdown file in `~/.pai/subagents/` (home,
every instance) or `<project>/.pai/subagents/` (trusted project only). Frontmatter
keys `name`, `description`, `model`, `thinking`, `backend`, `tools`
(comma-separated), `context` (`fresh`|`fork`); the body is the role's system
prompt. Example:

```markdown
---
name: planner
description: Breaks a task into an ordered plan
model: anthropic/claude-sonnet-4
thinking: high
context: fork
---
You are planner. Produce an ordered, minimal plan for the task. Never edit files.
```

Children never receive the `subagent` tool (recursion guard, lift it with
*Allow nested subagents*) and inherit the parent conversation only under
`context: fork`. Turns **you** drive in a child buffer stay there unless
*Report every turn* is on; turns the parent asked for are always reported.

The older `pai-subagents/` extension (a port of
[pi-subagents](https://github.com/nicobailon/pi-subagents)) still ships and
runs children headlessly. It registers a `subagent` tool of the same name, so
enable one or the other, not both.

## Learning memory

`pai-memory/` is being built in phases from
[`docs/SPEC-learning-memory.md`](https://github.com/dejanmilivojevic/pai/blob/main/docs/SPEC-learning-memory.md). All of V1 works: **session memory**,
**long-term memory**, the **learning loop**, and **skill curation**:

- **Observers.** As soon as enough new conversation has built up -- checked
  after every turn, so a long run does not have to finish first -- background
  observer workers turn the new part
  of the conversation into short, dated observations. These are stored in the
  session and follow `/tree` branches. While an observer runs, it shows as a line
  above the prompt, like a running subagent.
- **Compaction from observations.** `/compact` and auto-compaction render the stored
  observations instead of asking a model for a summary. The recent part stays word
  for word. If the observers are behind, only the unobserved part is summarized.
- **Consolidation.** When the stored observations grow past ~20k tokens, a background
  consolidator files the oldest ones into per-session topic files and a short
  `JOURNEY.md` under `~/.pai/memory/projects/<project>/sessions/<id>/`. The
  compaction block lists those topics (a memory map) and the journey, so the agent
  can read the details when it needs them. Forks keep their source's observations
  and topics.
- **Cost control.**
  - Presets: `off`, `economy`, `balanced` (default), `thorough`, `custom`.
  - Per-session overrides.
  - Budget caps: $1 per session and $5 per day by default. When a cap is reached,
    background work pauses.
  - Every run's cost is recorded. `/memory` shows the status and spend.
- **Long-term memory.** Three small Markdown files survive across sessions:
  `~/.pai/memory/USER.md` (you), `~/.pai/memory/MEMORY.md` (environment and tools),
  and a per-project `MEMORY.md`. Each holds entries separated by `§` lines and has a
  size limit. They are added to the system prompt as a `<memory>` section when a
  session starts. The agent saves to them with the `memory` tool, but only when you
  ask it to remember something. Changes take effect in the next session, and every
  change is logged and can be undone.
  - **Where entries come from:** `~/.pai/memory/entries.json` records this for each
    entry, along with a confidence score and how often later sessions confirmed it.
    `/memory why QUOTE` shows the full chain, back to the conversation.
  - **Temporary facts:** an entry can carry an expiry date (`expires`). Once it
    passes, the entry leaves the prompt and the curator proposes removing it.
  - **Growing memory:** set *Memory in the prompt* to `retrieval`. The prompt then
    gets only pinned entries plus the best few, and the rest is found through
    `memory_search`.
  - **Team memory:** in a trusted project, `.pai/memory/PROJECT.md` is loaded before
    your own project memory. It is committed with the code, and the learning loop
    only proposes edits to it (`G` in review stages them; it never commits).
  - **Project topics:** after each consolidation, what a session learned about the
    project is folded into `~/.pai/memory/projects/<slug>/topics/`. Its index goes
    into the prompt of later sessions. Contradictions are kept side by side and
    shown to you as proposals.
- **Learning loop.**
  - **When it runs:** after a consolidation, and when you leave a session (`/new`,
    `/resume`). Sessions that end any other way are caught up the next time you start
    one in the project.
  - **What it does:** a promoter reads what the session learned, then **proposes** new
    skills, skill fixes, and memory entries. Each proposal has a reason and evidence.
  - **Reviewing:** nothing takes effect until you accept it in `/memory-review`.
    `RET` shows the details and diff, `a` accepts, `r` rejects (your reason is shown
    to the promoter next time), `e` edits and accepts, `A` accepts all of one kind.
  - **What becomes a skill:** only a way of working **you taught or steered**: you
    corrected the assistant's approach, or explained how a kind of task should be
    done (observers record these as `correction:` / `taught:`). The skill captures
    that method for future, different tasks. What the session built or fixed, or a
    single request you made, is never a skill: without a steering record the
    promoter is not allowed to create one (patching existing skills and memory
    entries still work). `/learn` is the explicit exception.
  - **Or found by trial and error:** a method the assistant only found after other
    approaches failed (`discovered:` observations). The struggle is measured, not
    taken on trust: the stretch of conversation must contain at least
    `:discovery-min-failures` (default 3, `0` turns it off) failed tool calls. Dead
    ends become pitfalls with their cause and fix.
  - **What becomes a prompt snippet:** a short instruction **you keep attaching to
    your messages** ("keep it short", "show the plan first"; observers record these
    as `instruction:`). The promoter proposes a `snippet-create` (or a
    `snippet-patch` of an existing snippet) that you review like a skill; accepted
    snippets land in `~/.pai/snippets/` (or `.pai/snippets/` of a trusted project,
    scope `project`) and appear in `/snippets` at once. A new snippet must quote a
    listed instruction, exactly like a skill must quote its steering. Something you
    want on *every* message goes to user memory instead. Turn it off with
    `:learn-snippets` (*Learn prompt snippets* in `/menu`); `:max-snippet-chars`
    (default 1200) caps the size. Needs the prompt-snippets extension.
  - **Built for cheap models:** the memory workers can run on a smaller model
    (`/scoped-models`), so decisions are enforced in code rather than trusted to
    the model: the steering/discovery gate, the failure count, a new skill's
    evidence must quote a listed observation, a patch needs a fresh read, and the
    lint feeds back into the run. The promoter's prompt is a short step-by-step
    procedure.
  - **Where a lesson goes** (after Hermes' background review): first a skill used in
    that session, then another existing skill covering the same class of task, and
    only then a new, class-level skill (named for the kind of task, never for
    today's feature or bug). A patch is refused unless the promoter read the
    skill's current text in the same run.
  - **How a skill is written:** When to use (with "Not for:"), numbered Steps each
    ending in a completion check, Pitfalls (a rule plus one clause of why),
    Verification. No ticket numbers, dates or session narrative; one lesson is
    one rule, fixed in place; a preference lives either in the skill for that task
    or in user memory, never both. Environment failures, "tool X is broken"
    claims, transient errors and unresolved attempts are never captured.
    `/memory lint` and the review flag one-task names and history narration.
  - Proposals that contain shell commands, credentials and the like are flagged.
  - Learned skills go to `~/.pai/skills/learned/`, or to `.pai/skills/learned/` in
    trusted projects.
  - `/learn [what]` captures a skill from the current session on demand.
- **Skill curation.**
  - pai counts how often each skill is read and used, and whether observers saw it
    followed, changed, or failed. This is stored in
    `~/.pai/memory/skills-usage.json`, never in the skill itself.
  - A learned skill that keeps failing is handed back to the promoter to fix.
  - Once a week, a curator with no model calls marks a learned skill stale after
    20 sessions without using it, and archives it after 60. Restore one with
    `/memory-restore-skill`.
  - Unused time is counted in sessions, so time away from pai never ages a skill.
    Project skills count only their project's sessions. A minimum of 14 and 45 days
    also applies, so a burst of short sessions can't do it either.
  - `/memory-pin` exempts a skill. Hand-written skills are never touched.
- **Search (V2).** The agent's `memory_search` tool, and `/memory search`, search
  everything pai remembers without any model call: past conversations of the
  project (or all projects), observations, compaction summaries, topic files,
  long-term memory and skills. The index is `~/.pai/memory/index.sqlite`, uses
  Emacs's built-in SQLite, and updates while Emacs is idle.
- **Automatic recall (V2, opt-in).** `/menu` → Memory → Search → Automatic recall.
  Each new prompt is searched against earlier sessions, and the top few notes are
  added to what the model receives, not to your transcript. No model call is made.
- **Semantic search (V2, opt-in).** Set `/menu` → Memory → Search → *Embedder* to
  `openai` and give an OpenAI-compatible embeddings URL; a local server works. Notes
  are then embedded in the background (up to a daily token cap), and
  `memory_search` also finds notes that share no words with the query. Leave it
  blank to keep plain full-text search.

Observers use the `memory-observer` model role, which falls back to the task model
and then the main model. Point it at a cheap model in `/menu` → Model & Reasoning →
Scoped models, or with `/scoped-models memory-observer <id>`. To opt out, use `/memory session off` for one
session, or set the preset to `off` in `/menu` → Memory.

| Command | Effect |
|---|---|
| `/memory` | Layers, preset, observation pool, workers, spend, budget |
| `/memory observe` | Observe everything waiting now |
| `/memory consolidate` | File all stored observations into topics now |
| `/memory compact` | Compact now (from observations when possible) |
| `/memory on`, `/memory off` | Switch memory on or off everywhere (global settings; clears this project's and session's own on/off); `off` also stops every worker |
| `/memory stop [ID\|all]` | Stop running workers; without an ID, also stop memory for this session (widget `🧠 ⏹ stopped`) |
| `/memory session on\|off`, `learning on\|off`, `preset NAME` | Per-session overrides; add `--global` to `session`/`learning` to switch that layer everywhere |
| `/memory start` | Resume memory stopped with `/memory stop` |
| `/memory resume` | Lift a budget pause (or a stop) for this session |
| `/memory show` | Long-term memory as saved now |
| `/memory undo [ID]` | Undo the newest (or a given) memory or skill change |
| `/memory-review` (or `/memory review`) | Review pending proposals |
| `/memory-browse` | Browse memory: entries, topics, observations, skills, proposals; edit, remove, see sources, search |
| `/learn [what] [--from URL\|BUFFER\|FILE\|DIR ...]` | Capture a reusable skill from this session, or from docs, code or pages you name |
| `/memory promote` | Run the promoter over this session now |
| `/memory search WORDS` | Full-text search of past sessions, observations, topics, memory and skills |
| `/memory reindex` | Rebuild the search index |
| `/memory private [on\|off]` | Keep this session out of memory: no observers, learning, recall or search index |
| `/memory forget TEXT [--regex] [--all] [--dry-run]` | Remove something from memory files, observations and the search index (asks first; `/memory undo` restores files) |
| `/skills-export NAME...\|--learned\|--all DEST` | Copy skills to a directory or `.tar.gz` |
| `/skills-import DIR\|FILE.tar.gz\|GIT-URL` | Import skills as proposals to review (updates too) |
| `/memory why QUOTE\|SKILL` | Where a memory entry or skill came from: changes, proposal, observations, session |
| `/memory reflect` | Condense a long session's observations into patterns (opt-in: *Reflections* in `/menu`) |
| `t` in `/memory-review` | Test-run a proposed skill in a scratch copy of the project before accepting it (or turn on *Test new skills automatically* in `/menu`: only proposals with no security findings) |
| `/memory timeline [DAYS]`, `/memory graph` | What was learned when, and how it connects (Org buffer; Graphviz if installed) |
| `/memory merge-topics` | Fold this session's topics into the project's topic tree |
| `/memory-pin NAME\|QUOTE` | Pin a skill (never archived) or a memory entry (always in the prompt) |
| `/memory merge` | Look for overlapping learned skills and propose merging them (one model call, only when some overlap) |
| `/memory lint [SKILL]` | Check skills for style problems and dangerous content |
| `/memory insights [DAYS]` | Background spend by role, project and day, how compaction and learning went, and suggested settings |
| `/memory skills` | Usage, outcomes and state of every skill |
| `/memory curate` | Run the curator now |
| `/memory-pin`, `/memory-unpin`, `/memory-restore-skill NAME` | Curator controls |

## Asking the user

The `pai-ask-user/` extension ports
[`ask-user-question`](https://github.com/amosblomqvist/pi-config/blob/main/extensions/ask-user-question.ts)
from pi-config. It registers one tool the agent can call when it should stop
guessing:

```
ask_user_question {question, details?, options?, multiSelect?}
```

The arguments choose the mode, exactly as upstream: no `options` means a
free-form answer, `options` means pick one, and `options` plus `multiSelect`
means pick several. The model gets back the same result text upstream sends
(`User selected: 2. Patch it`) plus a structured `details` payload (`status`,
`mode`, `answers`, ...) that is persisted in the session log.

Where pi draws a TUI overlay, pai uses Emacs: select modes open a dialog
buffer rendered with `vui`, and free-form answers -- including the "Other"
answer that is always offered alongside options -- are composed in an ordinary
text buffer, so they can be multi-line and edited with your normal keys.

| Key | In a question dialog |
|-----|----------------------|
| `1`-`9` | choose (or toggle) that option |
| `TAB` / `S-TAB`, `RET` | move between choices, activate one |
| `o` | write a custom answer in an editor buffer |
| `C-c C-c` | submit (multi-select; also submits an answer buffer) |
| `C-c C-k` | cancel the question (or back out of an answer buffer) |

Answering leaves a note in the transcript, so a decision is findable weeks
later without hunting for the tool result that carried it:

```
❓ How should we fix it?
   → 2. Patch it
```

Ask the same question twice in a session and it comes up on your previous
answer: multi-select boxes are pre-ticked, a free-form editor opens with the
old text to edit, and a single-select marks last time's choice and puts point
on it, so RET repeats the decision. The memory is keyed by the question *and*
its options, lives in the session buffer only, and is never persisted.

The tool call is asynchronous: the turn is held open -- nothing is sent to the
model, the run simply waits -- until you answer. An unanswered question can
never wedge a session: killing its buffer cancels it, `pai-interrupt` cancels
it, ending the session cancels it, and an optional timeout (off by default)
cancels it. All four behaviours are toggles in *Ask user* in `/menu`
(`:ask-user` project settings). Every
outcome is reported to the model as a `cancelled` result, so the agent learns
it did not get an answer instead of stalling. `/ask` redisplays a question you
navigated away from. In a headless session (`pai-oneshot`, batch) the tool
returns `unavailable` rather than blocking.

## Dashboard

New conversations open with a dashboard, after the Spacemacs home buffer: a
logo drawn with the built-in `svg.el` in the current theme's colours (a text
logo in terminals), the model and project, and what is at hand:

- **Skills**, with their descriptions: `RET` puts `/skill:NAME` in the prompt.
- **Extensions**, described by the `;;; NAME --- SUMMARY` line of their main file
  (read with `lm-summary`): `RET` opens the file.
- **Prompt snippets**, when that extension is loaded: `RET` puts `/snippets` in
  the prompt.

It appears for a new buffer and after `/new`, not when a session is resumed,
and never in subagent sessions. It is display only (the model never sees it).
`/dashboard` shows it again; `pai-dashboard-enable` turns it off.

## Learning

The `pai-learn/` extension ports [`learn`](https://github.com/amosblomqvist/learn),
an AI learning system: a teaching skill (probe what you know with graded
questions, plan a dependency map from unconditional truths, then teach it node
by node, each node motivated, connected and quiz-checked), a graded `quiz`
tool, verified diagrams, and a readable lesson log. It is used rarely, so
**none of it is in every context**: loading it only adds slash commands.

| Command | Effect |
|---------|--------|
| `/teach [topic]` | turn this session into a teaching session and start |
| `/teach-log FILE` | mirror the lesson into an Org file (backfilled, then live) |
| `/teach-unlog` | stop mirroring |

`/teach` registers the `quiz` tool in *this* session only, gives its
`subagent` tool three extra roles (`learn-researcher` for fact checks with
sources, `mermaid-maker` and `svg-maker` for diagrams), and sends the teach
skill. A resumed or reloaded session that was taught before is re-armed from
its history. Maker subagents alone get the `render_diagram` tool. The skills
and roles live in `pai-learn/{skills,agents}/`, outside the skill
directories, so they never show up in other sessions.

What changed from upstream:

- **Questions** use pai's own `ask_user_question`; upstream's bundled copy is
  not ported.
- **`quiz`** is a `vui` dialog built from the ask-user dialog pieces: `1`-`9`
  answer/toggle, `?` I don't know, `n` attach a note, `C-c C-c` submit or
  continue, `C-c C-k` cancel. Options are shuffled; after answering you see
  ✓/✗, the key and the explanation until you continue.
- **The lesson log** is Org instead of Obsidian Markdown: replies are converted
  to Org, questions and quiz verdicts are logged once answered, Markdown image
  embeds become inline images, ```` ```mermaid ```` blocks are rendered to
  images next to the log (`viz/`), and math stays LaTeX (`org-pretty-entities`
  is on in the log; `C-c C-x C-l` previews when LaTeX is installed).
- **Diagrams render inside Emacs**: one hidden xwidget-webkit page runs
  mermaid.js and rasterises SVG to PNG on a canvas -- no Node, Chrome, rsvg or
  ImageMagick. mermaid.js is downloaded once (`url-copy-file`) to
  `~/.pai/learn/`. Needs a graphical Emacs built with xwidgets.

## Shaking context

The `pai-shake/` extension ports oh-my-pi's `/shake`: context reduction that is
*mechanical* rather than summarized. It makes no model call, so it is instant
and never paraphrases your conversation — where `/compact` rewrites history
into a summary, `/shake` drops the bulk and leaves everything else byte-identical.

| Mode | Effect |
|------|--------|
| `/shake` (= `/shake elide`) | Replace whole tool results and large fenced/XML blocks with a short placeholder |
| `/shake images` | Strip image blocks (image-only messages keep an `[image removed]` marker) |
| `/shake thinking` | Drop assistant reasoning blocks |
| `/shake all` | All of the above in one pass (the protected recent tail is still kept) |

Before eliding, the originals are written to a recovery artifact under
`~/.pai/artifacts/`, and every placeholder names that file and its region
number, so the agent can `read` back anything it turns out to still need (those
recovery reads are themselves protected from a later shake).

Safety rails, mirroring upstream: the newest 4k tokens of context are never
touched, so the tool results in flight survive; the system prompt and tool-call
blocks are never shaken, so tool-call/result pairing stays valid; only complete,
terminated blocks of at least 400 tokens qualify; and a shaken result is marked
so a second `/shake` will not re-elide its own placeholders. Both thresholds are
editable under *Session → Context* in `/menu` (`:shake` project settings).

Like `/compact`, `/shake` reduces the live context that is sent to the model; the
transcript on screen is a log of what happened and stays as it was.

## Todo list

The `pai-todo/` extension ports oh-my-pi's todo tool. For multi-step work the
agent keeps a **phased todo list** with the `todo` tool (`init`, `start`, `done`,
`drop`, `block`, `unblock`, `rm`, `append`, `view`). Tasks are named by their
text, exactly one is in progress, and a failed call changes nothing.

- **Status bar:** `☑ 2/5 · Current task` (`⛔N` when tasks are blocked); hover
  for the whole list.
- **Kept in the session:** the latest todo result or `/todo` edit on the current
  branch is the list, so `/resume`, `/tree` and forks bring back the right one.
- **Reminders:** when the agent stops with open tasks it gets a reminder listing
  them, at most twice per prompt of yours; not after an interrupt, not when it
  ends with a question for you, not while it ignored the last one. Blocked tasks
  don't count. `/todo reminders on|off`, or *Todo* in `/menu`.

| Command | Does |
|---|---|
| `/todo` | Show the list |
| `/todo done\|drop\|block\|unblock\|rm TASK-or-PHASE` | Change a task or a whole phase (fuzzy; `done`/`drop`/`block` alone: the task in progress) |
| `/todo start TASK` | Make a task the one in progress |
| `/todo append [PHASE:] TASK` | Add a task (default: the current task's phase) |
| `/todo edit` | Edit the list as an Org outline (`C-c C-c` saves) |
| `/todo export [FILE]`, `/todo import [FILE]` | Write or read the Org outline (default `TODO.org`) |
| `/todo clear` | Remove everything |

Task and phase names complete, spaces and all. Your changes are recorded in the
session like the agent's.

## Prompt snippets

The `pai-prompt-snippets/` extension ports
[`prompt-snippets`](https://github.com/amosblomqvist/pi-config/tree/main/extensions/prompt-snippets):
mix-and-match single-purpose prompt rules that are prepended or appended to your
message when you send it. Unlike skills, each snippet is a tiny, standalone
instruction — toggle exactly the ones you want per message.

Press `C-c s` (or run `/snippets`) to open the toggle menu, a `vui` buffer that
groups snippets into a **prepend** and an **append** section:

- `TAB`/`S-TAB` move, `SPC`/`RET` toggle, `p` previews the snippet at point in
  its own buffer, `C-c C-c` applies, `C-c C-k`/`q` cancels.
- Active snippets show up as a mode-line widget — `↑ prepend: …` (accent) and
  `↓ append: …` (warning).
- On send, the active bodies are merged into the message: prepend group (by
  `order`) → your text → append group (by `order`), separated by blank lines.
- Toggles reset to **all off** after each send and at session start.

Snippets are markdown files with frontmatter (`name`, `description`,
`placement`, `order`), living in `snippets/` next to the extension, your own
`~/.pai/snippets/`, `.pai/snippets/` under the project and anything in
`pai-prompt-snippets-directories` (later directories win on filename
collisions). Files are re-scanned every time the menu opens and every time a
message is sent, so edits take effect immediately — no `/reload` needed. Extra
snippet directories are editable under *Prompt snippets → Sources* in `/menu`.

Snippets can also be **learned**: when you keep adding the same kind of
instruction to your messages, the memory promoter proposes it as a snippet for
`/memory-review` (see *Learning memory*).

## MCP extension

`pai-mcp/` provides the `mcp` proxy tool and `/mcp` command.
Configure servers in a project `.mcp.json` under `mcpServers`; use `command`
and `args` for stdio, or `url` for HTTP. Connections start lazily when used.
HTTP requests and header helpers run asynchronously without blocking Emacs.

HTTP supports JSON and SSE responses, session headers, and legacy SSE
fallback on HTTP 404/405/406/415 during initialization. Set
`httpTransport: "sse"` to select legacy SSE explicitly. Both its discovery
GET and subsequent POST requests use the request signer when configured.

- `headers`: static values with environment interpolation. A value starting
  with `!` runs a shell helper at connection time; `!!` escapes a literal `!`.
  Failed or empty helper output aborts connection without sending requests.
- `requestHeadersCommand`: a shell command string, or an object with `command`,
  `args`, and `timeoutMs`. Receives JSON on stdin containing `version: 1`,
  `method`, `url`, and `bodyBase64` (the exact UTF-8 request body). Must return
  a JSON object of string-valued headers; `{}` is valid. Returned headers
  override other headers case-insensitively. Malformed output fails the request.
- `caFile`: readable certificate bundle for an HTTPS endpoint.

The MCP port is still in progress; modern protocol discovery/negotiation and
the remaining authentication, resources, prompts, and interaction features
are not yet complete. The HTTP transport currently uses the legacy initialize
handshake; `protocolVersion` does not yet implement modern discovery.

## Browser (xwidget-webkit)

`xwidget-browser/` gives the agent a **real browser**: the `browser`
tool drives Emacs' built-in WebKit widget, so pages that need JavaScript,
cookies or a logged-in session work exactly as they do for you.

| Group | Actions |
|-------|---------|
| Sessions | `sessions`, `attach`, `open`, `close`, `show`, `hide`, `info` |
| Navigation | `back`, `forward`, `reload`, `stop`, `wait` |
| Reading | `text`, `html`, `links`, `elements`, `console` |
| Acting | `click`, `fill`, `select`, `key`, `scroll`, `js` |
| Seeing | `screenshot` |

- **Shared sessions.** A session is just an `xwidget-webkit` buffer, so the
  browser *you* opened with `M-x xwidget-webkit-browse-url` is a session the
  agent can use. With no `session` argument the tool targets the session most
  recently used; opening a browser yourself always takes over, and `attach`
  (or `M-x xwidget-browser-attach-session`) pins one explicitly. Each result
  names its session (`[s2]`), which can be passed back as `session`.
- **Head-less by default.** Sessions the agent opens load pages, run scripts,
  fire timers and render without ever being displayed; `show` pops one into a
  window when you want to watch (`hide` puts it away again). Agent-created
  sessions are killed when the pai session ends; yours are left alone.
- **Screenshots as images.** `screenshot` returns a real image block, so any
  image-capable model can *look* at the page; `save_path` also writes a PNG/JPEG
  file. Emacs has no native webkit snapshot primitive (and a pgtk frame cannot
  be grabbed with X tools), so pixels are rendered in-page with
  [html2canvas](https://html2canvas.hertzen.com/), downloaded once into
  `~/.pai/xwidget-browser/`. Captures are viewport-sized unless `full_page` is
  set, and are downscaled to `max_width` (default 1400) to keep tokens sane.
- **JavaScript.** `js` evaluates an expression or a `return`-using body in the
  page. Promises are awaited, results come back as JSON, and large payloads are
  streamed in chunks — none of which `xwidget-webkit-execute-script` does on its
  own. `console` reports `console.*` output and errors captured since the
  document was committed.
- **Interactive helpers**: `M-x xwidget-browser-open`,
  `M-x xwidget-browser-attach-session`, `M-x xwidget-browser-capture`, and the
  `/browser` command (`/browser` lists, `/browser URL` opens, `/browser attach
  [id]` pins, `/browser close` kills the agent's sessions).

Viewport size, screenshot width, console capture and the tool itself are
configurable under *Browser* in `/menu` (`:browser` settings). Requires an
Emacs built `--with-xwidgets` running on a graphical display.

## Development

Each extension keeps its tests in its own `test/` subdirectory
(`pai-memory/test/`, `pai-mcp/test/` with its fixtures, ...). The loader only
looks at an extension's top-level `.el` files, so tests are never loaded as
extension code. They run from the pai checkout this repository is cloned into:

```sh
cd pai                   # with this repository cloned as extensions/
make test                # core + extension tests
make test-extensions     # extension tests only
make compile             # byte-compile core and extensions
```

The tests reuse the core's test helpers (`test/pai-faux.el` and friends), so
they need a pai checkout; they make no network calls and need no API keys.

## License

MIT, like pai itself.
