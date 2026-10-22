# pai-harness2 — Harness² with pai as the agent

[Harness²](https://github.com/google-research/harness2) (google-research,
"Recursive Agent Harnessing for an Open World") improves an agent's
**harness**: the files around the model (system prompt, guardrails, memory,
tool guidance, subagents, skills, scripts, hooks). For each task it
*proposes* harness edits, *probes* them with real rollouts of the same task,
*composes* a refined harness from the behavioural contrasts (no ground truth
needed), and stores the lessons in an experience library used for the next
task.

Upstream drives OpenCode or Codex. This extension adds **pai** as a third
substrate (`--substrate pai`), and as the improver (`pai` channel), so a
complete run needs nothing but Emacs, pai and Harness²'s Python package.
It does not need OpenCode, bun or Codex.

## What is in here

| Path | Role |
|---|---|
| `pai-harness2-batch.el` | Headless rollout: a real pai chat in batch Emacs, recorded as a JSONL event stream |
| `pai-harness2-hooks.el` | `.pai/hooks.json`: Claude-Code-schema lifecycle hooks dispatched inside pai's tool loop |
| `bin/pai-harness2-run` | Launcher for one rollout (`--cwd --prompt-file --model --events --timeout …`) |
| `pai-harness2.el` | `/harness2` command (install, setup, check, demo, run, judge, report, …) |
| `python/harness2_pai/` | `PaiSubstrate`, the `pai` improver channel, the JobBench launcher, the `harness2-pai` CLI |

### How the harness maps onto pai

| Harness² component | pai file in the rollout workspace |
|---|---|
| c1–c4 system prompt, guardrails, memory, tool guidance | `AGENTS.md` (pai's `<project_context>`, always on, cap 16k chars) |
| c5 subagents `sub_agents/<name>/AGENT.md` | `.pai/subagents/<name>.md` roles (headless `pai-subagents`) |
| c6 skills | `.pai/skills/<name>/SKILL.md` (+ `scripts/`) |
| c7 scripts | `scripts/<name>.py` |
| c8 hooks | `.pai/hooks.json` — `PreToolUse`, `PostToolUse`, `SessionStart`, `UserPromptSubmit`, `Stop` |

Hooks follow the Claude-Code schema that Harness² already validates for Codex.
A hook blocks by exiting 2 or by printing `{"decision":"block","reason":…}`.
On `PreToolUse` a block denies the call, on `PostToolUse` the reason is
appended to the tool result, and on `Stop` the reason comes back as a new
user turn (at most 3 times). Matchers compare pai's tool names (`bash`,
`edit`, `write` …) case-insensitively, so `"Bash"` and `"Edit|Write"` work.
Hooks run synchronously, so they are only installed by the batch runner and
never in an interactive Emacs.

Skill and subagent names must be valid pai names (lowercase words joined by
single hyphens). Otherwise pai would silently drop them, so the adapter
rejects such trees and Harness² retries the proposal.

### Rollout isolation

By default every rollout gets a throw-away pai directory with copies of
`settings.json`, `auth.json` and `mcp.json` and a link to `extensions/`.
Rollouts reach the same providers but never write sessions, history, trust
decisions or memory into `~/.pai`. Learning extensions (`pai-memory`) and
UI-only ones are switched off there: Harness² probes must differ only by
their harness. `--no-isolate`, `--enable-ext`, `--disable-ext` and
`--inherit-skills` change that. The workspace is trusted for the run only.

The event stream (`session`, `message`, `tool_start`, `tool_result`, `hook`,
`agent_end`, `done`) is rendered into Harness²'s trajectory grammar
(`[USER] [ASSISTANT] [REASONING] [ACTION] [OBSERVATION] [HARNESS] [ERROR]`)
by `harness2_pai.substrate.render_events`. Exit status: 0 done, 1 failed,
2 bad invocation, 124 timeout.

## Usage

In a pai chat (everything runs asynchronously in `*harness2: …*` buffers):

```
/harness2 install          # clone Harness² to pai-harness2-directory, uv venv, install both packages
/harness2 demo             # the ledger demo, every rollout a real pai session (scripted editor)
/harness2 demo --improver pai   # … and pai edits the harness too
/harness2 setup jb         # download JobBench (upstream harness2-setup)
/harness2 check            # preflight the pai cell (no model calls)
/harness2 run --bench jb --k 1 --mode parallel --max-tasks 1
/harness2 judge --bench jb --k 1 --mode parallel
/harness2 report --bench jb --k 1 --mode parallel
/harness2 runs             # dired the runs directory
/harness2 render FILE      # a trajectory as the improver sees it
/harness2 status
```

Rollouts use the chat's current model unless you set `pai-harness2-model`.
The other settings are `pai-harness2-improver-model`,
`pai-harness2-improver-channel` (`pai`, `opencode` or `claude_cli`),
`pai-harness2-thinking` and `pai-harness2-environment`. Put judge settings
(e.g. `HARNESS2_JUDGE_MODEL`, `GOOGLE_CLOUD_PROJECT`) in
`pai-harness2-environment` or in the checkout's `.env`.

From a shell, inside the Harness² venv:

```bash
uv pip install -e ~/.pai/extensions/pai-harness2/python
HARNESS2_PAI_MODEL=anthropic/claude-sonnet-4-6 harness2-pai demo
HARNESS2_PAI_MODEL=anthropic/claude-sonnet-4-6 harness2-pai run --bench jb --k 1 --max-tasks 1
```

Environment: `HARNESS2_PAI_MODEL` (falls back to `HARNESS2_SOLVER_MODEL`),
`HARNESS2_PAI_IMPROVER_MODEL`, `HARNESS2_IMPROVER_CHANNEL`,
`HARNESS2_PAI_THINKING`, `HARNESS2_PAI_TIMEOUT`, `HARNESS2_PAI_ARGS` (extra
runner flags), `HARNESS2_PAI_WORKSPACES`, `PAI_HARNESS2_RUNNER`, `PAI_EMACS`,
`PAI_ROOT`.

## Scope and limits

- **Benchmarks:** JobBench only for now. LAB and WorkBuddy-Bench need their
  own launchers; WorkBuddy also needs Emacs and pai inside its Docker image.
- **Judging** is unchanged upstream code. JobBench's judge still needs its own
  model endpoint (Vertex by default).
- **Integration method:** `harness2_pai.register.install()` extends Harness²'s
  substrate, install-mode, channel and prompt tables at runtime. It checks that
  every seam exists and fails loudly if an upstream refactor moved one.
- **Cost:** a rollout's `cost_usd` is pai's priced cost. Models pai has no
  price for report 0.

## Tests

```bash
make test                                   # ERT, includes test/pai-harness2-test.el
cd <harness2 checkout> && .venv/bin/python -m pytest ~/.pai/extensions/pai-harness2/python/tests
```

Both suites run real batch-Emacs rollouts against pai's faux provider. Neither
makes a network call.
