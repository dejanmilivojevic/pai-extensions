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
| `pai-harness2-batch.el` | One headless rollout: a real pai chat in a child Emacs, recorded as a JSONL event stream |
| `pai-harness2-hooks.el` | `.pai/hooks.json`: Claude-Code-schema lifecycle hooks dispatched inside pai's tool loop |
| `pai-harness2.el` | `/harness2` command (install, setup, check, demo, run, judge, report, …) |
| `python/harness2_pai/` | `PaiSubstrate`, the `pai` improver channel, the JobBench, LAB and WorkBuddy adapters, the `harness2-pai` CLI |
| `python/harness2_pai/wb_runner/` | The WorkBuddy Harbor agent and harness config `harness2-pai setup wb` installs |

Everything is driven from Emacs. `/harness2` starts each step as an asynchronous
process in a compilation buffer. When Harness² needs a rollout it starts a child
Emacs with `emacs -Q --batch -L … -l pai-harness2-batch`. That command line comes
from your running Emacs (`pai-harness2-batch-command`, the same load path) and
reaches Harness² through `PAI_HARNESS2_EMACS_ARGV`. No wrapper script and no
copy of Emacs is involved, so your Emacs never runs a rollout itself and never
freezes.

## Benchmarks: parity with the OpenCode and Codex cells

| | JobBench (`jb`) | LAB (`lab`) | WorkBuddy-Bench (`wb`) |
|---|---|---|---|
| Rollout | child Emacs in a private workspace (task folder + output/) | child Emacs in a scratch workspace (read-only documents/, output/, `scripts/read_doc.py` installed last) | Harbor starts the task container; pai runs **on the host** and works **inside the container over TRAMP** (`/docker:USER@CONTAINER:/workspace/`) |
| Model | `HARNESS2_PAI_MODEL` (any pai model) | same | the bench proxy's solver route (as for OpenCode, logged by the proxy), or `HARNESS2_PAI_WB_MODEL` |
| Records | `model_traj/<label>/*.jsonl`, `attempt_index.tsv` | `config.json`, `pai_session.jsonl`, `metrics.json`, `output/` | `agent/pai-events.jsonl`, ATIF `trajectory.json` |
| Judge / score | upstream | upstream | the verifier's `score.json` (upstream) |
| Base harness | the Codex cell's AGENTS.md | the Codex cell's AGENTS.md (output/ and read_doc.py rules enforced) | the Codex cell's AGENTS.md |

On WorkBuddy no Emacs is installed into task images and nothing is mounted. pai's
bash, grep, file tools and hooks run in the container, under its egress control,
through TRAMP's built-in `docker` method. pai itself makes only the model call,
from the host. This relies on two core pai changes: tools follow a remote
working directory, and helper processes such as curl stay local.

The WorkBuddy setup still requires what upstream requires: Docker, and a
`google-vertex/gemini…` solver route and a `gemini…` judge in the WorkBuddy
model configs. Set `HARNESS2_SOLVER_MODEL` explicitly for `setup wb`.

Rollouts are refused network access to the hosts that publish the benchmarks'
data and rubrics (`huggingface.co`, `hf.co`, `github.com`,
`raw.githubusercontent.com`; see upstream docs/EXPERIMENTS.md, "Web access").
Any tool call whose arguments name one of them is blocked. Upstream only
recommends this for the other cells. Override the list with
`HARNESS2_PAI_DENY_HOSTS` (`none` disables it), and remove tools with
`HARNESS2_PAI_DENY_TOOLS`.

### How the harness maps onto pai

| Harness² component | pai file in the rollout workspace |
|---|---|
| c1–c4 system prompt, guardrails, memory, tool guidance | `AGENTS.md` (pai's `<project_context>`, always on, cap 16k chars) |
| c5 subagents `sub_agents/<name>/AGENT.md` | `.pai/subagents/<name>.md` roles (headless `pai-subagents`; allowed wherever upstream allows them for OpenCode) |
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
/harness2 install                    # clone Harness² to pai-harness2-directory, uv venv, install both packages
/harness2 demo                       # the ledger demo, every rollout a real pai session (scripted editor)
/harness2 demo --improver pai        # … and pai edits the harness too
/harness2 setup jb                   # or: setup lab, setup wb --subset office
/harness2 check --bench lab          # preflight a pai cell (no model calls)
/harness2 run --bench jb --k 1 --mode parallel --max-tasks 1
/harness2 run --bench wb --subset office --k 1 --max-tasks 1
/harness2 judge --bench jb --k 1 --mode parallel
/harness2 report --bench jb --k 1 --mode parallel
/harness2 runs                       # dired the runs directory
/harness2 render FILE                # a trajectory as the improver sees it
/harness2 status
```

Rollouts use the chat's current model unless you set `pai-harness2-model`.
The other settings are `pai-harness2-improver-model`,
`pai-harness2-improver-channel` (`pai`, `opencode` or `claude_cli`),
`pai-harness2-thinking` and `pai-harness2-environment` (extra `NAME=value`
entries: the judge settings such as `HARNESS2_JUDGE_MODEL` and
`GOOGLE_CLOUD_PROJECT`, `HARNESS2_PAI_WB_MODEL`, `HARNESS2_PAI_DENY_HOSTS`,
`HARNESS2_RUNS_DIR` …). You can also put the judge settings in the checkout's
`.env`.

Environment read by the Python side: `HARNESS2_PAI_MODEL` (falls back to
`HARNESS2_SOLVER_MODEL`), `HARNESS2_PAI_IMPROVER_MODEL`,
`HARNESS2_IMPROVER_CHANNEL`, `HARNESS2_PAI_THINKING`, `HARNESS2_PAI_TIMEOUT`,
`HARNESS2_PAI_ARGS` (extra rollout flags), `HARNESS2_PAI_WORKSPACES`,
`HARNESS2_PAI_LAB_SCRATCH`, `HARNESS2_PAI_WB_MODEL`, `HARNESS2_PAI_DENY_HOSTS`,
`HARNESS2_PAI_DENY_TOOLS`, `PAI_HARNESS2_EMACS_ARGV` (set by `/harness2`), and
`PAI_ROOT`/`PAI_EMACS` (its fallback).

## Scope and limits

- **Integration method:** `harness2_pai.register.install()` extends Harness²'s
  substrate, install-mode, channel, prompt and WorkBuddy tables at runtime.
  `harness2-pai setup wb` adds one `pai-harness` entry to WorkBuddy's runtime
  adapter table. Every seam is checked, so an upstream refactor fails loudly.
- **WorkBuddy** needs Docker and is tested only with a fake `docker` (TRAMP's
  docker method against the local machine) and a fake Harbor environment. A
  real container run still has to happen where Docker is available.
- **Cost:** a rollout's `cost_usd` is pai's priced cost. Models pai has no price
  for report 0.

## Tests

```bash
make test                                   # ERT, includes test/pai-harness2-test.el
cd <harness2 checkout> && .venv/bin/python -m pytest ~/.pai/extensions/pai-harness2/python/tests
cd <harness2 checkout>/dependencies/workbuddy-bench && \
  .venv/bin/python -m pytest --noconftest ~/.pai/extensions/pai-harness2/python/tests/test_wb_agent.py
```

Both suites run real batch-Emacs rollouts against pai's faux provider. Neither
makes a network call.
