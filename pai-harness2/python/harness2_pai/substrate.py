"""PaiSubstrate: the pai Emacs agent as a Harness² solver substrate.

The editable c1..c8 tree maps onto what a pai chat reads from its project directory:

    AGENTS.md                         c1-c4  always on (pai's <project_context>)
    .pai/skills/<name>/SKILL.md       c6     skills (name+description advertised)
    .pai/skills/<name>/scripts/*             helpers carried with a skill
    scripts/<name>.py                 c7     deterministic helpers
    sub_agents/<name>/AGENT.md        c5     installed as .pai/subagents/<name>.md roles
    .pai/hooks.json                   c8     Claude-Code schema lifecycle hooks, dispatched
                                             by the headless runner inside the tool loop

One install mode, ``pai_root``: the staged tree is copied to the workspace root, where
``bin/pai-harness2-run --cwd <ws>`` (a batch Emacs running a real pai chat) finds it.
The rollout writes a JSONL event stream; `render_events` turns it into the shared
trajectory grammar ([USER] [ASSISTANT] [REASONING] [ACTION] [OBSERVATION] [HARNESS]).
"""

from __future__ import annotations

import json
import os
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Mapping

from harness2.core import runmark as RM
from harness2.core import tree as HT
from harness2.core.tree import SurfaceSpec
from harness2.substrate.base import Install, Launcher, RunRequest

from . import settings

INSTALL_MODE = "pai_root"

HOOK_EVENTS = frozenset({"PreToolUse", "PostToolUse", "SessionStart", "UserPromptSubmit", "Stop"})

PAI_SURFACE = SurfaceSpec(
    name="pai",
    always_on=("AGENTS.md",),
    always_on_max=16_000,
    always_on_cap_reason=(
        "AGENTS.md is injected whole into pai's system prompt on EVERY model request"
    ),
    skill_dir=".pai/skills",
    skill_name_rule="overrides_dir",
    trigger_hint=(
        'Fix it by adding a trigger line naming `{name}` to AGENTS.md, e.g. "When '
        "<the situation this skill handles>, read .pai/skills/{name}/SKILL.md "
        'and follow it."'
    ),
    autotrigger_file="AGENTS.md",
    trigger_line="- When {desc}, read .pai/skills/{name}/SKILL.md and follow it.\n",
    subagent_requires_mode=False,
    plugin_dir=None,
    hooks_file=".pai/hooks.json",
    hook_events=HOOK_EVENTS,
    compile_skill_scripts=True,
    reject_empty_always_on=True,
    stubs=(
        (
            "scripts/README.md",
            "<!-- c7: deterministic helpers, run as `python3 scripts/<name>.py`. "
            "Add one by emitting scripts/<name>.py. -->\n",
        ),
        (
            "sub_agents/README.md",
            "<!-- c5: delegated pai subagent roles, each run in its OWN context through the "
            "`subagent` tool. Add one as sub_agents/<name>/AGENT.md with frontmatter "
            "`description:`; the runner installs it as the pai role <name>. -->\n",
        ),
        (
            ".pai/README.md",
            "<!-- c8: lifecycle hooks -- the only lever that acts INSIDE the tool loop. Add ONE "
            "file, .pai/hooks.json, Claude-Code hook schema: {\"hooks\": {\"PostToolUse\": "
            '[{"matcher": "bash", "hooks": [{"type": "command", "command": "..."}]}]}}. '
            'A command printing {"decision":"block","reason":"..."} (or exiting 2) feeds the '
            "reason back to the model. -->\n",
        ),
        (
            ".pai/skills/README.md",
            "<!-- c6: reusable workflows. Add one as .pai/skills/<name>/SKILL.md with YAML "
            "frontmatter: `description:` is REQUIRED (no description = skill silently "
            "dropped). Names are lowercase words joined by hyphens. Only name+description "
            "are advertised; the body is read on demand. -->\n",
        ),
    ),
)

_PAI_NAME = re.compile(r"[a-z0-9]+(?:-[a-z0-9]+)*")
ROOT_ENTRIES = ("AGENTS.md", ".pai", "scripts")


def validate_pai_tree(tree: Mapping[str, str]) -> None:
    """pai-specific invariants beyond the framework rules; raise ValueError listing them."""
    errs: list[str] = []
    for kind, names in (
        ("skill", HT.skill_names(PAI_SURFACE, dict(tree))),
        ("sub-agent", HT.subagent_names(PAI_SURFACE, dict(tree))),
    ):
        for name in names:
            if not _PAI_NAME.fullmatch(name) or len(name) > 64:
                errs.append(
                    f"{kind} name {name!r} is not valid for pai (lowercase letters and digits "
                    f"joined by single hyphens, at most 64 chars) -- pai drops it SILENTLY"
                )
    if errs:
        raise ValueError("; ".join(errs))


def _role_file(name: str, agent_md: str) -> str:
    """sub_agents/<name>/AGENT.md -> a pai role file (frontmatter `name:` is required)."""
    text = agent_md.lstrip("\ufeff")
    if text.startswith("---\n"):
        end = text.find("\n---", 4)
        if end != -1:
            head, body = text[4:end], text[end + 4 :]
            if not re.search(r"^name:\s*\S", head, re.M):
                head = f"name: {name}\n{head}"
            if not body.strip():
                body = "\nYou are a delegated sub-agent. Do the task you are given.\n"
            return f"---\n{head}\n---{body}"
    return f"---\nname: {name}\ndescription: {name} sub-agent\n---\n{text}"


def install_tree(staged: Path, workspace: Path) -> None:
    """Copy a staged pai tree into WORKSPACE's root."""
    for entry in ROOT_ENTRIES:
        src = staged / entry
        if not src.exists():
            continue
        dst = workspace / entry
        if src.is_dir():
            shutil.copytree(src, dst, dirs_exist_ok=True)
        else:
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src, dst)


class PaiSubstrate:
    """pai (batch Emacs) behind the one Substrate interface."""

    name = "pai"

    def __init__(self, launcher: Launcher | None = None, *, model: str | None = None):
        self._launcher = launcher
        self.model = model if model is not None else settings.executor_model()

    def surface(self) -> SurfaceSpec:
        return PAI_SURFACE

    def materialise(self, tree_dir: Path, spec, ctx: Mapping[str, str]) -> Install:
        """Stage AGENTS.md, .pai/ (skills, hooks, subagent roles) and scripts/."""
        if spec.install_mode != INSTALL_MODE:
            raise ValueError(
                f"install_mode {spec.install_mode!r} is not the pai mode; this substrate "
                f"has exactly one: {INSTALL_MODE!r}"
            )
        tree_dir = Path(tree_dir)
        if not HT.is_tree(PAI_SURFACE, tree_dir):
            raise ValueError(f"{tree_dir} is not a pai harness tree (no AGENTS.md)")
        tree = HT.read_tree(PAI_SURFACE, tree_dir)
        staged = Path(tempfile.mkdtemp(prefix="adapt-pai-root-"))
        for rel, src in tree.items():
            if rel.startswith("sub_agents/"):
                continue
            dst = staged / rel
            dst.parent.mkdir(parents=True, exist_ok=True)
            dst.write_text(src, encoding="utf-8")
            if rel.endswith(".py") or "/scripts/" in rel:
                dst.chmod(0o755)
        for name in HT.subagent_names(PAI_SURFACE, tree):
            role = staged / ".pai" / "subagents" / f"{name}.md"
            role.parent.mkdir(parents=True, exist_ok=True)
            role.write_text(_role_file(name, tree[f"sub_agents/{name}/AGENT.md"]), "utf-8")
        for rel, value in ctx.items():
            if rel.startswith("file:"):
                dst = staged / rel[5:]
                dst.parent.mkdir(parents=True, exist_ok=True)
                dst.write_text(value, encoding="utf-8")
        return Install(
            staging_dir=staged,
            fingerprint=RM.fingerprint(tree_dir),
            extras={
                "install_mode": INSTALL_MODE,
                "model": self.model,
                **{k: v for k, v in ctx.items() if not k.startswith("file:")},
            },
        )

    def run_argv(
        self,
        workspace: Path,
        prompt_file: Path,
        events: Path,
        timeout: int,
        *,
        model: str | None = None,
        final: Path | None = None,
    ) -> list[str]:
        """The `pai-harness2-run` argv for one rollout in WORKSPACE."""
        argv = [
            str(settings.runner()),
            "--cwd",
            str(workspace),
            "--prompt-file",
            str(prompt_file),
            "--events",
            str(events),
            "--timeout",
            str(int(timeout)),
        ]
        model = model or self.model
        if model:
            argv += ["--model", model]
        thinking = settings.thinking()
        if thinking:
            argv += ["--thinking", thinking]
        if final is not None:
            argv += ["--final", str(final)]
        argv += settings.extra_args()
        return argv

    def run(
        self,
        prompt: str,
        workspace: Path,
        events: Path,
        timeout: int,
        *,
        log: Path | None = None,
        model: str | None = None,
        final: Path | None = None,
    ) -> int:
        """Run one rollout; return the runner's exit status (124 = timeout)."""
        workspace = Path(workspace)
        events = Path(events)
        events.parent.mkdir(parents=True, exist_ok=True)
        fd, prompt_path = tempfile.mkstemp(prefix="pai-prompt-", suffix=".txt")
        with os.fdopen(fd, "w", encoding="utf-8") as fh:
            fh.write(prompt)
        argv = self.run_argv(
            workspace, Path(prompt_path), events, timeout, model=model, final=final
        )
        try:
            sink = open(log, "a", encoding="utf-8") if log else subprocess.DEVNULL
            try:
                p = subprocess.run(
                    argv,
                    cwd=workspace,
                    stdin=subprocess.DEVNULL,
                    stdout=sink,
                    stderr=sink,
                    env=settings.runner_env(),
                    timeout=int(timeout) + settings.GRACE_SECONDS,
                )
                return p.returncode
            except subprocess.TimeoutExpired:
                return 124
            finally:
                if log:
                    sink.close()
        finally:
            os.unlink(prompt_path)

    def execute(
        self, task_ref: str, install: Install, run_ids: list[str], timeout: int
    ) -> list[Path]:
        if self._launcher is None:
            raise RuntimeError(
                "PaiSubstrate was constructed without a launcher body; the benchmark "
                "adapter must supply one"
            )
        return self._launcher(
            RunRequest(task_ref=task_ref, run_ids=tuple(run_ids), timeout=int(timeout)), install
        )

    def read_trajectory(
        self, stream_path: Path, *, obs_trunc: int, max_chars: int, args_trunc: int = 2000
    ) -> str:
        return render_events(
            Path(stream_path), obs_trunc=obs_trunc, max_chars=max_chars, args_trunc=args_trunc
        )

    def preflight(self) -> None:
        """The runner, Emacs and a model must be there; no model call is made."""
        runner = settings.runner()
        if not runner.is_file() or not os.access(runner, os.X_OK):
            raise RuntimeError(
                f"pai runner missing or not executable: {runner}; set PAI_HARNESS2_RUNNER"
            )
        emacs = settings.emacs()
        if shutil.which(emacs) is None and not Path(emacs).is_file():
            raise RuntimeError(f"Emacs not found ({emacs}); set PAI_EMACS")
        if not self.model:
            raise RuntimeError(
                "No pai executor model: set HARNESS2_PAI_MODEL (a pai model id such as "
                "anthropic/claude-sonnet-4-5)"
            )


# ---------------------------------------------------------------------------
# Trajectory rendering

_TRUNC_FMT = "…(+{n} chars — render truncation)"


def _clip(s, n: int) -> str:
    s = "" if s is None else str(s)
    s = s.replace("\x00", "")
    if n <= 0 or len(s) <= n:
        return s
    return s[:n] + _TRUNC_FMT.format(n=len(s) - n)


def parse_events(path: Path) -> list[dict]:
    """Read a pai-harness2 JSONL stream (junk lines tolerated)."""
    try:
        raw = Path(path).read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [{"type": "_read_error", "message": str(e)}]
    out = []
    for ln in raw.replace("\x00", "").splitlines():
        ln = ln.strip()
        if not ln.startswith("{"):
            continue
        try:
            ev = json.loads(ln)
        except json.JSONDecodeError:
            continue
        if isinstance(ev, dict):
            out.append(ev)
    return out


def _project_context(system_prompt: str) -> str:
    m = re.search(r"<project_context>(.*?)</project_context>", system_prompt or "", re.S)
    return m.group(1).strip() if m else ""


def _action_args(name: str, args) -> str:
    if isinstance(args, dict):
        if name == "bash" and "command" in args:
            return str(args["command"])
        if name in ("read", "write", "edit", "ls", "find") and "path" in args:
            rest = {k: v for k, v in args.items() if k not in ("path", "content")}
            extra = f" {json.dumps(rest, ensure_ascii=False)}" if rest else ""
            if name == "write":
                extra += f" ({len(str(args.get('content', '')))} chars)"
            return f"{args['path']}{extra}"
        return json.dumps(args, ensure_ascii=False)
    return str(args)


def render_events(path: Path, *, obs_trunc: int, max_chars: int, args_trunc: int = 2000) -> str:
    """Render a pai event stream into the shared trajectory grammar."""
    lines: list[str] = []
    started: set[str] = set()
    for ev in parse_events(Path(path)):
        et = ev.get("type")
        if et == "_read_error":
            return f"(failed to read trajectory: {ev.get('message')})"
        if et == "session":
            ctx = _project_context(ev.get("system_prompt") or "")
            if ctx:
                lines.append(f"[HARNESS AGENTS.md] {_clip(ctx, obs_trunc)}")
            if ev.get("hooks"):
                lines.append(f"[HARNESS] hooks active: {', '.join(ev['hooks'])}")
        elif et == "message" and ev.get("role") == "user":
            txt = ev.get("text") or ""
            if txt.strip():
                lines.append(f"[USER] {_clip(txt, obs_trunc)}")
        elif et == "message" and ev.get("role") == "assistant":
            for b in ev.get("content") or []:
                bt = b.get("type")
                if bt == "thinking" and (b.get("text") or "").strip():
                    lines.append(f"[REASONING] {_clip(b['text'], obs_trunc)}")
                elif bt == "text" and (b.get("text") or "").strip():
                    lines.append(f"[ASSISTANT] {_clip(b['text'], obs_trunc)}")
            if ev.get("stop_reason") in ("error", "aborted"):
                lines.append(f"[ERROR] {_clip(ev.get('error') or ev['stop_reason'], 1000)}")
        elif et == "tool_start":
            started.add(str(ev.get("id")))
            name = ev.get("name") or "tool"
            lines.append(
                f"[ACTION] {name}: {_clip(_action_args(name, ev.get('arguments')), args_trunc)}"
            )
        elif et == "tool_result":
            err = bool(ev.get("is_error"))
            lines.append(
                f"[OBSERVATION{' ERROR' if err else ''}] {_clip(ev.get('output'), obs_trunc)}"
            )
        elif et == "hook":
            if ev.get("continue"):
                lines.append(f"[HARNESS] Stop hook sent the agent back: {_clip(ev.get('reason'), 1000)}")
            elif ev.get("refused"):
                lines.append(f"[HARNESS] UserPromptSubmit hook refused: {_clip(ev['refused'], 1000)}")
            else:
                out = (ev.get("stdout") or "").strip() or (ev.get("stderr") or "").strip()
                lines.append(
                    f"[HARNESS] hook {ev.get('event')}: {_clip(ev.get('command'), 300)} "
                    f"| exit={ev.get('exit')}" + (f" | {_clip(out, 1000)}" if out else "")
                )
        elif et == "agent_end" and ev.get("aborted"):
            lines.append(f"[ERROR] run aborted: {_clip(ev.get('error') or 'aborted', 1000)}")
        elif et == "error":
            lines.append(f"[ERROR] {_clip(ev.get('message'), 1000)}")
        elif et == "done":
            u = ev.get("usage") or {}
            lines.append(
                "[HARNESS] run {s} after {w}s | tokens in={i} out={o} cache_read={c} "
                "| cost_usd={cost}".format(
                    s=ev.get("status"),
                    w=ev.get("wall_s"),
                    i=u.get("input"),
                    o=u.get("output"),
                    c=u.get("cache_read"),
                    cost=ev.get("cost_usd"),
                )
            )
    rendered = "\n".join(lines).strip()
    if not rendered:
        return "(no parseable events in trajectory)"
    if len(rendered) > max_chars:
        dropped = len(rendered) - max_chars
        rendered = rendered[:max_chars] + "\n" + _TRUNC_FMT.format(n=dropped)
    return rendered


def final_message(path: Path) -> str:
    """The final assistant text (from the `done` event, else the last text block)."""
    last = ""
    for ev in parse_events(Path(path)):
        if ev.get("type") == "done" and (ev.get("final") or "").strip():
            return ev["final"]
        if ev.get("type") == "message" and ev.get("role") == "assistant":
            for b in ev.get("content") or []:
                if b.get("type") == "text" and (b.get("text") or "").strip():
                    last = b["text"]
    return last


def run_cost(path: Path) -> float:
    """cost_usd of the `done` event, or 0."""
    for ev in reversed(parse_events(Path(path))):
        if ev.get("type") == "done":
            try:
                return float(ev.get("cost_usd") or 0.0)
            except (TypeError, ValueError):
                return 0.0
    return 0.0
