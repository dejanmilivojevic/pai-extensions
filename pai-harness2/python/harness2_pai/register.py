"""Teach an installed Harness² about the pai substrate and the pai improver channel.

Harness² ships two substrates (oc, cx) and two improver channels (opencode, claude_cli),
selected by a handful of module-level tables. `install()` extends those tables in place:

    harness2.core.spec.INSTALL_MODES        + "pai_root"
    harness2.core.spec.CHANNELS             + "pai"
    harness2.config.PROPOSER_MODEL_BY_CHANNEL["pai"]
    harness2.config.DEFAULT_PROPOSER_CHANNEL (HARNESS2_IMPROVER_CHANNEL, default "pai")
    harness2.core.prompts._SURFACE_TEXTS["pai"]   the improver's description of the tree
    harness2.core.proposer.Proposer               sessions on the "pai" channel
    harness2.cli._common.SUBSTRATES/make_adapter  `--substrate pai`
    harness2.cli.run_sweep._check_identity        records the pai executor model

It is idempotent and checks that every seam still exists, so an upstream refactor fails
loudly here instead of silently running the wrong agent.
"""

from __future__ import annotations

import functools
import json
import os
import shutil
import tempfile
from pathlib import Path

from . import settings

_INSTALLED = False

# Supported benchmarks for --substrate pai: all of Harness²'s.
PAI_BENCHES = ("jb", "lab", "wb")
WB_SUBSETS = ("office", "web", "code")


def _require(module, *names: str) -> None:
    missing = [n for n in names if not hasattr(module, n)]
    if missing:
        raise RuntimeError(
            f"harness2_pai: this Harness² checkout lacks {module.__name__}."
            f"{', '.join(missing)}; the pai integration targets the 2026 release layout"
        )


# ---------------------------------------------------------------------------
# Improver-facing description of the pai tree

_PAI_REPLACEMENTS = (
    (".agents/skills", ".pai/skills"),
    (".codex/hooks.json", ".pai/hooks.json"),
    (
        "an event -- `PreToolUse`, `PostToolUse`, `SessionStart` -- to matcher entries",
        "an event -- `PreToolUse`, `PostToolUse`, `SessionStart`, `UserPromptSubmit`, `Stop` "
        "-- to matcher entries",
    ),
    (
        "an event (`PreToolUse`, `PostToolUse`, `SessionStart`) to matcher entries",
        "an event (`PreToolUse`, `PostToolUse`, `SessionStart`, `UserPromptSubmit`, `Stop`) "
        "to matcher entries",
    ),
    ('such as "Bash", plus', 'such as "bash" (pai tool names: bash, read, write, edit, ls, '
     'find, grep; matched case-insensitively, `|` separates alternatives), plus'),
    ('as "Bash" plus', 'as "bash" (pai tool names; case-insensitive) plus'),
    (
        "`PreToolUse` can deny a call the same way.",
        "`PreToolUse` can deny a call the same way, and a blocking `Stop` hook sends its "
        "reason back as a new user turn (at most 3 times) -- the way to refuse a finish "
        "before the deliverable exists.",
    ),
    ("run via Bash", "run via the bash tool"),
    ("Scripts run via Bash.", "Scripts run via the bash tool."),
    (
        "a `name:` line is optional and must match the directory (it\noverrides it).",
        "a `name:` line is optional and must match the directory (it\noverrides it). "
        "Names are lowercase words joined by single hyphens (`fix-tables`), or pai drops "
        "the skill.",
    ),
    ("the runner installs each as a named agent\nrole.", "the runner installs each as a pai "
     "subagent\nrole, spawned with the `subagent` tool."),
)


def paiify(text: str) -> str:
    """Rewrite a Codex-surface prompt fragment for the pai surface."""
    for old, new in _PAI_REPLACEMENTS:
        text = text.replace(old, new)
    return text


def _install_surface_text(prompts) -> None:
    _require(prompts, "_SURFACE_TEXTS")
    if "pai" in prompts._SURFACE_TEXTS:
        return
    cx = prompts._SURFACE_TEXTS["codex"]
    prompts._SURFACE_TEXTS["pai"] = type(cx)(
        surface_md=paiify(cx.surface_md),
        inv=paiify(cx.inv),
        dom=paiify(cx.dom),
        tts_surface=paiify(cx.tts_surface),
    )


# ---------------------------------------------------------------------------
# The pai improver channel


def _pai_session(self, cwd: Path, mission: str, system: str, wall: int):
    """Proposer channel body: one headless pai session in CWD (the session workspace).

    Same contract as Proposer._opencode: MISSION.md in the workspace, the reply read from
    output/answer.md or the final message; returns (text, cost_usd, error).
    """
    from .substrate import PaiSubstrate, final_message, run_cost

    (cwd / "MISSION.md").write_text(
        (f"{system}\n\n---\n\n" if system else "") + mission, encoding="utf-8"
    )
    out = cwd / "output"
    out.mkdir(exist_ok=True)
    prompt = (
        "Read MISSION.md first. Do what it asks. Reply with the requested output as "
        "your final message AND save it verbatim to output/answer.md."
    )
    events_dir = Path(tempfile.mkdtemp(prefix="harness2-pai-prop-"))
    events = events_dir / "events.jsonl"
    try:
        rc = PaiSubstrate(model=self.spec.model).run(
            prompt, cwd, events, wall, model=self.spec.model, log=events_dir / "runner.log",
            rollout=False,
        )
        cost = run_cost(events)
        ans = out / "answer.md"
        if ans.is_file() and ans.read_text(errors="replace").strip():
            return ans.read_text(errors="replace"), cost, ""
        text = final_message(events)
        if text.strip():
            return text, cost, ""
        log = (events_dir / "runner.log").read_text(errors="replace")[-300:]
        return "", cost, f"rc={rc}: {log.strip() or 'empty reply'}"
    finally:
        if self.audit_dir:
            dst = Path(self.audit_dir) / "pai-sessions"
            dst.mkdir(parents=True, exist_ok=True)
            if events.is_file():
                shutil.copy(events, dst / f"{os.getpid()}-{events_dir.name}.jsonl")
        shutil.rmtree(events_dir, ignore_errors=True)


def _install_channel(spec, config, proposer) -> None:
    _require(spec, "CHANNELS")
    _require(config, "PROPOSER_MODEL_BY_CHANNEL", "DEFAULT_PROPOSER_CHANNEL")
    _require(proposer.Proposer, "_opencode")
    if "pai" not in spec.CHANNELS:
        spec.CHANNELS = (*spec.CHANNELS, "pai")
    config.PROPOSER_MODEL_BY_CHANNEL["pai"] = settings.improver_model()
    channel = settings.improver_channel()
    if channel not in spec.CHANNELS:
        raise RuntimeError(
            f"HARNESS2_IMPROVER_CHANNEL={channel!r}; expected one of {spec.CHANNELS}"
        )
    config.DEFAULT_PROPOSER_CHANNEL = channel
    if not getattr(proposer.Proposer._opencode, "_pai_wrapped", False):
        original = proposer.Proposer._opencode

        @functools.wraps(original)
        def _opencode_or_pai(self, cwd, mission, system, wall):
            if self.spec.kind == "pai":
                return _pai_session(self, cwd, mission, system, wall)
            return original(self, cwd, mission, system, wall)

        _opencode_or_pai._pai_wrapped = True
        proposer.Proposer._opencode = _opencode_or_pai


# ---------------------------------------------------------------------------
# CLI seams


def make_pai_adapter(bench: str, subset: str | None, split_file: Path | None = None):
    if bench not in PAI_BENCHES:
        raise ValueError(f"unknown bench {bench!r}; expected one of {PAI_BENCHES}")
    if bench == "wb":
        if subset not in WB_SUBSETS:
            raise ValueError(f"--bench wb needs --subset from {WB_SUBSETS}, got {subset!r}")
        from .workbuddy import PaiWbAdapter

        return PaiWbAdapter(subset, split_file=split_file)
    if subset:
        raise ValueError(f"--subset applies to --bench wb only, got {subset!r} for {bench}")
    if bench == "lab":
        from .lab import PaiLabAdapter

        return PaiLabAdapter(split_file=split_file)
    from .jobbench import PaiJobBenchAdapter

    return PaiJobBenchAdapter(split_file=split_file)


def _install_cli(common, run_sweep) -> None:
    _require(common, "SUBSTRATES", "make_adapter")
    if "pai" not in common.SUBSTRATES:
        common.SUBSTRATES = (*common.SUBSTRATES, "pai")
    if not getattr(common.make_adapter, "_pai_wrapped", False):
        original = common.make_adapter

        @functools.wraps(original)
        def make_adapter(bench, subset, substrate, split_file=None):
            if substrate == "pai":
                return make_pai_adapter(bench, subset, split_file)
            return original(bench, subset, substrate, split_file)

        make_adapter._pai_wrapped = True
        common.make_adapter = make_adapter

    if hasattr(run_sweep, "_check_identity") and not getattr(
        run_sweep._check_identity, "_pai_wrapped", False
    ):
        original_identity = run_sweep._check_identity
        from harness2 import config

        @functools.wraps(original_identity)
        def _check_identity(adapter, args, namespace_dir):
            if getattr(args, "substrate", None) != "pai":
                return original_identity(adapter, args, namespace_dir)
            saved = config.SOLVER_MODEL_CX, config.CODEX_BASE_URL
            config.SOLVER_MODEL_CX = f"pai:{settings.executor_model()}"
            config.CODEX_BASE_URL = ""
            try:
                return original_identity(adapter, args, namespace_dir)
            finally:
                config.SOLVER_MODEL_CX, config.CODEX_BASE_URL = saved

        _check_identity._pai_wrapped = True
        run_sweep._check_identity = _check_identity


def prepare_env() -> None:
    """Default the Harness² model variables from the pai ones, before config is imported.

    harness2.config reads HARNESS2_SOLVER_MODEL / HARNESS2_IMPROVER_MODEL at import time and
    validates them (provider/model syntax, which pai ids share).
    """
    model = settings.executor_model()
    if model:
        os.environ.setdefault("HARNESS2_SOLVER_MODEL", model)
    imp = settings.improver_model()
    if imp:
        os.environ.setdefault("HARNESS2_IMPROVER_MODEL", imp)


def install() -> None:
    """Register the pai substrate and channel with the imported Harness² (idempotent)."""
    global _INSTALLED
    prepare_env()
    from harness2 import config
    from harness2.cli import _common as common
    from harness2.cli import run_sweep
    from harness2.core import prompts, proposer, spec

    _require(spec, "INSTALL_MODES")
    from .substrate import INSTALL_MODE

    if INSTALL_MODE not in spec.INSTALL_MODES:
        spec.INSTALL_MODES = (*spec.INSTALL_MODES, INSTALL_MODE)
    _install_surface_text(prompts)
    _install_channel(spec, config, proposer)
    _install_cli(common, run_sweep)
    from .workbuddy import install_tables

    install_tables()
    _INSTALLED = True


def describe() -> dict:
    """The effective pai configuration, for `harness2-pai status`."""
    return {
        "executor_model": settings.executor_model(),
        "improver_model": settings.improver_model(),
        "improver_channel": settings.improver_channel(),
        "thinking": settings.thinking(),
        "emacs_argv": settings.emacs_argv(),
        "deny_hosts": settings.deny_hosts(),
        "extra_args": settings.extra_args(),
        "benches": list(PAI_BENCHES),
    }


def describe_json() -> str:
    return json.dumps(describe(), indent=2)
