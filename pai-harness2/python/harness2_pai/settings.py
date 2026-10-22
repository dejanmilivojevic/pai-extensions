"""Environment knobs of the pai substrate (read at call time, never cached)."""

from __future__ import annotations

import json
import os
import shlex
from pathlib import Path

GRACE_SECONDS = 120
"""Wall time beyond --timeout before the runner process itself is killed."""

PACKAGE_DIR = Path(__file__).resolve().parent
EXTENSION_DIR = PACKAGE_DIR.parent.parent
"""extensions/pai-harness2/ (this package lives in its python/ directory)."""


def _env(name: str, default: str = "") -> str:
    return os.environ.get(name, default).strip()


def executor_model() -> str:
    """pai model id for rollouts: HARNESS2_PAI_MODEL, else HARNESS2_SOLVER_MODEL."""
    return _env("HARNESS2_PAI_MODEL") or _env("HARNESS2_SOLVER_MODEL")


def improver_model() -> str:
    """pai model id for improver sessions on the `pai` channel."""
    return (
        _env("HARNESS2_PAI_IMPROVER_MODEL")
        or _env("HARNESS2_IMPROVER_MODEL")
        or executor_model()
    )


def improver_channel() -> str:
    """Channel improver sessions run on: pai (default), opencode or claude_cli."""
    return _env("HARNESS2_IMPROVER_CHANNEL", "pai") or "pai"


def thinking() -> str:
    """Thinking level passed to rollouts (HARNESS2_PAI_THINKING), or ''."""
    return _env("HARNESS2_PAI_THINKING")


def emacs() -> str:
    return _env("PAI_EMACS", "emacs") or "emacs"


def pai_root() -> Path | None:
    """The pai checkout: PAI_ROOT, else the one this extension lives in."""
    raw = _env("PAI_ROOT")
    root = Path(raw).expanduser() if raw else EXTENSION_DIR.parent.parent
    return root if (root / "lisp" / "pai.el").is_file() else None


def emacs_argv() -> list[str]:
    """The child-Emacs command line of one rollout (arguments follow it).

    `/harness2' passes the one its Emacs computed (PAI_HARNESS2_EMACS_ARGV, a JSON
    list: the same load path the running Emacs loaded pai from); otherwise it is
    derived from the pai checkout.
    """
    raw = _env("PAI_HARNESS2_EMACS_ARGV")
    if raw:
        argv = json.loads(raw)
        if not (isinstance(argv, list) and argv and all(isinstance(a, str) for a in argv)):
            raise ValueError("PAI_HARNESS2_EMACS_ARGV must be a JSON list of strings")
        return list(argv)
    root = pai_root()
    if root is None:
        raise RuntimeError(
            "cannot locate pai: start Harness² from /harness2 in Emacs, or set PAI_ROOT"
        )
    load = [root / "lisp", root / "vendor" / "vui", root / "test", EXTENSION_DIR]
    argv = [emacs(), "-Q", "--batch", "--eval", "(setq load-prefer-newer t)"]
    for d in load:
        if d.is_dir():
            argv += ["-L", str(d)]
    return argv + ["-l", "pai-harness2-batch", "-f", "pai-harness2-batch-main"]


DEFAULT_DENY_HOSTS = ("huggingface.co", "hf.co", "github.com", "raw.githubusercontent.com")
"""Hosts benchmark rollouts may not reach: where the benchmarks' data and grading
criteria are published (see Harness²'s docs/EXPERIMENTS.md, "Web access")."""


def deny_hosts() -> list[str]:
    """HARNESS2_PAI_DENY_HOSTS: comma list; `none` disables; default DEFAULT_DENY_HOSTS."""
    raw = _env("HARNESS2_PAI_DENY_HOSTS")
    if raw.lower() in ("none", "off", "0"):
        return []
    return [h.strip() for h in raw.split(",") if h.strip()] if raw else list(DEFAULT_DENY_HOSTS)


def deny_tools() -> list[str]:
    """HARNESS2_PAI_DENY_TOOLS: comma list of pai tools removed from rollouts."""
    return [t.strip() for t in _env("HARNESS2_PAI_DENY_TOOLS").split(",") if t.strip()]


def extra_args() -> list[str]:
    """Extra runner arguments (HARNESS2_PAI_ARGS, shell-split)."""
    raw = _env("HARNESS2_PAI_ARGS")
    return shlex.split(raw) if raw else []


def runner_env() -> dict[str, str]:
    return dict(os.environ)


def workspace_base() -> Path:
    base = Path(_env("HARNESS2_PAI_WORKSPACES", "/tmp/pai_benchmark_workspace")).expanduser()
    base.mkdir(parents=True, exist_ok=True)
    return base


def timeout(default: int) -> int:
    raw = _env("HARNESS2_PAI_TIMEOUT")
    return int(raw) if raw.isdigit() else default
