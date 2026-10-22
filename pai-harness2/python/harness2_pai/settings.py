"""Environment knobs of the pai substrate (read at call time, never cached)."""

from __future__ import annotations

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


def runner() -> Path:
    """The pai-harness2-run script."""
    return Path(
        _env("PAI_HARNESS2_RUNNER") or EXTENSION_DIR / "bin" / "pai-harness2-run"
    ).expanduser()


def emacs() -> str:
    return _env("PAI_EMACS", "emacs") or "emacs"


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
