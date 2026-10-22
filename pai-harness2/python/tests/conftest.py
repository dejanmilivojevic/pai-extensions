"""Shared fixtures: every rollout runs a real batch Emacs with pai's faux provider."""

from __future__ import annotations

import shutil
import textwrap
from pathlib import Path

import pytest

pytest.importorskip("harness2")

if shutil.which("emacs") is None:
    pytest.skip("emacs is not installed", allow_module_level=True)


@pytest.fixture(autouse=True)
def _pai_env(monkeypatch, tmp_path):
    monkeypatch.setenv("HARNESS2_PAI_MODEL", "faux")
    monkeypatch.setenv("HARNESS2_PAI_WORKSPACES", str(tmp_path / "workspaces"))
    monkeypatch.delenv("HARNESS2_PAI_ARGS", raising=False)
    monkeypatch.delenv("HARNESS2_IMPROVER_CHANNEL", raising=False)
    import harness2_pai

    harness2_pai.install()


@pytest.fixture
def faux(tmp_path, monkeypatch):
    """Write scripted faux responses (Elisp plists) and point rollouts at them."""

    def write(spec: str) -> Path:
        path = tmp_path / "faux.eld"
        path.write_text(textwrap.dedent(spec), encoding="utf-8")
        monkeypatch.setenv("HARNESS2_PAI_ARGS", f"--faux {path}")
        return path

    return write
