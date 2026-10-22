"""LAB (harvey-labs) on the pai substrate.

Everything benchmark-shaped (task discovery, the criteria-free task view, extraction with
the judge's own reader, judging, scoring) is inherited from Harness²'s `LabAdapter`. The
launcher mirrors the upstream Codex twin (external/adapt_runs_cx/run_codex_lab.py) step for
step: a scratch workspace with read-only documents/, output/ and the harness at its root,
scripts/read_doc.py installed LAST so no harness file can shadow it, the same assignment
message, then config.json, pai_session.jsonl, output/ and metrics.json (written last, as
the completion record) in the run directory.
"""

from __future__ import annotations

import dataclasses
import json
import os
import shutil
import tempfile
import time
from datetime import datetime, timezone
from pathlib import Path

from harness2 import config
from harness2.benchmarks import lab as LAB
from harness2.core import tree as HT
from harness2.substrate.base import Install, RunRequest

from . import settings
from .substrate import INSTALL_MODE, PAI_SURFACE, PaiSubstrate, install_tree, parse_events
from .substrate import validate_pai_tree

SESSION_FILE = "pai_session.jsonl"


def build_prompt(instructions: str, workspace: Path) -> str:
    """The assignment message: instructions verbatim plus the concrete absolute paths."""
    return (
        f"=== WORKSPACE ===\n{workspace}\n\n"
        f"=== DOCUMENTS (read-only) ===\n{workspace / 'documents'}\n\n"
        f"=== OUTPUT DIRECTORY (deliverables go here) ===\n{workspace / 'output'}\n\n"
        f"=== ASSIGNMENT ===\n{instructions.strip()}"
    )


def session_summary(events_path: Path) -> dict:
    """Counts for metrics.json from a pai event stream."""
    tool_calls = turns = errors = 0
    usage: dict = {}
    final = ""
    for ev in parse_events(events_path):
        t = ev.get("type")
        if t == "tool_start":
            tool_calls += 1
        elif t == "message" and ev.get("role") == "assistant":
            turns += 1
            if ev.get("stop_reason") in ("error", "aborted"):
                errors += 1
        elif t == "error":
            errors += 1
        elif t == "done":
            usage = ev.get("usage") or {}
            final = ev.get("final") or ""
    return {
        "n_events": len(parse_events(events_path)),
        "tool_calls": tool_calls,
        "turns": turns,
        "errors": errors,
        "input_tokens": int(usage.get("input") or 0),
        "output_tokens": int(usage.get("output") or 0),
        "cached_input_tokens": int(usage.get("cache_read") or 0),
        "final_text": final,
    }


class PaiLabAdapter(LAB.LabAdapter):
    """LAB behind the BenchmarkAdapter protocol, executed by pai."""

    def __init__(self, *, model: str | None = None, timeout: int | None = None, **kw):
        # The Codex cell is the shape pai shares: one always-on AGENTS.md at the root.
        super().__init__(substrate="cx", timeout=timeout or settings.timeout(3600), **kw)
        self.spec = dataclasses.replace(
            self.spec, install_mode=INSTALL_MODE
        )
        self.substrate = PaiSubstrate(launcher=self._launch_pai, model=model)
        self.model = self.substrate.model
        self._results_root = config.RUNS / "lab" / "results" / "pai"

    def base_harness(self) -> Path:
        d = self.spec.runs_root / f"_base_harness_pai_{os.getpid()}"
        with LAB._BASE_LOCK:
            if d not in LAB._BASE_DONE:
                HT.write_tree(PAI_SURFACE, {"AGENTS.md": LAB.CODEX_BASE_BODY}, d)
                LAB._BASE_DONE.add(d)
        return d

    def validate_tree(self, tree: dict[str, str]) -> None:
        errs = []
        try:
            self._validate_cx(tree)  # AGENTS.md must keep the output/ and read_doc.py rules
        except ValueError as exc:
            errs.append(str(exc).replace("under codex", "under pai"))
        try:
            validate_pai_tree(tree)
        except ValueError as exc:
            errs.append(str(exc))
        if errs:
            raise ValueError(" | ".join(errs))

    def trajectory(self, run_dir: Path, *, obs_trunc: int) -> str:
        stream = Path(run_dir) / SESSION_FILE
        if not stream.is_file():
            return f"(no trajectory recorded for {Path(run_dir).name})"
        return self.substrate.read_trajectory(stream, obs_trunc=obs_trunc, max_chars=10**9)

    def _read_doc(self) -> Path:
        return config.lab_repo() / "opencode" / "scripts" / "read_doc.py"

    def preflight(self) -> None:
        self.substrate.preflight()
        if not config.LAB_PYTHON.is_file():
            raise RuntimeError(
                f"LAB interpreter missing at {config.LAB_PYTHON}; run /harness2 setup lab"
            )
        if not self._read_doc().is_file():
            raise RuntimeError(
                f"LAB document reader missing at {self._read_doc()}; run /harness2 setup lab"
            )
        import subprocess

        r = subprocess.run(
            [str(config.LAB_PYTHON), "-c", "import evaluation.scoring"],
            cwd=config.lab_repo(),
            env=LAB._lab_env(),
            capture_output=True,
            text=True,
            timeout=120,
        )
        if r.returncode != 0:
            raise RuntimeError(
                f"`import evaluation.scoring` fails under {config.LAB_PYTHON}; "
                f"run /harness2 setup lab"
            )

    def _scratch(self) -> Path:
        base = Path(
            os.environ.get("HARNESS2_PAI_LAB_SCRATCH") or config.RUNS / "lab" / "scratch"
        )
        base.mkdir(parents=True, exist_ok=True)
        return base

    def _launch_pai(self, req: RunRequest, install: Install) -> list[Path]:
        dirs = []
        for run_id in req.run_ids:
            dirs.append(self._run_one(req.task_ref, run_id, req.timeout, install))
        return dirs

    def _run_one(self, task_id: str, run_id: str, timeout: int, install: Install) -> Path:
        self.substrate.preflight()
        run_dir = self._run_dir(run_id)
        run_dir.mkdir(parents=True, exist_ok=True)
        tmp_root = Path(tempfile.mkdtemp(prefix="pai-lab-", dir=self._scratch()))
        ws = tmp_root / "ws"
        started = datetime.now(timezone.utc).isoformat()
        t0 = time.time()
        try:
            (ws / "output").mkdir(parents=True)
            docs = self.task_files(task_id)
            if docs is not None:
                shutil.copytree(docs, ws / "documents")
                for p in (ws / "documents").rglob("*"):
                    if p.is_file():
                        p.chmod(0o444)
            else:
                (ws / "documents").mkdir()
            harness = install.staging_dir if install.extras.get("harness") != "base" else None
            if harness is None:
                harness = self.base_harness()
                staged = self.substrate.materialise(harness, self.spec, {}).staging_dir
            else:
                staged = harness
            install_tree(staged, ws)
            installed = sorted(
                str(p.relative_to(ws))
                for p in ws.rglob("*")
                if p.is_file() and not str(p.relative_to(ws)).startswith(("documents/", "output/"))
            )
            (ws / "scripts").mkdir(exist_ok=True)
            shutil.copy2(self._read_doc(), ws / "scripts" / "read_doc.py")  # LAST: runtime wins
            if staged != install.staging_dir:
                shutil.rmtree(staged, ignore_errors=True)
            cfg = {
                "runner": "pai",
                "model": self.model,
                "emacs_argv": settings.emacs_argv(),
                "task": task_id,
                "run_id": run_id,
                "timeout": timeout,
                "harness_files": installed,
                "workspace": str(ws),
                "started_at": started,
            }
            (run_dir / "config.json").write_text(json.dumps(cfg, indent=2), encoding="utf-8")
            session = run_dir / SESSION_FILE
            env = {
                # read_doc.py and helper scripts run under the LAB interpreter, as for Codex.
                "PATH": LAB._lab_env()["PATH"],
                "LAB_DOC_PARSER": os.environ.get("LAB_DOC_PARSER", "sandbox"),
                "LAB_BENCH_ROOT": str(config.lab_repo()),
            }
            prompt = build_prompt(str(self._task_json(task_id).get("instructions", "")), ws)
            rc = self.substrate.run(
                prompt, ws, session, int(max(60, timeout)), log=run_dir / "pai_stderr.log",
                env=env,
            )
            wall = time.time() - t0
            graded = run_dir / "output"
            graded.mkdir(parents=True, exist_ok=True)
            for src in (ws / "output").rglob("*"):
                if src.is_file():
                    dest = graded / src.relative_to(ws / "output")
                    dest.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copy2(src, dest)
            delivered = sorted(str(p.relative_to(graded)) for p in graded.rglob("*") if p.is_file())
            summary = session_summary(session)
            if summary["n_events"] == 0:
                # No parseable events: a dead chain, never cached as a finished rollout.
                return run_dir
            if rc == 0 and not delivered:
                rc = 1
            text = session.read_text(encoding="utf-8", errors="replace")
            rels = [str(p.relative_to(ws / "documents"))
                    for p in sorted((ws / "documents").rglob("*")) if p.is_file()]
            metrics = {
                "runner": "pai",
                "model": self.model,
                "task": task_id,
                "run_id": run_id,
                "turn_count": summary["turns"],
                "tool_calls": summary["tool_calls"],
                "stream_errors": summary["errors"],
                "input_tokens": summary["input_tokens"],
                "cached_input_tokens": summary["cached_input_tokens"],
                "output_tokens": summary["output_tokens"],
                "total_tokens": summary["input_tokens"] + summary["output_tokens"],
                "wall_clock_seconds": round(wall, 1),
                "finished_cleanly": rc == 0,
                "exit_code": rc,
                "deliverables": delivered,
                "documents_read": sum(1 for r in rels if r in text),
                "total_documents": len(rels),
                "harness_files": installed,
                "completed_at": datetime.now(timezone.utc).isoformat(),
            }
            (run_dir / "metrics.json").write_text(json.dumps(metrics, indent=2), encoding="utf-8")
            return run_dir
        finally:
            shutil.rmtree(tmp_root, ignore_errors=True)
