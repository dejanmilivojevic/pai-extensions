"""JobBench on the pai substrate.

Everything benchmark-shaped (task discovery, store layout, judge, scoring, extraction) is
inherited from Harness²'s `JobBenchAdapter`; only the launcher differs. It mirrors the
upstream Codex runner (third_party/jb_runner/run_benchmark_codex_cli_adapt.sh) step for
step, in Python: a private /tmp workspace holding task_folder/ and output/, the harness
copied to the workspace root, the same prompt, one JSONL trajectory per attempt under
model_traj/<label>/, an attempt_index.tsv row, and the deliverables moved to
model_output/<label>/.
"""

from __future__ import annotations

import dataclasses
import shutil
import tempfile
from datetime import datetime, timezone
from pathlib import Path

from harness2.benchmarks import jobbench as JB
from harness2.core.spec import ChannelSpec, resolve_channel_model
from harness2.substrate.base import Install, RunRequest

from . import settings
from .substrate import INSTALL_MODE, PaiSubstrate, install_tree, validate_pai_tree

DEFAULT_TIMEOUT = 3600

PROMPT = """=== TASK FOLDER ===
{task_folder}

=== INSTRUCTIONS ===
1. Read the TASK_INSTRUCTIONS.txt file in the task folder above
2. Based on the Reference Files section in TASK_INSTRUCTIONS.txt, read the corresponding files from the same task folder using appropriate tools.
3. Complete the task as specified in TASK_INSTRUCTIONS.txt
4. Only save the final deliverables to the output directory specified below. Do not save any intermediate or temporary files.

=== OUTPUT DIRECTORY ===
{output}

IMPORTANT:
- All reference files are in the task folder: {task_folder}
- Only save the final deliverables to the output directory {output}. Do not save any intermediate or temporary files.
- You MUST only access files within {workspace} or search online for new reference files if you find needed. Do NOT access any files or directories in this system outside of this path.
- If you encounter ambiguous or conflicting information, analyze the conflict, explain your reasoning, and justify the approach you choose.
- If a file cannot be read directly (e.g., .xlsx, .docx, .db, .pptx), use appropriate tools, MCP servers, or code to extract and process its contents."""

_INDEX_HEADER = (
    "task_name\tmodel_id\tmodel_name\tattempt\tstatus\texit_code\ttimeout_seconds\t"
    "session_id\tstarted_at\tfinished_at\ttraj_file\n"
)


def improver_channels() -> dict[str, ChannelSpec]:
    ch = resolve_channel_model(None, None, effort="high")
    return {"proposer": ch, "selector": ch}


def _now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def _session_id(events: Path) -> str:
    from .substrate import parse_events

    for ev in parse_events(events):
        if ev.get("type") == "session":
            return str(ev.get("session_id") or "")
    return ""


class PaiJobBenchAdapter(JB.JobBenchAdapter):
    """JobBench behind the BenchmarkAdapter protocol, executed by pai."""

    def __init__(
        self,
        substrate: str = "pai",
        *,
        timeout: int | None = None,
        store_root: Path | None = None,
        split_file: Path | None = None,
        model: str | None = None,
    ):
        # The Codex cell is the closest upstream shape (one always-on AGENTS.md at the
        # workspace root); build it, then swap in the pai substrate and install mode.
        super().__init__(
            "cx",
            timeout=timeout or settings.timeout(DEFAULT_TIMEOUT),
            store_root=store_root,
            split_file=split_file,
        )
        self.spec = dataclasses.replace(
            self.spec,
            install_mode=INSTALL_MODE,
            allow_subagents=True,
            channels=improver_channels(),
        )
        self.substrate = PaiSubstrate(launcher=self._launch_pai, model=model)

    def base_harness(self) -> Path:
        """The upstream Codex base harness: a two-line AGENTS.md, valid as a pai tree too,
        so the pai and Codex cells start from the same instructions."""
        return super().base_harness()

    def validate_tree(self, tree: dict) -> None:
        super().validate_tree(tree)
        validate_pai_tree(tree)

    def preflight(self) -> None:
        super().preflight()

    def _launch_pai(self, req: RunRequest, install: Install) -> list[Path]:
        task_id, label, t = self._launch_common(req)
        self.substrate.preflight()
        self._ensure_store_task(task_id)
        task_dir = self.store / task_id / "task_folder"
        if not (task_dir / "TASK_INSTRUCTIONS.txt").is_file():
            raise FileNotFoundError(f"TASK_INSTRUCTIONS.txt not found in {task_dir}")
        profession, task_n = task_id.split("/", 1)
        task_name = f"{profession}_{task_n}"
        final_out = self._out_dir(task_id, label)
        traj_dir = self._traj_dir(task_id, label)
        traj_dir.mkdir(parents=True, exist_ok=True)
        log_dir = self.store / ".harness2-logs"
        log_dir.mkdir(parents=True, exist_ok=True)
        stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        traj_file = traj_dir / f"{task_name}_{label}_attempt1_{stamp}.jsonl"
        log_file = log_dir / f"jb_{task_name}_pai_{stamp}.log"

        ws = Path(tempfile.mkdtemp(prefix=f"{task_name}_pai_", dir=settings.workspace_base()))
        try:
            shutil.copytree(task_dir, ws / "task_folder")
            out = ws / "output"
            out.mkdir()
            install_tree(install.staging_dir, ws)
            prompt = PROMPT.format(task_folder=ws / "task_folder", output=out, workspace=ws)
            started = _now()
            rc = self.substrate.run(prompt, ws, traj_file, t, log=log_file)
            produced = any(p.is_file() for p in out.rglob("*"))
            if rc == 0 and not produced:
                rc = 1
            status = "success" if rc == 0 else "timeout" if rc == 124 else "failed"
            index = traj_dir / "attempt_index.tsv"
            if not index.is_file():
                index.write_text(_INDEX_HEADER, encoding="utf-8")
            with index.open("a", encoding="utf-8") as fh:
                fh.write(
                    "\t".join(
                        [
                            task_name,
                            self.substrate.model,
                            label,
                            "1",
                            status,
                            str(rc),
                            str(t),
                            _session_id(traj_file),
                            started,
                            _now(),
                            str(traj_file),
                        ]
                    )
                    + "\n"
                )
            if produced:
                final_out.mkdir(parents=True, exist_ok=True)
                for p in out.iterdir():
                    shutil.move(str(p), final_out / p.name)
        finally:
            shutil.rmtree(ws, ignore_errors=True)
        return [self._require_artifacts(task_id, label)]
