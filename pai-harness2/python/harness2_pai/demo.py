"""The Harness² local demo with real pai rollouts.

Upstream's `harness2-demo` runs the whole recursion (base rollouts, Propose-Probe-Compose,
selection, the experience library) on a two-task ledger domain, with a scripted editor
and a fake executor that just runs the harness's script. Here every rollout is a real
headless pai session working in a workspace that holds the harness: pai reads AGENTS.md,
runs scripts/summarize.py with its bash tool and writes answer.txt.

The editor stays scripted by default (`DemoProposer`: it fixes the summing bug in the
script), so the demo checks the pai substrate end to end without paying for improver
sessions; pass `--improver pai` to let pai edit the harness too.
"""

from __future__ import annotations

import shutil
import tempfile
from pathlib import Path

from harness2.core import tree
from harness2.core.spec import BenchPrompts, BenchSpec, ChannelSpec
from harness2.demo import BASE_SCRIPT

from .substrate import INSTALL_MODE, PAI_SURFACE, PaiSubstrate, install_tree

TASK_PROMPT = (
    "Sum every signed amount in ledger.csv. Return Total: with two decimal places. "
    "Write the result line to answer.txt in the current directory."
)

BASE_AGENTS_MD = (
    "Reconcile the ledger using `python3 scripts/summarize.py ledger.csv` and write the "
    "line it prints, verbatim, to answer.txt.\n"
)


class PaiDemoAdapter:
    """The upstream DemoAdapter's domain, executed by pai."""

    def __init__(self, root: Path, *, channel: ChannelSpec | None = None, timeout: int = 600):
        self.root = root
        self.timeout = timeout
        self.substrate = PaiSubstrate()
        channel = channel or ChannelSpec("opencode", "local/deterministic")
        self.spec = BenchSpec(
            name="demo-pai",
            runs_root=root / "runs",
            install_mode=INSTALL_MODE,
            prompts=BenchPrompts(work="reconcile signed amounts in a transaction ledger"),
            channels={"proposer": channel, "selector": channel},
            leak_globs=("reference_answer.txt",),
        )
        self.execution_count = 0
        base = {name: "" for name in PAI_SURFACE.always_on}
        base["AGENTS.md"] = BASE_AGENTS_MD
        base["scripts/summarize.py"] = BASE_SCRIPT
        tree.write_tree(PAI_SURFACE, base, self.base_harness(), stubs=False)
        for task_id, amounts in (
            ("ledger-1", "100.25\n-20.10\n5.00\n"),
            ("ledger-2", "35.50\n-10.25\n-5.00\n"),
        ):
            source = self.task_files(task_id)
            source.mkdir(parents=True, exist_ok=True)
            (source / "ledger.csv").write_text("amount\n" + amounts, encoding="utf-8")

    def tasks(self, split: str = "all") -> list[str]:
        return ["ledger-1", "ledger-2"]

    def domain_of(self, task_id: str) -> str:
        return "accounting"

    def task_prompt(self, task_id: str) -> str:
        return TASK_PROMPT

    def task_files(self, task_id: str) -> Path:
        return self.root / "sources" / task_id

    def base_harness(self) -> Path:
        return self.root / "base"

    def validate_tree(self, harness: tree.Tree) -> None:
        if "scripts/summarize.py" not in harness:
            raise ValueError("The demo harness requires scripts/summarize.py")

    def execute(
        self, task_id: str, harness_dir: Path | None, run_id: str, *, timeout: int | None = None
    ) -> Path:
        self.execution_count += 1
        run_dir = self.root / "executions" / run_id
        if (run_dir / "answer.txt").is_file() and (run_dir / "events.jsonl").is_file():
            return run_dir
        run_dir.mkdir(parents=True, exist_ok=True)
        install = self.substrate.materialise(
            Path(harness_dir or self.base_harness()), self.spec, {}
        )
        ws = Path(tempfile.mkdtemp(prefix=f"pai-demo-{task_id}-"))
        try:
            shutil.copy(self.task_files(task_id) / "ledger.csv", ws / "ledger.csv")
            install_tree(install.staging_dir, ws)
            rc = self.substrate.run(
                TASK_PROMPT,
                ws,
                run_dir / "events.jsonl",
                int(timeout or self.timeout),
                log=run_dir / "runner.log",
            )
            answer = ws / "answer.txt"
            if answer.is_file():
                shutil.copy(answer, run_dir / "answer.txt")
            elif rc != 0:
                raise RuntimeError(
                    f"pai rollout for {task_id} failed (exit {rc}); see {run_dir / 'runner.log'}"
                )
        finally:
            shutil.rmtree(ws, ignore_errors=True)
            shutil.rmtree(install.staging_dir, ignore_errors=True)
        return run_dir

    def delivered(self, run_dir: Path) -> bool:
        return (Path(run_dir) / "answer.txt").is_file()

    def trajectory(self, run_dir: Path, *, obs_trunc: int) -> str:
        return self.substrate.read_trajectory(
            Path(run_dir) / "events.jsonl", obs_trunc=obs_trunc, max_chars=200_000
        )

    def deliverable_text(self, run_dir: Path) -> str:
        p = Path(run_dir) / "answer.txt"
        return p.read_text(encoding="utf-8") if p.is_file() else ""

    def deliverable_files(self, run_dir: Path) -> dict[str, str]:
        return {"answer.txt": self.deliverable_text(run_dir)}
