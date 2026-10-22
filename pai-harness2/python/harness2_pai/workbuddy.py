"""WorkBuddy-Bench on the pai substrate.

Upstream runs each rollout as a Harbor batch: a generated job YAML names the harness, and
Harbor starts the task container and the harness agent. The pai harness
(`wb_runner/agents/pai_harness_agent.py`, installed into the WorkBuddy checkout by
`harness2-pai setup wb`) keeps pai on the host and works in the container over TRAMP, so
the container image stays untouched -- no Emacs, no mount.

Everything else (batching, trial discovery and ranking, rendering, the verifier's score)
is the inherited `WbAdapter`; `register.install()` routes the job YAML and concurrency
tables for the `pai` substrate here.
"""

from __future__ import annotations

import dataclasses
import json
import os
import re
import shutil
import threading
from pathlib import Path

from harness2 import config
from harness2.benchmarks import workbuddy as WB
from harness2.core import tree as HT

from . import settings
from .substrate import INSTALL_MODE, PAI_SURFACE, PaiSubstrate, parse_events, validate_pai_tree

HARNESS_FAMILY = "pai-harness"
HARNESS_VERSION = "0.1.0"
EVENTS = "pai-events.jsonl"
ASSETS = Path(__file__).resolve().parent / "wb_runner"

BASE_AGENTS_MD = (
    "Follow the task instructions and complete the requested work in the workspace, "
    "writing every deliverable where the instructions say."
)

_REGISTRATION = '''    # pai (the Emacs agent) + harness workspace, installed by harness2-pai setup wb.
    # pai runs on the host and works in the container over TRAMP; under local_proxy it
    # addresses the bench proxy itself (connection.proxy_url), so no env pair is used.
    "pai-harness": HarnessRuntimeAdapter(
        harness_name="pai-harness",
        canonical_display_name="pai (Emacs agent, harness-adaptive)",
        harness_protocol="openai",
        backend_base_env="PAI_BASE_URL",
        backend_key_env="PAI_API_KEY",
        proxy_url_env="",
        uses_anthropic_env=False,
    ),
'''


def harness_slug() -> str:
    return f"{HARNESS_FAMILY}/{HARNESS_VERSION}"


def wb_model() -> str:
    """HARNESS2_PAI_WB_MODEL: a pai model id; empty = the bench proxy's solver route."""
    return os.environ.get("HARNESS2_PAI_WB_MODEL", "").strip()


# ---------------------------------------------------------------------------
# Installing the pai harness into a WorkBuddy checkout


def _adapters_file(repo: Path) -> Path:
    return repo / "src" / "workbuddy_bench" / "runner" / "harness_adapters.py"


def registration_present(repo: Path) -> bool:
    f = _adapters_file(repo)
    return f.is_file() and '"pai-harness": HarnessRuntimeAdapter(' in f.read_text(encoding="utf-8")


def install_assets(repo: Path, *, dry_run: bool = False) -> list[str]:
    """Copy the pai agent and harness configs into REPO and register the runtime adapter.

    Idempotent. Returns a description of each step taken.
    """
    steps = []
    agent_dst = repo / "src" / "workbuddy_bench" / "agents" / "pai_harness_agent.py"
    cfg_dst = repo / "configs" / "harnesses" / HARNESS_FAMILY
    steps.append(f"copy {ASSETS / 'agents/pai_harness_agent.py'} -> {agent_dst}")
    steps.append(f"copy {ASSETS / 'configs/harnesses' / HARNESS_FAMILY} -> {cfg_dst}")
    if not dry_run:
        agent_dst.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(ASSETS / "agents" / "pai_harness_agent.py", agent_dst)
        shutil.copytree(ASSETS / "configs" / "harnesses" / HARNESS_FAMILY, cfg_dst,
                        dirs_exist_ok=True)
    f = _adapters_file(repo)
    if registration_present(repo):
        steps.append(f"{f}: pai-harness already registered")
        return steps
    steps.append(f"register pai-harness in {f}")
    if dry_run:
        return steps
    src = f.read_text(encoding="utf-8")
    m = re.search(r"^HARNESS_ADAPTERS: dict\[str, HarnessRuntimeAdapter\] = \{\n.*?^\}\n",
                  src, re.S | re.M)
    if m is None:
        raise RuntimeError(
            f"{f}: HARNESS_ADAPTERS table not found; this WorkBuddy checkout is not the "
            f"pinned revision"
        )
    end = m.end() - 2  # before the closing "}\n"
    f.write_text(src[:end] + _REGISTRATION + src[end:], encoding="utf-8")
    return steps


# ---------------------------------------------------------------------------
# The job YAML for a pai batch


def job_yaml(
    subset: WB.Subset, label: str, tasks: list[str], ws: Path | None, k: int, jobs_dir_abs: Path
) -> str:
    sel = "\n".join(f"    - {t}" for t in tasks)
    hp: dict = {
        "PAI_EMACS_ARGV": json.dumps(settings.emacs_argv()),
        "PAI_TIMEOUT": settings.timeout(3600),
        "PAI_ARGS": json.dumps(
            [a for h in settings.deny_hosts() for a in ("--deny-host", h)]
            + [a for t in settings.deny_tools() for a in ("--deny-tool", t)]
            + settings.extra_args()
        ),
    }
    if wb_model():
        hp["PAI_MODEL"] = wb_model()
    if settings.thinking():
        hp["PAI_THINKING"] = settings.thinking()
    if ws is not None:
        hp["WB_HARNESS_WS"] = str(Path(ws).resolve())
    hp_lines = "\n".join(f"  {key}: {json.dumps(val)}" for key, val in sorted(hp.items()))
    judge = (
        f"llm_judge_override:\n  model: {subset.llm_judge_model}\n"
        if subset.llm_judge_model
        else ""
    )
    return (
        f"# GENERATED by harness2_pai/workbuddy.py -- ephemeral, safe to delete. label={label}\n"
        f"model: {config.WB_SOLVER_MODEL}\n"
        f"harness: {harness_slug()}\n"
        f"dataset: datasets/{subset.dataset_id}/tasks\n"
        "harness_backend: local\n"
        f"model_connection: {config.WB_MODEL_CONNECTION}\n"
        f"n_attempts: {k}\n"
        f"jobs_dir: {jobs_dir_abs}\n"
        "task_selection:\n  mode: name\n  names:\n"
        f"{sel}\n"
        "orchestrator_override:\n"
        f"  n_concurrent_trials: {WB._conc('pai')}\n"
        f"harness_params_override:\n{hp_lines}\n"
        f"{judge}"
    )


def install_tables() -> None:
    """Teach the upstream WorkBuddy module about the `pai` substrate (idempotent)."""
    WB.MAX_JOBS.setdefault("pai", 1)
    WB.MAX_TRIALS.setdefault("pai", 2)
    WB._slots.setdefault("pai", threading.Semaphore(WB.MAX_JOBS["pai"]))
    if "pai" not in WB.SUBSTRATES:
        WB.SUBSTRATES = (*WB.SUBSTRATES, "pai")
    if not getattr(WB._job_yaml, "_pai_wrapped", False):
        original = WB._job_yaml

        def _job_yaml(subset, substrate, label, tasks, ws, k, jobs_dir_abs):
            if substrate == "pai":
                return job_yaml(subset, label, tasks, ws, k, jobs_dir_abs)
            return original(subset, substrate, label, tasks, ws, k, jobs_dir_abs)

        _job_yaml._pai_wrapped = True
        WB._job_yaml = _job_yaml


# ---------------------------------------------------------------------------
# The adapter


class PaiWbAdapter(WB.WbAdapter):
    """One (subset, pai) cell of WB-Bench."""

    def __init__(self, subset: str = "office", *, split_file: Path | None = None):
        install_tables()
        super().__init__(subset, substrate="cx", split_file=split_file)
        self.spec = dataclasses.replace(
            self.spec, install_mode=INSTALL_MODE, allow_subagents=True
        )
        self.substrate = PaiSubstrate(
            launcher=self._launch, model=wb_model() or config.WB_SOLVER_MODEL
        )

    def _cell_identity(self) -> dict:
        return {
            "solver": f"pai:{wb_model() or config.WB_SOLVER_MODEL}",
            "pai_ver": HARNESS_VERSION,
            "connection": config.WB_MODEL_CONNECTION,
        }

    def trajectory_path(self, run_dir: Path) -> Path:
        return Path(run_dir) / "agent" / EVENTS

    def base_harness(self) -> Path:
        d = self.spec.runs_root / f"_base_harness_pai_{os.getpid()}"
        with WB._BASE_LOCK:
            if d not in WB._BASE_DONE:
                HT.write_tree(PAI_SURFACE, {"AGENTS.md": BASE_AGENTS_MD}, d, stubs=True)
                WB._BASE_DONE.add(d)
        return d

    def validate_tree(self, tree: dict[str, str]) -> None:
        validate_pai_tree(tree)

    def _killed(self, run_dir: Path) -> bool:
        """The rollout was cut short by a provider failure rather than finishing."""
        events = parse_events(self.trajectory_path(run_dir))
        if not events:
            return False
        last = events[-1]
        return last.get("type") == "error" or (
            last.get("type") == "done" and last.get("status") == "failed"
        )

    def preflight(self) -> None:
        if shutil.which("uv") is None:
            raise RuntimeError("`uv` is not on PATH; WB batches run as `uv run ./scripts/run.sh`")
        if shutil.which("docker") is None:
            raise RuntimeError("`docker` is not on PATH; WorkBuddy runs every task in a container")
        repo = config.wb_repo()
        if not (repo / "scripts" / "run.sh").is_file():
            raise RuntimeError(f"no WorkBuddy-Bench checkout at {repo}; run /harness2 setup wb")
        entry = repo / "configs" / "harnesses" / HARNESS_FAMILY / "versions" / f"{HARNESS_VERSION}.yaml"
        agent = repo / "src" / "workbuddy_bench" / "agents" / "pai_harness_agent.py"
        if not entry.is_file() or not agent.is_file() or not registration_present(repo):
            raise RuntimeError(
                f"the pai harness is not installed in {repo}; run /harness2 setup wb"
            )
        argv = settings.emacs_argv()
        if shutil.which(argv[0]) is None and not Path(argv[0]).is_file():
            raise RuntimeError(f"Emacs not found ({argv[0]})")
