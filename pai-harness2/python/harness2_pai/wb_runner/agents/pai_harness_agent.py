"""PaiHarnessAgent -- pai (the Emacs agent) as a WorkBuddy-Bench / Harbor harness.

Unlike the OpenCode and Codex harnesses, nothing is installed into the task container:
pai runs on the HOST, in a child Emacs, and its tools act inside the container through
TRAMP's built-in `docker' method -- the workspace is /docker:<user>@<container>:/workspace/.
So bash commands, hooks and file edits happen in the container (under its egress
control), exactly where OpenCode's or Codex's would, while the model is reached from the
host: through the job-local bench proxy under `local_proxy' (the same route and logging
as the other harnesses), or with pai's own provider when PAI_MODEL is set.

The harness workspace (AGENTS.md, .pai/, scripts/) is installed at /workspace and
committed pre-agent, as the Codex harness agent does, so it stays out of the diff.
The pai event stream lands in /logs/agent/pai-events.jsonl and is mapped to ATIF.

Installed into the WorkBuddy checkout by `harness2-pai setup wb`.
"""

from __future__ import annotations

import asyncio
import base64
import io
import json
import os
import shlex
import signal
import tarfile
import tempfile
from pathlib import Path

from harbor.agents.installed.base import BaseInstalledAgent
from harbor.environments.base import BaseEnvironment
from harbor.models.agent.context import AgentContext
from harbor.models.trajectories.agent import Agent
from harbor.models.trajectories.final_metrics import FinalMetrics
from harbor.models.trajectories.metrics import Metrics
from harbor.models.trajectories.observation import Observation
from harbor.models.trajectories.observation_result import ObservationResult
from harbor.models.trajectories.step import Step
from harbor.models.trajectories.tool_call import ToolCall
from harbor.models.trajectories.trajectory import Trajectory

from workbuddy_bench.agents._agent_user import ensure_agent_user

EVENTS_FILENAME = "pai-events.jsonl"
STDERR_FILENAME = "pai-stderr.log"
HARNESS_ENTRIES = ("AGENTS.md", ".pai", "scripts")
WORKDIR = "/workspace"


def host_url(url: str) -> str:
    """The bench proxy as seen from the host: the URL is the in-container one."""
    u = (url or "").strip().rstrip("/")
    for alias in ("host.docker.internal", "gateway.docker.internal"):
        u = u.replace(f"//{alias}", "//127.0.0.1")
    if u.endswith("/chat/completions"):
        u = u[: -len("/chat/completions")]
    if u and not u.endswith("/v1"):
        u += "/v1"
    return u


def tramp_dir(container: str, user: str | int | None) -> str:
    """The TRAMP name of the container's /workspace for USER."""
    who = "" if user in (None, "", "root", 0, "0") else f"{user}@"
    return f"/docker:{who}{container}:{WORKDIR}/"


def harness_payload(ws: Path) -> tuple[bytes, int]:
    """gzip tar of the pai-root entries present in harness workspace WS."""
    buf = io.BytesIO()
    n = 0
    with tarfile.open(fileobj=buf, mode="w:gz") as tar:
        for entry in HARNESS_ENTRIES:
            src = ws / entry
            if src.is_file():
                tar.add(src, arcname=entry)
                n += 1
            elif src.is_dir():
                for p in sorted(src.rglob("*")):
                    if p.is_file():
                        tar.add(p, arcname=f"{entry}/{p.relative_to(src)}")
                        n += 1
    return buf.getvalue(), n


def read_events(path: Path) -> list[dict]:
    events: list[dict] = []
    try:
        for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
            line = line.strip()
            if not line.startswith("{"):
                continue
            try:
                ev = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(ev, dict):
                events.append(ev)
    except OSError:
        return []
    return events


def build_trajectory(events: list[dict], *, model_name: str, version: str) -> Trajectory | None:
    """Map a pai event stream to ATIF: one Step per assistant message, its tool results
    as the Step's observation, totals from the final `done` event."""
    steps: list[Step] = []
    results: dict[str, str] = {}
    errors: list[str] = []
    session_id = None
    done: dict = {}
    for ev in events:
        t = ev.get("type")
        if t == "session":
            session_id = ev.get("session_id")
        elif t == "tool_result":
            results[str(ev.get("id"))] = str(ev.get("output") or "")
        elif t == "error":
            errors.append(str(ev.get("message")))
        elif t == "done":
            done = ev
    for ev in events:
        if not (ev.get("type") == "message" and ev.get("role") == "assistant"):
            continue
        content = ev.get("content") or []
        text = "\n".join(b.get("text") or "" for b in content if b.get("type") == "text")
        reasoning = "\n".join(b.get("text") or "" for b in content if b.get("type") == "thinking")
        calls = [
            ToolCall(
                tool_call_id=str(b.get("id") or ""),
                function_name=str(b.get("name") or ""),
                arguments=b.get("arguments") if isinstance(b.get("arguments"), dict) else {},
            )
            for b in content
            if b.get("type") == "tool_call"
        ]
        usage = ev.get("usage") or {}
        obs = [
            ObservationResult(source_call_id=c.tool_call_id, content=results[c.tool_call_id])
            for c in calls
            if c.tool_call_id in results
        ]
        steps.append(
            Step(
                step_id=len(steps) + 1,
                source="agent",
                model_name=ev.get("model") or model_name,
                message=text,
                reasoning_content=reasoning or None,
                tool_calls=calls or None,
                observation=Observation(results=obs) if obs else None,
                metrics=Metrics(
                    prompt_tokens=int(usage.get("input") or 0),
                    completion_tokens=int(usage.get("output") or 0),
                    cached_tokens=int(usage.get("cache_read") or 0),
                ),
                llm_call_count=1,
            )
        )
        if ev.get("stop_reason") in ("error", "aborted") and ev.get("error"):
            errors.append(str(ev["error"]))
    if not steps and not errors and not done:
        return None
    if not steps:
        steps.append(
            Step(
                step_id=1,
                source="agent",
                model_name=model_name,
                message="run produced no steps; " + "; ".join(errors[:3] or [str(done.get("error"))]),
                llm_call_count=0,
            )
        )
    usage = done.get("usage") or {}
    cost = done.get("cost_usd")
    return Trajectory(
        session_id=session_id,
        agent=Agent(name="pai", version=version, model_name=model_name),
        steps=steps,
        final_metrics=FinalMetrics(
            total_prompt_tokens=int(usage.get("input") or 0),
            total_completion_tokens=int(usage.get("output") or 0),
            total_cached_tokens=int(usage.get("cache_read") or 0),
            total_cost_usd=float(cost) if isinstance(cost, (int, float)) and cost else None,
            total_steps=len(steps),
            extra={"status": done.get("status"), "errors": errors or None},
        ),
        notes="pai event stream (pai-events.jsonl): one Step per assistant message; "
        "totals from the run's final `done` event.",
    )


class PaiHarnessAgent(BaseInstalledAgent):
    """pai on the host, working in the task container over TRAMP."""

    SUPPORTS_ATIF: bool = True

    def __init__(self, logs_dir: Path, *args, **kwargs):
        ws = kwargs.pop("WB_HARNESS_WS", None)
        self._harness_ws: Path | None = Path(ws) if ws else None
        argv = kwargs.pop("PAI_EMACS_ARGV", None)
        self._emacs_argv: list[str] = json.loads(argv) if isinstance(argv, str) else list(argv or [])
        self._pai_model = str(kwargs.pop("PAI_MODEL", "") or "")
        self._thinking = str(kwargs.pop("PAI_THINKING", "") or "")
        self._timeout = int(kwargs.pop("PAI_TIMEOUT", 0) or 0)
        extra = kwargs.pop("PAI_ARGS", None)
        self._extra_args: list[str] = (
            json.loads(extra) if isinstance(extra, str) and extra.startswith("[")
            else shlex.split(extra) if isinstance(extra, str) else list(extra or [])
        )
        self._pai_version = str(kwargs.pop("PAI_VERSION", "") or "unknown")
        connection = kwargs.pop("connection", None) or {}
        self._conn_mode = str(connection.get("mode") or "direct")
        self._proxy_url = str(connection.get("proxy_url") or "")
        for k in ("model_params", "context_window", "context_compact_pct", "settings_preset",
                  "models_preset", "mount_path", "instance_id"):
            kwargs.pop(k, None)
        self._trial_id = logs_dir.parent.name if Path(logs_dir).name == "agent" else ""
        super().__init__(logs_dir, *args, version=self._pai_version, **kwargs)

    @staticmethod
    def name() -> str:
        return "pai-harness"

    def get_version_command(self) -> str | None:
        return None

    async def install(self, environment: BaseEnvironment) -> None:
        """Nothing goes into the container but the agent user: pai stays on the host."""
        await ensure_agent_user(self, environment)

    async def _container_id(self, environment: BaseEnvironment) -> str:
        getter = getattr(environment, "_run_docker_compose_command", None)
        if getter is None:
            raise RuntimeError(
                "pai-harness needs Harbor's local Docker environment (harness_backend: local)"
            )
        result = await getter(["ps", "-q", "main"])
        cid = (result.stdout or "").strip().splitlines()
        if not cid:
            raise RuntimeError("could not find the task container (docker compose ps -q main)")
        return cid[-1].strip()

    async def _materialize_harness(self, environment: BaseEnvironment) -> int:
        ws = self._harness_ws
        if ws is None or not ws.is_dir():
            return 0
        blob, n = harness_payload(ws)
        if n == 0:
            return 0
        b64 = base64.b64encode(blob).decode()
        await self.exec_as_agent(
            environment,
            command=(
                f"cd {WORKDIR} && echo {b64} | base64 -d | tar -xzf - -C {WORKDIR} && "
                "if git rev-parse --git-dir >/dev/null 2>&1; then "
                "git add -A && { git diff --cached --quiet || "
                "git -c user.email=harness@bench -c user.name=harness "
                'commit -q -m "harness install (pre-agent)"; }; fi'
            ),
        )
        return n

    def _model_args(self) -> list[str]:
        if self._pai_model:
            return ["--model", self._pai_model]
        if self._conn_mode != "local_proxy" or not self._proxy_url:
            raise RuntimeError(
                "pai-harness needs model_connection: local_proxy (the bench proxy route) "
                "or PAI_MODEL (a pai model id) in the harness params"
            )
        route = self.model_name
        key = f"{self._trial_id}::{route}" if self._trial_id else route
        return ["--model", f"endpoint/{route}", "--endpoint", host_url(self._proxy_url),
                "--endpoint-key", key]

    def argv(self, workspace: str, prompt_file: Path, events: Path) -> list[str]:
        if not self._emacs_argv:
            raise RuntimeError("PAI_EMACS_ARGV is not set; the job must pass it (harness2-pai)")
        argv = [*self._emacs_argv, "--cwd", workspace, "--prompt-file", str(prompt_file),
                "--events", str(events), *self._model_args()]
        if self._timeout:
            argv += ["--timeout", str(self._timeout)]
        if self._thinking:
            argv += ["--thinking", self._thinking]
        return argv + self._extra_args

    async def run(
        self, instruction: str, environment: BaseEnvironment, context: AgentContext
    ) -> None:
        instruction = self.render_instruction(instruction)
        await self._materialize_harness(environment)
        container = await self._container_id(environment)
        workspace = tramp_dir(container, getattr(environment, "default_user", None))
        self.logs_dir.mkdir(parents=True, exist_ok=True)
        events = self.logs_dir / EVENTS_FILENAME
        fd, prompt = tempfile.mkstemp(prefix="pai-wb-prompt-", suffix=".txt")
        with os.fdopen(fd, "w", encoding="utf-8") as fh:
            fh.write(instruction)
        try:
            with open(self.logs_dir / STDERR_FILENAME, "w", encoding="utf-8") as log:
                proc = await asyncio.create_subprocess_exec(
                    *self.argv(workspace, Path(prompt), events),
                    stdin=asyncio.subprocess.DEVNULL,
                    stdout=log,
                    stderr=log,
                    start_new_session=True,
                )
                try:
                    rc = await proc.wait()
                except BaseException:
                    # Trial timeout or cancellation: take the whole Emacs group down.
                    try:
                        os.killpg(proc.pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass
                    raise
            if rc not in (0, 124):
                self.logger.warning("pai rollout exited with %s; see %s", rc, STDERR_FILENAME)
        finally:
            os.unlink(prompt)

    def populate_context_post_run(self, context: AgentContext) -> None:
        events = read_events(self.logs_dir / EVENTS_FILENAME)
        if not events:
            return
        traj = build_trajectory(events, model_name=self.model_name or "", version=self._pai_version)
        if traj is None:
            return
        try:
            with open(self.logs_dir / "trajectory.json", "w", encoding="utf-8") as fh:
                json.dump(traj.to_json_dict(), fh, indent=2, ensure_ascii=False)
        except (OSError, AttributeError) as exc:
            self.logger.debug("Failed writing trajectory.json: %s", exc)
        fm = traj.final_metrics
        if fm:
            context.n_input_tokens = fm.total_prompt_tokens or 0
            context.n_output_tokens = fm.total_completion_tokens or 0
            context.n_cache_tokens = fm.total_cached_tokens or 0
            if fm.total_cost_usd is not None:
                context.cost_usd = fm.total_cost_usd
