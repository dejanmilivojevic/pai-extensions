"""The WorkBuddy Harbor agent, run for real (under the WorkBuddy venv, which has Harbor):
a fake Harbor environment, a fake `docker' for TRAMP, pai's faux model."""

from __future__ import annotations

import asyncio
import json
import os
import shutil
import sys
import textwrap
from pathlib import Path
from types import SimpleNamespace

import pytest

pytest.importorskip("harbor")
pytest.importorskip("workbuddy_bench")

AGENTS = Path(__file__).resolve().parents[1] / "harness2_pai" / "wb_runner" / "agents"
sys.path.insert(0, str(AGENTS))
import pai_harness_agent as PA  # noqa: E402

from harbor.models.agent.context import AgentContext  # noqa: E402

ROOT = Path(__file__).resolve().parents[4]   # the pai checkout
EXT = Path(__file__).resolve().parents[2]


def emacs_argv() -> list[str]:
    load = [ROOT / "lisp", ROOT / "vendor" / "vui", ROOT / "test", EXT]
    argv = ["emacs", "-Q", "--batch", "--eval", "(setq load-prefer-newer t)"]
    for d in load:
        argv += ["-L", str(d)]
    return argv + ["-l", "pai-harness2-batch", "-f", "pai-harness2-batch-main"]


class FakeEnv:
    """Harbor's local Docker environment, minus Docker: commands run on this host."""

    default_user = None

    def __init__(self):
        self.commands = []

    async def exec(self, command, cwd=None, env=None, timeout_sec=None, user=None):
        self.commands.append(command)
        p = await asyncio.create_subprocess_exec(
            "bash", "-c", command, stdout=asyncio.subprocess.PIPE, stderr=asyncio.subprocess.PIPE)
        out, err = await p.communicate()
        return SimpleNamespace(return_code=p.returncode, stdout=out.decode(), stderr=err.decode())

    async def _run_docker_compose_command(self, args):
        assert args == ["ps", "-q", "main"]
        return SimpleNamespace(return_code=0, stdout="box\n", stderr="")


@pytest.fixture
def fake_docker(tmp_path, monkeypatch):
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    docker = bin_dir / "docker"
    docker.write_text(textwrap.dedent(f"""\
        #!/bin/sh
        echo "$@" >> {tmp_path}/docker.log
        [ "$1" = exec ] || exit 1; shift
        while [ $# -gt 0 ]; do case "$1" in -u) shift 2;; -*) shift;; *) break;; esac; done
        shift
        exec "$@"
        """))
    docker.chmod(0o755)
    monkeypatch.setenv("PATH", f"{bin_dir}:{os.environ['PATH']}")
    monkeypatch.setenv("HOME", str(tmp_path / "home"))
    return tmp_path / "docker.log"


def test_helpers():
    assert PA.host_url("http://host.docker.internal:3456") == "http://127.0.0.1:3456/v1"
    assert PA.host_url("http://host.docker.internal:3456/v1/chat/completions") == \
        "http://127.0.0.1:3456/v1"
    assert PA.tramp_dir("abc", "dev") == "/docker:dev@abc:/workspace/"
    assert PA.tramp_dir("abc", "root") == "/docker:abc:/workspace/"


def test_agent_runs_pai_in_the_container_over_tramp(tmp_path, fake_docker, monkeypatch):
    if shutil.which("emacs") is None:
        pytest.skip("no emacs")
    ws = tmp_path / "workspace"
    ws.mkdir()
    monkeypatch.setattr(PA, "WORKDIR", str(ws))
    harness = tmp_path / "harness"
    (harness / ".pai").mkdir(parents=True)
    (harness / "AGENTS.md").write_text("WB-HARNESS-RULE\n")
    (harness / ".pai" / "hooks.json").write_text(json.dumps(
        {"hooks": {"PostToolUse": [{"hooks": [{"type": "command", "command": "touch hooked"}]}]}}))
    faux = tmp_path / "faux.eld"
    faux.write_text("""((:tool-calls ((:id "c1" :name "bash" :arguments (:command "echo built > page.html; pwd"))))
 (:text "Built page.html" :stop-reason stop :usage (:input 7 :output 3 :total-tokens 10)))""")
    logs = tmp_path / "trial" / "agent"
    agent = PA.PaiHarnessAgent(
        logs,
        model_name="harness2/solver",
        WB_HARNESS_WS=str(harness),
        PAI_EMACS_ARGV=json.dumps(emacs_argv()),
        PAI_TIMEOUT=60,
        PAI_ARGS=json.dumps(["--faux", str(faux)]),
        PAI_VERSION="0.1.0",
        connection={"mode": "local_proxy", "proxy_url": "http://host.docker.internal:3456"},
    )
    assert agent.name() == "pai-harness"
    argv = agent.argv("/docker:box:/w/", Path("/p"), Path("/e"))
    assert argv[argv.index("--endpoint") + 1] == "http://127.0.0.1:3456/v1"
    assert argv[argv.index("--endpoint-key") + 1] == "trial::harness2/solver"
    env = FakeEnv()
    asyncio.run(agent.run("Build a page.", env, AgentContext()))
    assert (ws / "AGENTS.md").read_text() == "WB-HARNESS-RULE\n"   # harness installed
    assert (ws / "page.html").read_text() == "built\n"              # tool ran "in the container"
    assert (ws / "hooked").exists()                                  # so did the hook
    assert "exec" in fake_docker.read_text()                         # through docker (TRAMP)
    events = PA.read_events(logs / PA.EVENTS_FILENAME)
    assert events[-1]["type"] == "done" and events[-1]["status"] == "success"
    session = events[0]
    assert "WB-HARNESS-RULE" in session["system_prompt"]
    assert session["cwd"] == f"/docker:box:{ws}/"
    ctx = AgentContext()
    agent.populate_context_post_run(ctx)
    traj = json.loads((logs / "trajectory.json").read_text())
    assert len(traj["steps"]) == 2
    assert traj["steps"][0]["tool_calls"][0]["function_name"] == "bash"
    assert ctx.n_input_tokens == 17 and ctx.n_output_tokens == 8
