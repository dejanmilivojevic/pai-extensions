from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys

import pytest
from harness2 import config
from harness2.benchmarks import workbuddy as WB
from harness2.core import tree as HT

from harness2_pai import workbuddy as PW
from harness2_pai.substrate import PAI_SURFACE


def test_job_yaml_routes_pai_and_keeps_upstream(tmp_path, monkeypatch):
    monkeypatch.setenv("HARNESS2_PAI_THINKING", "high")
    sub = WB.SUBSETS["web"]
    y = WB._job_yaml(sub, "pai", "wb-web-pai-agg1__t__base1", ["t1"], tmp_path / "ws", 2, tmp_path)
    assert "harness: pai-harness/0.1.0" in y
    assert "model: harness2/solver" in y and "model_connection: local_proxy" in y
    assert "n_attempts: 2" in y and "    - t1" in y
    assert f'WB_HARNESS_WS: "{(tmp_path / "ws").resolve()}"' in y
    hp = dict(line.strip().split(": ", 1) for line in y.split("harness_params_override:\n")[1]
              .split("llm_judge_override")[0].strip().splitlines())
    argv = json.loads(json.loads(hp["PAI_EMACS_ARGV"]))
    assert "pai-harness2-batch-main" in argv
    assert "--deny-host" in json.loads(json.loads(hp["PAI_ARGS"]))
    assert json.loads(hp["PAI_THINKING"]) == "high"
    assert "llm_judge_override:\n  model: harness2/judge" in y
    cx = WB._job_yaml(sub, "cx", "wb-web-cx-x", ["t1"], None, 1, tmp_path)
    assert "codex-harness/" in cx and "pai-harness" not in cx


def test_adapter_cell(tmp_path, monkeypatch):
    monkeypatch.setattr(config, "RUNS", tmp_path / "runs")
    a = PW.PaiWbAdapter("office")
    assert a.substrate.name == "pai" and a.spec.install_mode == "pai_root"
    assert a.spec.name == "wb-office"
    assert a._label("wb-office-pai-agg1/t/base1") == "wb-office-pai-agg1__t__base1"
    with pytest.raises(ValueError):
        a._label("wb-office-cx-agg1/t/base1")
    assert a._cell_identity()["solver"] == "pai:harness2/solver"
    tree = HT.read_tree(PAI_SURFACE, a.base_harness())
    HT.validate(PAI_SURFACE, tree)
    trial = tmp_path / "trial"
    (trial / "agent").mkdir(parents=True)
    assert not a._killed(trial)
    (trial / "agent" / "pai-events.jsonl").write_text(
        json.dumps({"type": "done", "status": "failed", "error": "HTTP 500"}) + "\n")
    assert a._killed(trial)
    (trial / "agent" / "pai-events.jsonl").write_text(
        json.dumps({"type": "message", "role": "assistant",
                    "content": [{"type": "text", "text": "x"}]}) + "\n"
        + json.dumps({"type": "done", "status": "success"}) + "\n")
    assert not a._killed(trial)
    assert "[ASSISTANT] x" in a.trajectory(trial, obs_trunc=100)


WB_REPO = config.wb_repo()
WB_PY = WB_REPO / ".venv" / "bin" / "python"


@pytest.mark.skipif(not (WB_REPO / "src").is_dir(), reason="WorkBuddy checkout not installed")
def test_install_assets_registers_the_harness(tmp_path):
    repo = tmp_path / "wb"
    shutil.copytree(WB_REPO / "src", repo / "src")
    (repo / "configs" / "harnesses").mkdir(parents=True)
    first = PW.install_assets(repo)
    second = PW.install_assets(repo)
    assert any("register" in s for s in first)
    assert any("already registered" in s for s in second)
    src = (repo / "src/workbuddy_bench/runner/harness_adapters.py").read_text()
    assert src.count('"pai-harness": HarnessRuntimeAdapter(') == 1
    assert (repo / "src/workbuddy_bench/agents/pai_harness_agent.py").is_file()
    assert (repo / "configs/harnesses/pai-harness/versions/0.1.0.yaml").is_file()
    py = str(WB_PY) if WB_PY.is_file() else sys.executable
    out = subprocess.run(
        [py, "-m", "workbuddy_bench.runner.harness_adapters", "pai-harness"],
        env={**os.environ, "PYTHONPATH": str(repo / "src")},
        capture_output=True, text=True, check=True,
    ).stdout
    assert "HARNESS_NAME='pai-harness'" in out


def test_setup_dry_run_plans_the_pai_harness(capsys):
    from harness2_pai import cli

    assert cli.main(["setup", "wb", "--subset", "office", "--dry-run"]) == 0
    out = capsys.readouterr().out
    assert "fetch-dataset.sh office" in out
    assert "pai_harness_agent.py" in out and "pai-harness" in out
    assert "opencode" not in out.lower() and "codex" not in out.lower()
