from __future__ import annotations

import json

import pytest
from harness2 import config

pytestmark = pytest.mark.skipif(
    not (config.lab_repo() / "tasks").is_dir() or not config.LAB_PYTHON.is_file(),
    reason="LAB checkout not installed (/harness2 setup lab)",
)


def _task() -> str:
    from harness2_pai.lab import PaiLabAdapter

    return next(t for t in PaiLabAdapter().tasks() if (config.lab_repo() / "tasks" / t / "documents").is_dir())


def test_lab_rollout_end_to_end(tmp_path, faux, monkeypatch):
    monkeypatch.setattr(config, "RUNS", tmp_path / "runs")
    faux("""
    ((:tool-calls ((:id "c1" :name "bash" :arguments
                    (:command "ls documents >/dev/null && test -r scripts/read_doc.py && echo READER && echo memo > output/memo.md"))))
     (:text "Wrote output/memo.md" :stop-reason stop))
    """)
    from harness2_pai.lab import PaiLabAdapter

    a = PaiLabAdapter()
    assert a.spec.install_mode == "pai_root" and a.substrate.name == "pai"
    tid = _task()
    run_id = f"lab-pai-agg1/{tid}/base1"
    rd = a.execute(tid, None, run_id)
    m = json.loads((rd / "metrics.json").read_text())
    assert m["runner"] == "pai" and m["deliverables"] == ["memo.md"] and m["finished_cleanly"]
    assert m["total_documents"] > 0
    traj = a.trajectory(rd, obs_trunc=1000)
    assert "[OBSERVATION] READER" in traj and "=== ASSIGNMENT ===" in traj
    assert a.deliverable_files(rd) == {"memo.md": "memo\n"}
    assert a.execute(tid, None, run_id) == rd  # reused


def test_lab_validate_tree_keeps_runtime_rules(tmp_path, monkeypatch):
    monkeypatch.setattr(config, "RUNS", tmp_path / "runs")
    from harness2_pai.lab import PaiLabAdapter

    a = PaiLabAdapter()
    a.validate_tree({"AGENTS.md": (a.base_harness() / "AGENTS.md").read_text()})
    with pytest.raises(ValueError, match="under pai"):
        a.validate_tree({"AGENTS.md": "Do the work."})
