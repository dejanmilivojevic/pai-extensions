from __future__ import annotations

from harness2_pai.jobbench import PaiJobBenchAdapter


def test_rollout_end_to_end(tmp_path, faux):
    faux("""
    ((:tool-calls ((:id "c1" :name "bash" :arguments
                    (:command "cat task_folder/TASK_INSTRUCTIONS.txt && echo memo > output/memo.md"))))
     (:text "Saved output/memo.md" :stop-reason stop))
    """)
    store = tmp_path / "store"
    task = store / "accountant" / "task1" / "task_folder"
    task.mkdir(parents=True)
    (task / "TASK_INSTRUCTIONS.txt").write_text("Write a memo.\n")
    a = PaiJobBenchAdapter(store_root=store)
    assert a.spec.install_mode == "pai_root" and a.substrate.name == "pai"
    run_id = "jb-pai-agg1/accountant/task1/base1"
    rd = a.execute("accountant/task1", None, run_id)
    assert (rd / "memo.md").read_text() == "memo\n"
    assert a.delivered(rd) and a.artifacts_intact(rd)
    traj = a.trajectory(rd, obs_trunc=2000)
    assert "[ACTION] bash: cat task_folder/TASK_INSTRUCTIONS.txt" in traj
    assert "[OBSERVATION] Write a memo." in traj
    index = next((store / "accountant/task1/model_traj").rglob("attempt_index.tsv")).read_text()
    assert "\tsuccess\t0\t" in index
    assert a.execute("accountant/task1", None, run_id) == rd  # reused, not re-rolled
    assert not any((tmp_path / "workspaces").iterdir())


def test_rollout_without_deliverables_stays_unmarked(tmp_path, faux):
    import pytest

    faux('((:text "I did nothing" :stop-reason stop))')
    store = tmp_path / "store"
    task = store / "accountant" / "task2" / "task_folder"
    task.mkdir(parents=True)
    (task / "TASK_INSTRUCTIONS.txt").write_text("Write a memo.\n")
    a = PaiJobBenchAdapter(store_root=store)
    with pytest.raises(RuntimeError, match="produced no output"):
        a.execute("accountant/task2", None, "jb-pai-agg1/accountant/task2/base1")
