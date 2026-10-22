from __future__ import annotations

import json
from pathlib import Path

import pytest
from harness2.core import tree as HT
from harness2.core.spec import BenchPrompts, BenchSpec, ChannelSpec

from harness2_pai.substrate import (
    INSTALL_MODE,
    PAI_SURFACE,
    PaiSubstrate,
    final_message,
    install_tree,
    render_events,
    validate_pai_tree,
)


def _spec(tmp_path, mode=INSTALL_MODE):
    ch = ChannelSpec("pai", "faux")
    return BenchSpec(
        name="t",
        runs_root=tmp_path,
        install_mode=mode,
        prompts=BenchPrompts(work="w"),
        channels={"proposer": ch, "selector": ch},
    )


def _tree(tmp_path) -> Path:
    t = {
        "AGENTS.md": "Do it.\n- When tables, read .pai/skills/fix-tables/SKILL.md and follow it.\n",
        ".pai/skills/fix-tables/SKILL.md": "---\ndescription: fix tables\n---\nbody\n",
        ".pai/skills/fix-tables/scripts/t.py": "print(1)\n",
        "scripts/sum.py": "print(2)\n",
        "sub_agents/reader/AGENT.md": "---\ndescription: reads big files\n---\nRead.\n",
        ".pai/hooks.json": json.dumps(
            {"hooks": {"PostToolUse": [{"matcher": "bash", "hooks": [
                {"type": "command", "command": "true"}]}]}}
        ),
    }
    d = tmp_path / "tree"
    HT.write_tree(PAI_SURFACE, t, d, stubs=False)
    return d


def test_surface_accepts_every_component(tmp_path):
    d = _tree(tmp_path)
    tree = HT.read_tree(PAI_SURFACE, d)
    census = HT.validate(PAI_SURFACE, tree)
    assert census["hooks"] == 1
    assert HT.skill_names(PAI_SURFACE, tree) == ["fix-tables"]
    assert HT.subagent_names(PAI_SURFACE, tree) == ["reader"]


def test_materialise_lays_out_pai_files(tmp_path):
    install = PaiSubstrate().materialise(_tree(tmp_path), _spec(tmp_path), {"file:extra.txt": "x"})
    s = install.staging_dir
    assert (s / "AGENTS.md").is_file()
    assert (s / ".pai/skills/fix-tables/SKILL.md").is_file()
    assert (s / ".pai/hooks.json").is_file()
    assert (s / "scripts/sum.py").is_file()
    role = (s / ".pai/subagents/reader.md").read_text()
    assert role.startswith("---\nname: reader\n")
    assert "description: reads big files" in role
    assert not (s / "sub_agents").exists()
    assert (s / "extra.txt").read_text() == "x"
    ws = tmp_path / "ws"
    ws.mkdir()
    install_tree(s, ws)
    assert (ws / ".pai/subagents/reader.md").is_file()


def test_materialise_rejects_other_modes(tmp_path):
    with pytest.raises(ValueError, match="pai_root"):
        PaiSubstrate().materialise(_tree(tmp_path), _spec(tmp_path, "codex_root"), {})


def test_validate_rejects_names_pai_would_drop():
    with pytest.raises(ValueError, match="not valid for pai"):
        validate_pai_tree({"AGENTS.md": "x", ".pai/skills/bad_name/SKILL.md": "---\n"})
    validate_pai_tree({"AGENTS.md": "x", ".pai/skills/good-name/SKILL.md": "---\n"})


def test_render_events_grammar(tmp_path):
    events = [
        {"type": "session", "system_prompt": "<project_context>\nRULES\n</project_context>",
         "hooks": ["Stop"]},
        {"type": "message", "role": "user", "text": "task"},
        {"type": "message", "role": "assistant", "content": [
            {"type": "thinking", "text": "hmm"}, {"type": "text", "text": "plan"},
            {"type": "tool_call", "id": "1", "name": "bash", "arguments": {"command": "ls"}}]},
        {"type": "tool_start", "id": "1", "name": "bash", "arguments": {"command": "ls"}},
        {"type": "tool_result", "id": "1", "name": "bash", "is_error": True, "output": "boom"},
        {"type": "hook", "event": "Stop", "continue": True, "reason": "write it"},
        {"type": "done", "status": "success", "final": "bye", "wall_s": 3,
         "usage": {"input": 1, "output": 2}, "cost_usd": 0.5},
    ]
    p = tmp_path / "e.jsonl"
    p.write_text("junk\n" + "\n".join(json.dumps(e) for e in events) + "\n")
    out = render_events(p, obs_trunc=100, max_chars=10_000)
    assert out.splitlines() == [
        "[HARNESS AGENTS.md] RULES",
        "[HARNESS] hooks active: Stop",
        "[USER] task",
        "[REASONING] hmm",
        "[ASSISTANT] plan",
        "[ACTION] bash: ls",
        "[OBSERVATION ERROR] boom",
        "[HARNESS] Stop hook sent the agent back: write it",
        "[HARNESS] run success after 3s | tokens in=1 out=2 cache_read=None | cost_usd=0.5",
    ]
    assert final_message(p) == "bye"
    assert "render truncation" in render_events(p, obs_trunc=100, max_chars=20)


def test_real_rollout_runs_harness_files(tmp_path, faux):
    faux("""
    ((:tool-calls ((:id "c1" :name "bash" :arguments (:command "python3 scripts/sum.py > out.txt"))))
     (:text "done" :stop-reason stop))
    """)
    install = PaiSubstrate().materialise(_tree(tmp_path), _spec(tmp_path), {})
    ws = tmp_path / "ws"
    ws.mkdir()
    install_tree(install.staging_dir, ws)
    events = tmp_path / "traj" / "e.jsonl"
    rc = PaiSubstrate().run("go", ws, events, 60, log=tmp_path / "log")
    assert rc == 0, (tmp_path / "log").read_text()
    assert (ws / "out.txt").read_text() == "2\n"
    text = render_events(events, obs_trunc=1000, max_chars=100_000)
    assert "[HARNESS AGENTS.md]" in text and "Do it." in text
    assert "[HARNESS] hooks active: PostToolUse" in text
    assert "[HARNESS] hook PostToolUse: true | exit=0" in text
    assert "[ACTION] bash: python3 scripts/sum.py > out.txt" in text
    assert final_message(events) == "done"


def test_preflight_needs_a_model(monkeypatch):
    monkeypatch.delenv("HARNESS2_PAI_MODEL")
    monkeypatch.delenv("HARNESS2_SOLVER_MODEL", raising=False)
    with pytest.raises(RuntimeError, match="HARNESS2_PAI_MODEL"):
        PaiSubstrate().preflight()
