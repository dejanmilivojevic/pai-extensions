from __future__ import annotations

import pytest

import harness2_pai
from harness2_pai import register


def test_install_is_idempotent_and_registers_everything():
    harness2_pai.install()
    harness2_pai.install()
    from harness2 import config
    from harness2.cli import _common as C
    from harness2.core import prompts, spec

    assert C.SUBSTRATES.count("pai") == 1
    assert spec.INSTALL_MODES.count("pai_root") == 1
    assert spec.CHANNELS.count("pai") == 1
    assert config.DEFAULT_PROPOSER_CHANNEL == "pai"
    assert config.PROPOSER_MODEL_BY_CHANNEL["pai"] == "faux"
    st = prompts._SURFACE_TEXTS["pai"]
    for text in (st.surface_md, st.inv, st.tts_surface):
        assert ".codex" not in text and ".agents/skills" not in text
    assert ".pai/hooks.json" in st.surface_md and ".pai/skills/" in st.surface_md
    assert "`Stop`" in st.surface_md


def test_make_adapter_dispatches_pai_and_keeps_upstream():
    from harness2.cli import _common as C

    assert type(C.make_adapter("jb", None, "pai")).__name__ == "PaiJobBenchAdapter"
    assert type(C.make_adapter("jb", None, "cx")).__name__ == "JobBenchAdapter"
    with pytest.raises(ValueError, match="supports --bench jb"):
        C.make_adapter("lab", None, "pai")


def test_paiify_rewrites_codex_paths():
    out = register.paiify("see `.codex/hooks.json` and `.agents/skills/x/SKILL.md`")
    assert out == "see `.pai/hooks.json` and `.pai/skills/x/SKILL.md`"


def test_pai_improver_channel_session(tmp_path, faux):
    from harness2.core.proposer import Proposer
    from harness2.core.spec import ChannelSpec

    faux("""
    ((:tool-calls ((:id "c1" :name "bash" :arguments
                    (:command "cat MISSION.md && echo 'edited' > context/harness/AGENTS.md && echo 'ANSWER' > output/answer.md"))))
     (:text "ok" :stop-reason stop))
    """)
    ctx = tmp_path / "ctx"
    (ctx / "harness").mkdir(parents=True)
    (ctx / "harness" / "AGENTS.md").write_text("old\n")
    prop = Proposer(ChannelSpec("pai", "faux"), audit_dir=tmp_path / "audit")
    reply = prop.call("Edit the harness.", ctx, "propose.inv1", harvest="context/harness")
    assert reply.ok and reply.text.strip() == "ANSWER"
    assert reply.channel == "pai"
    assert (reply.workspace / "AGENTS.md").read_text() == "edited\n"
    assert list((tmp_path / "audit" / "pai-sessions").glob("*.jsonl"))
