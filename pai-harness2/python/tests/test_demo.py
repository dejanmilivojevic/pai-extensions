from __future__ import annotations

import json

from harness2_pai import cli


def test_demo_recursion_with_pai_rollouts(tmp_path, faux):
    path = faux("""
    ((:tool-calls ((:id "c1" :name "bash" :arguments
                    (:command "python3 scripts/summarize.py ledger.csv > answer.txt"))))
     (:text "Wrote answer.txt" :stop-reason stop))
    """)
    out = tmp_path / "demo"
    assert cli.main(["demo", "--output", str(out), "--k", "1", "--faux", str(path)]) == 0
    result = json.loads((out / "demo.json").read_text())
    assert result["done"] == 2 and result["failed"] == 0
    assert result["execution_count"] == 8
    answers = {p.parent.name: p.read_text().strip()
               for p in (out / "executions").rglob("ledger-1/*/answer.txt")}
    assert answers["base1"] == "Total: 105.25"     # the base harness's positive-only bug
    assert answers["agg1"] == "Total: 85.15"       # the composed harness fixed it
