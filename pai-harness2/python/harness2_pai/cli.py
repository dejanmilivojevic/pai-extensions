"""harness2-pai: the Harness² command line with the pai substrate registered.

    harness2-pai run    --bench jb --k 1 --mode parallel ...   (= harness2 --substrate pai)
    harness2-pai judge  --bench jb --k 1 --mode parallel       (= harness2-judge)
    harness2-pai report --bench jb --k 1 --mode parallel       (= harness2-report)
    harness2-pai check  --bench jb                             preflight, no model calls
    harness2-pai demo   [--output DIR] [--k N] [--improver demo|pai] [--faux FILE]
    harness2-pai render TRAJECTORY.jsonl                       print a rendered trajectory
    harness2-pai status                                        effective configuration
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

from . import register

COMMANDS = ("run", "judge", "report", "check", "demo", "render", "status")


def _with_substrate(argv: list[str]) -> list[str]:
    if any(a == "--substrate" or a.startswith("--substrate=") for a in argv):
        return argv
    return [*argv, "--substrate", "pai"]


def _check(argv: list[str]) -> int:
    p = argparse.ArgumentParser(prog="harness2-pai check", description="pai cell preflight")
    p.add_argument("--bench", default="jb", choices=register.PAI_BENCHES)
    args = p.parse_args(argv)
    from harness2 import config
    from harness2.cli import _common as C

    errors: list[str] = []
    try:
        adapter = C.make_adapter(args.bench, None, "pai")
    except (OSError, RuntimeError, ValueError) as exc:
        print(f"Not ready: {exc}")
        return 1
    for step in (lambda: config.validate_models(judge=True), adapter.tasks, adapter.preflight):
        try:
            step()
        except (OSError, RuntimeError, ValueError) as exc:
            errors.append(str(exc))
    if errors:
        print("Not ready:\n" + "\n".join(f"  - {e}" for e in errors))
        return 1
    print(
        f"Ready: {adapter.spec.name}/pai; {len(adapter.tasks())} tasks; executor "
        f"{adapter.substrate.model}; improver {config.DEFAULT_PROPOSER_CHANNEL}/"
        f"{config.PROPOSER_MODEL_BY_CHANNEL.get(config.DEFAULT_PROPOSER_CHANNEL)}"
    )
    return 0


def _demo(argv: list[str]) -> int:
    p = argparse.ArgumentParser(prog="harness2-pai demo", description=__doc__)
    p.add_argument("--output", type=Path, default=Path("runs/demo-pai"))
    p.add_argument("--k", type=int, default=1)
    p.add_argument("--mode", choices=("parallel", "sequential", "both"), default="parallel")
    p.add_argument(
        "--improver",
        choices=("demo", "pai"),
        default="demo",
        help="demo: scripted editor (no improver cost); pai: pai edits the harness",
    )
    p.add_argument("--faux", type=Path, help="scripted faux pai model responses (tests)")
    args = p.parse_args(argv)
    if args.k < 1:
        p.error("--k must be positive")
    if args.faux:
        os.environ["HARNESS2_PAI_ARGS"] = (
            os.environ.get("HARNESS2_PAI_ARGS", "") + f" --faux {args.faux.resolve()}"
        ).strip()
    from harness2.core.proposer import Proposer
    from harness2.core.schedule import mode_token, run_domain
    from harness2.core.spec import ChannelSpec
    from harness2.demo import DemoProposer

    from . import settings
    from .demo import PaiDemoAdapter

    root = args.output.expanduser().resolve()
    if args.improver == "pai":
        channel = ChannelSpec("pai", settings.improver_model())
        adapter = PaiDemoAdapter(root, channel=channel)
        proposer = Proposer(channel, audit_dir=root / "improver")
    else:
        adapter = PaiDemoAdapter(root)
        proposer = DemoProposer(adapter.spec.channels["proposer"])
    modes = ("agg", "seq") if args.mode == "both" else (mode_token(args.mode),)
    result = run_domain(
        "accounting",
        adapter.tasks(),
        args.k,
        adapter=adapter,
        prop=proposer,
        runs_root=adapter.spec.runs_root,
        library_root=root / "library" / f"k{args.k}",
        modes=modes,
    )
    result["execution_count"] = adapter.execution_count
    result["editor_calls"] = len(proposer._calls)
    report = root / "demo.json"
    report.write_text(json.dumps(result, indent=2, default=str) + "\n", encoding="utf-8")
    print(
        f"pai demo: {result['done']} tasks completed; {result['failed']} failed; "
        f"{adapter.execution_count} pai rollouts. {report}"
    )
    return int(result["failed"] > 0)


def _render(argv: list[str]) -> int:
    p = argparse.ArgumentParser(prog="harness2-pai render")
    p.add_argument("trajectory", type=Path)
    p.add_argument("--obs-trunc", type=int, default=4000)
    args = p.parse_args(argv)
    from .substrate import render_events

    print(render_events(args.trajectory, obs_trunc=args.obs_trunc, max_chars=10_000_000))
    return 0


def main(argv: list[str] | None = None) -> int:
    argv = list(sys.argv[1:] if argv is None else argv)
    if not argv or argv[0] in ("-h", "--help"):
        print(__doc__)
        return 0
    cmd, rest = argv[0], argv[1:]
    if cmd not in COMMANDS:
        print(f"harness2-pai: unknown command {cmd!r}; expected one of {', '.join(COMMANDS)}")
        return 2
    register.install()
    if cmd == "status":
        print(register.describe_json())
        return 0
    if cmd == "render":
        return _render(rest)
    if cmd == "check":
        return _check(rest)
    if cmd == "demo":
        return _demo(rest)
    if cmd == "run":
        from harness2.cli import run_sweep

        return int(run_sweep.main(_with_substrate(rest)) or 0)
    if cmd == "judge":
        from harness2.cli import judge

        return int(judge.main(_with_substrate(rest)) or 0)
    from harness2.cli import report

    return int(report.main(_with_substrate(rest)) or 0)


if __name__ == "__main__":
    raise SystemExit(main())
