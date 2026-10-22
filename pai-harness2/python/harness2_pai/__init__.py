"""pai substrate for Harness² (google-research/harness2).

`harness2_pai.register.install()` registers `--substrate pai` and the `pai` improver
channel with an installed Harness²; `harness2-pai` is its command line.
"""

from .register import install

__all__ = ["install"]
