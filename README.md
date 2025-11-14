## ramu

Implementation of [Hofmann, Martin, and Steffen Jost. "Static prediction of heap space usage for first-order functional programs." ACM SIGPLAN Notices 38.1 (2003): 185-197.](https://dl.acm.org/doi/10.1145/640128.604148) with some extensions.

### Overview

ramu is a first-order static-type functional programming language with an advanced type system that can express and verify heap space usage of programs.

### Quick Start

```bash
git clone https://github.com/abap34/ramu.git
cd ramu
# execution with 100 heap cells
julia --project="." -m src/ramu.jl examples/list_length.ramu 100
# Execution Finished.
# Final Value: ramu.LocVal(3)
# ========================================
# Final Stack:
#   xs => ramu.NilVal()
# ========================================
# Final Heap:
#   2 => ramu.PairVal(ramu.UnitVal(), ramu.LocVal(1))
#   3 => ramu.PairVal(ramu.UnitVal(), ramu.LocVal(2))
#   1 => ramu.PairVal(ramu.UnitVal(), ramu.NilVal())
```
