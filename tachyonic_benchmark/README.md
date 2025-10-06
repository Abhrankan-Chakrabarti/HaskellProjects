# tachyonic_benchmark

A **visual and performance benchmarking tool** written in **Haskell**, demonstrating sorting performance and visualization with optional “warp” effects.
It includes both **pure benchmarking mode** and an **animated demo mode**.

## ⚙️ Build & Run

Ensure you have **GHC** installed.
To build and execute:

```bash
cd tachyonic_benchmark
ghc -O2 tachyonic_benchmark.hs -o tachyonic_benchmark
./tachyonic_benchmark [options] <size>
```

### Options:

* `--benchmark <n>`	Run a CPU-time benchmark on a random list of size *n*.
* `--demo <n>`	Run an interactive bubble sort animation on a list of size *n*.
* `--warp <n>`	Same as demo mode but with warp sounds and faster visual feedback.
* `--help`	Show usage instructions.

### Example:

```bash
./tachyonic_benchmark --benchmark 500
```

Output:

```
Tachyonic benchmark initiated!
Starting benchmark: Nebula BubbleSort
Finished in 0.036 seconds.
First 10 sorted elements: [1,2,3,4,5,6,7,8,9,10]
```

## 🧩 How It Works

* Implements both **pure** and **IORef-based** bubble sort for testing performance and animation.
* `tachyonicBubble` animates the sort with colored output, progress bars, and optional sound effects.
* The **warp mode** adds random “sci-fi” sound effects (e.g., *ZOOOM!*, *WHOOSH!*, *KAPOW!*).
* Includes a **tachyonic ASCII banner** at startup.
* `tachyonicBenchmark` measures CPU time using `System.CPUTime`.

## 🧠 Notes

* Use smaller input sizes (≤200) for demo/warp mode for smooth visualization.
* Benchmark mode supports larger sizes (≥1000).
* Requires a terminal that supports ANSI color codes for full visual output.

## 🪪 Author

**Abhrankan Chakrabarti**

