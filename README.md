# HaskellProjects

A collection of classic algorithms, mathematical computations, and benchmarks implemented in **Haskell**.
Each project is self-contained with its own source file(s).

## 📂 Included Projects

* **ackermann** — Ackermann function implementation
* **catalan** — Catalan numbers generator
* **exp** — Exponential function computation
* **fib** — Simple Fibonacci numbers
* **fibgmp** — Arbitrary-precision Fibonacci using GMP
* **fibonacci** — Alternative Fibonacci implementation
* **golden_ratio** — High-precision golden ratio calculation
* **n_queen** — N-Queens solver
* **pi** — High-precision π calculation (GMP)
* **primes** — Prime number utilities
* **sqrt** — High-precision square root calculation
* **tower_of_hanoi** — Tower of Hanoi solver
* **tachyonic_benchmark** — Performance benchmarking
* **tachyonic_void** — Experimental performance test

## 🛠 Build & Run

Make sure you have **GHC** (Glasgow Haskell Compiler) installed.
You can build and run any project like this:

```bash
cd <project-name>
ghc <project-name>.hs -o <project-name>
./<project-name>
```

Example (Ackermann function):

```bash
cd ackermann
ghc ackermann.hs -o ackermann
./ackermann
```

## 📜 License

This repository is licensed under the **MIT License**.

