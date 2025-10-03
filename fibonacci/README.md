# Fibonacci Numbers (Alternative Implementation)

Computes **Fibonacci numbers** using a recursive algorithm with optimizations for even/odd indices. Supports interactive input, command-line arguments, and optional timing of computation.

## 🛠 Build & Run

Make sure you have **GHC** (Glasgow Haskell Compiler) installed.

```bash
cd fibonacci
ghc fibonacci.hs -o fibonacci
./fibonacci          # Linux/macOS
.\fibonacci.exe     # Windows
```

## ⚙ Command-line Arguments

* `--print` — prints the Fibonacci number
* `[n]` — optional number argument; if not provided, program prompts for input

Example run:

```bash
./fibonacci --print 50      # Linux/macOS
.\fibonacci.exe --print 50  # Windows
```

Or interactively:

```
./fibonacci --print
Enter n: 50
```

## 📋 Expected Output

The program prints the Fibonacci number and computation time.

Example:

```
F(50) = 12586269025
Elapsed time: 0.000123 sec
```

## 🔔 Notes

* Supports decimal, scientific (e.g., `1e3`), and hexadecimal (e.g., `0xFF`) inputs.
* Optimized recursive computation for negative and even/odd indices.
* Timing is provided using CPU time for reference.

## 📜 License

This project is licensed under the **MIT License**.

