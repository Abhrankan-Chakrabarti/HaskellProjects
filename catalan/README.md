# Catalan Numbers

Generates **Catalan numbers**, a sequence of natural numbers that appear in various combinatorial problems, such as counting binary trees, parenthesizations, and lattice paths.

## 🛠 Build & Run

Make sure you have **GHC** (Glasgow Haskell Compiler) installed.

```bash
cd catalan
ghc catalan.hs -o catalan
./catalan 5      # Linux/macOS
.\catalan.exe 5  # Windows
```

## ⚙ Command-line Arguments

This program expects **one integer argument** `n` to compute the nth Catalan number.

Example:

```bash
./catalan 5       # Linux/macOS
.\catalan.exe 5   # Windows
```

## 📋 Expected Output

The program prints `Cat(n)` for the input `n`.

Example output:

```
Cat(5) = 42
```

## 🔔 Notes

* The program uses a simple recursive formula; large `n` may take longer to compute.
* For very large `n`, consider optimizing or using memoization to improve performance.

## 📜 License

This project is licensed under the **MIT License**.

