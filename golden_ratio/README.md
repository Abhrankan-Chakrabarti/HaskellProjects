# Golden Ratio Calculation (High Precision)

Computes the **golden ratio (ϕ)** to a specified number of decimal digits using arbitrary-precision integer arithmetic via GMP.

## 🛠 Build & Run

Make sure you have **GHC** (Glasgow Haskell Compiler) and GMP installed.

```bash
cd golden_ratio
ghc golden_ratio.hs -o golden_ratio
./golden_ratio <digits>      # Linux/macOS
.\golden_ratio.exe <digits>  # Windows
```

Example:

```bash
./golden_ratio 50      # Compute ϕ to 50 digits
```

## ⚙ Command-line Arguments

* `[digits]` — number of decimal digits to compute (required)

Example run:

```
./golden_ratio 50
```

## 📋 Expected Output

The program prints the golden ratio to the specified number of digits:

```
ϕ to 50 digits is: 1.61803398874989484820458683436563811772030917980576
```

## 🔔 Notes

* Uses GMP via Haskell FFI for high-precision integer arithmetic.
* Handles very large numbers for arbitrary decimal precision.
* Output is formatted to fill trailing zeros correctly.

## 📜 License

This project is licensed under the **MIT License**.

