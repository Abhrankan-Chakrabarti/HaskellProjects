# Ackermann Function

Implements the classic **Ackermann function**, a recursive mathematical function that grows rapidly and demonstrates deep recursion in Haskell.

## 🛠 Build & Run

Make sure you have **GHC** (Glasgow Haskell Compiler) installed.

```bash
cd ackermann
ghc ackermann.hs -o ackermann
./ackermann 3 4      # Linux/macOS
.\ackermann.exe 3 4  # Windows
```

## ⚙ Command-line Arguments

This program expects **two integer arguments** `m` and `n` corresponding to `A(m, n)` in the Ackermann function.

Example:

```bash
./ackermann 3 4      # Linux/macOS
.\ackermann.exe 3 4  # Windows
```

## 📋 Expected Output

The program prints the result of `A(m, n)`.

Example output:

```
A(3, 4) = 125
```

## 🔔 Notes

* Large inputs can cause stack overflow due to deep recursion.
* Recommended to use small integer values for testing.

## 📜 License

This project is licensed under the **MIT License**.

