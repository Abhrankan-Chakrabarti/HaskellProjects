# Exponential Function (exp)

Computes **e^x** with a specified number of digits of precision using a binary splitting method for high-precision calculation.

## 🛠 Build & Run

Make sure you have **GHC** (Glasgow Haskell Compiler) installed.

```bash
cd exp
ghc exp.hs -o exp
./exp      # Linux/macOS
.\exp.exe  # Windows
```

## ⚙ Command-line Arguments

This program **interactively asks for input** rather than command-line arguments.

1. Enter the value of `x` (the exponent).
2. Enter the number of digits of precision.

Example run:

```bash
./exp      # Linux/macOS
.\exp.exe  # Windows
```

```
Enter the value of x (exponent): 2
Enter the number of digits of precision: 10
```

## 📋 Expected Output

The program prints the value of `e^x` to the requested number of digits.

Example output:

```
e^2 to 10 digits is: 7.389056098
```

## 🔔 Notes

* The program uses a **binary splitting algorithm** for high-precision computation.
* Ensure reasonable input values for `x` and digits to avoid long computation times.
* Input validation is included to prevent invalid entries.

## 📜 License

This project is licensed under the **MIT License**.

