# sqrt

A **high-precision square root calculator** implemented in **Haskell** using the **GMP (GNU Multiple Precision Arithmetic Library)** for accurate big-integer computations.

This program computes the square root of a given number up to a specified number of digits of precision.

## ⚙️ Build & Run

Ensure that **GHC** and **GMP** are installed.
To build and execute:

```bash
cd sqrt
ghc -O2 sqrt.hs -o sqrt
./sqrt <n> <digits>
```

Example:

```bash
cd sqrt
ghc -O2 sqrt.hs -o sqrt
./sqrt 2 50
```

Output:

```
√2 to 50 digits is: 1.41421356237309504880168872420969807856967187537694
```

## 🧩 How It Works

* Uses the **GMP function `mpz_sqrt`** for efficient integer square root calculation.
* Wraps GMP operations with **Haskell’s Foreign Function Interface (FFI)** for performance and precision.
* Computes and prints the result up to the specified number of digits.
* The helper functions handle zero-padding and formatted output.

## 🧠 Notes

* The result is computed using integer arithmetic, ensuring high precision.
* Works best for positive integers; inputting very large values showcases GMP’s power.
* Modify output formatting as needed for scientific or fractional display.

## 🪪 Author

**Abhrankan Chakrabarti**

