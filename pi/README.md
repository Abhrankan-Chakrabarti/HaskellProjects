### 🧮 Chudnovsky π Calculator (Haskell)

#### 📘 Overview

This Haskell program computes π (pi) to an arbitrary number of digits using the **Chudnovsky algorithm**, a highly efficient formula based on Ramanujan-type series.
It leverages **GMP (GNU Multiple Precision Arithmetic Library)** for high-precision integer arithmetic via Haskell’s FFI interface.

#### ⚙️ Features

* Implements the **binary splitting** method for efficient term computation.
* Uses GMP’s `mpz_sqrt` for precise square root evaluation.
* Prints π to the desired number of digits with correct formatting.
* Modular, recursive design reflecting the Chudnovsky formula structure.

#### 🧩 Algorithm

The Chudnovsky formula for π is:
[
\frac{1}{\pi} = 12 \sum_{k=0}^\infty \frac{(-1)^k (6k)! (13591409 + 545140134k)}{(3k)! (k!)^3 (640320)^{3k + 3/2}}
]

This implementation:

1. Splits the range recursively with `bs`.
2. Computes partial products `(p, q, t)` for each interval.
3. Combines results efficiently using the binary splitting technique.

#### 💻 Usage

Compile and run using:

```bash
ghc -O2 pi_chud.hs -o pi_chud
./pi_chud 1000
```

This will output π to **1000 digits**.

#### 📦 Dependencies

* **GHC (Glasgow Haskell Compiler)**
* **GMP (GNU Multiple Precision Arithmetic Library)**
* **Numeric.GMP Haskell bindings**

#### 🧠 Example Output

```
π to 100 digits is: 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170
```

#### 🪪 Author

**Abhrankan Chakrabarti**

