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
![\frac{1}{\pi} = 12 \sum_{k=0}^\infty \frac{(-1)^k (6k)! (13591409 + 545140134k)}{(3k)! (k!)^3 (640320)^{3k + 3/2}}](https://latex.codecogs.com/svg.latex?%5Cfrac%7B1%7D%7B%5Cpi%7D%20%3D%2012%20%5Csum_%7Bk%3D0%7D%5E%5Cinfty%20%5Cfrac%7B%28-1%29%5Ek%20%286k%29%21%20%2813591409%20%2B%20545140134k%29%7D%7B%283k%29%21%20%28k%21%29%5E3%20%28640320%29%5E%7B3k%20%2B%203%2F2%7D%7D)

This implementation:

1. Splits the range recursively with `bs`.
2. Computes partial products `(p, q, t)` for each interval.
3. Combines results efficiently using the binary splitting technique.

#### 💻 Usage

Compile and run using:

```bash
gcc -c cbits/wrappers.c -o cbits/wrappers.o
ghc -O2 pi.hs cbits/wrappers.o -o pi
./pi 1000
```

This will output π to **1000 digits**.

#### 📦 Dependencies

* **GCC**
* **GHC (Glasgow Haskell Compiler)**
* **GMP (GNU Multiple Precision Arithmetic Library)**
* **Numeric.GMP Haskell bindings**

#### 🧠 Example Output

```
π to 100 digits is: 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170
```

#### 🪪 Author

**Abhrankan Chakrabarti**

