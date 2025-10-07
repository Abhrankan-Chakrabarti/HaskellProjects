# primes

A **high-precision prime number generator** implemented in **Haskell** using the **GMP (GNU Multiple Precision Arithmetic Library)** for efficient prime computation.

This program finds and prints the next 10 prime numbers greater than a given integer.

## ⚙️ Build & Run

Make sure **GCC**, **GHC**, and **GMP** are installed.
To build and execute:

```bash
cd primes
gcc -c cbits/wrappers.c -o cbits/wrappers.o
ghc -O2 primes.hs cbits/wrappers.o -o primes
./primes <n>
```

Example:

```bash
cd primes
ghc -O2 primes.hs -o primes
./primes 100
```

Output:

```
101
103
107
109
113
127
131
137
139
149
```

## 🧩 How It Works

* Uses `mpz_nextprime` from GMP to find the next prime efficiently.
* Wraps GMP operations with **Haskell FFI** (`Foreign Function Interface`).
* The function `primes n` generates an infinite lazy list of primes greater than `n`.

## 🧠 Notes

* Precision and performance depend on GMP’s optimized big integer arithmetic.
* The program prints the **next 10 primes** by default.
* You can easily modify the `take 10` part in the code to generate more primes.

## 🪪 Author

**Abhrankan Chakrabarti**

