# Fibonacci & Lucas Numbers

Computes **Fibonacci and Lucas numbers** for a given integer input, supporting decimal, scientific, and hexadecimal formats. Includes optional timing of computation.

## 🛠 Build & Run

Make sure you have **GHC** (Glasgow Haskell Compiler) installed.

```bash
cd fib
ghc fib.hs -o fib
./fib      # Linux/macOS
.\fib.exe  # Windows
```

## ⚙ Command-line Arguments

The program supports optional arguments to control output:

* `--fib` — print only the Fibonacci number
* `--lucas` — print only the Lucas number
* `--print` — print both Fibonacci and Lucas numbers

Example run:

```bash
./fib --print      # Linux/macOS
.\fib.exe --print   # Windows
```

The program will then prompt for a number.

## 📋 Expected Output

After entering a number, the program prints the requested sequence(s) along with computation time.

Example:

```
This program computes the Fibonacci and Lucas numbers of a given integer.
Enter a number: 10
F(10) = 55
L(10) = 123
Elapsed time: 0.000001 sec
```

## 🔔 Notes

* Supports decimal, scientific (e.g., `1e3`), and hexadecimal (e.g., `0xFF`) inputs.
* Computation uses a combined Fibonacci-Lucas algorithm for efficiency.
* Timing is provided using CPU time for reference.

## 📜 License

This project is licensed under the **MIT License**.

