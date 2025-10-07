# Arbitrary-Precision Fibonacci (fibgmp)

Computes **Fibonacci numbers** using **GMP (GNU Multiple Precision Arithmetic Library)** for arbitrary-precision arithmetic. Supports interactive input, command-line arguments, and optional verbose output.

## 🛠 Build & Run

Make sure you have **GCC**, **GHC**, and **GMP** installed.

```bash
cd fibgmp
gcc -c cbits/wrappers.c -o cbits/wrappers.o
ghc fibgmp.hs cbits/wrappers.o -o fibgmp
./fibgmp          # Linux/macOS
.\fibgmp.exe     # Windows
```

## ⚙ Command-line Arguments

* `--print` — prints the Fibonacci number
* `--verbose` — prints additional computation info
* `[n]` — optional number argument; if not provided, program prompts for input

Example run:

```bash
./fibgmp --print --verbose 1000    # Linux/macOS
.\fibgmp.exe --print --verbose 1000  # Windows
```

Or interactively:

```
./fibgmp --print
Enter n: 1000
```

## 📋 Expected Output

The program prints the Fibonacci number and optionally computation times if verbose mode is on.

Example:

```
Computing F(1000)...
F(1000) = 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
Elapsed time: 0.001234 sec
```

## 🔔 Notes

* Uses **unsafePerformIO** to wrap GMP computations for pure function usage.
* Supports decimal, scientific, and hexadecimal inputs.
* Ensure reasonable input values to avoid long computation times or memory usage.
* Verbose mode provides timing and computation details.

## 📜 License

This project is licensed under the **MIT License**.

