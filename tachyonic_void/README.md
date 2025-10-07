# tachyonic_void

A **turbulent, animated sorting benchmark** written in **Haskell**, featuring a real-time **battle** between *Tachyonic Quicksort* and *Tachyonic Insertion Sort*.
This program visualizes sorting performance using colorful ASCII bars, dynamic progress animations, and cosmic humor.

## ⚙️ Build & Run

Make sure **GHC** is installed.
To build and execute:

```bash
cd tachyonic_void
ghc -O2 tachyonic_void.hs -o tachyonic_void
./tachyonic_void [n]
```

Example:

```bash
cd tachyonic_void
ghc -O2 tachyonic_void.hs -o tachyonic_void
./tachyonic_void 1000
```

If no argument is provided, the program interactively prompts:

```
Enter upper limit (e.g., 5000):
```

## 🧩 How It Works

* **Two algorithms compete:**

  * `tachyonicQsort`: A recursive Quicksort implementation.
  * `tachyonicIsort`: A recursive Insertion Sort implementation.
* The input list is **randomized** using a “tachyonic riffle shuffle.”
* Execution times are measured using `System.CPUTime`.
* Animated progress bars show relative speeds.
* A **tachyonic banner** introduces each run, followed by a victory message.

## 🌀 Features

* Live animated progress bar using Unicode characters (⚡🌀✨).
* Real-time CPU time measurement.
* Cosmic color-coded output (green for Quicksort, blue for InsertionSort).
* Funny “tachyonic” sci-fi references and a terminal bell alert on victory.

## 🧠 Notes

* Use moderate list sizes (≤5000) for smooth animation.
* For large inputs, the animation may slow down due to CPU overhead.
* Requires a terminal supporting ANSI color codes and Unicode.

## 🪪 Author

**Abhrankan Chakrabarti**

