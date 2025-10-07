# tower_of_hanoi

A **recursive Tower of Hanoi solver** implemented in **Haskell**.
This program demonstrates the classic recursive algorithm to move disks between three pegs following the rules of the puzzle.

## ⚙️ Build & Run

Make sure **GHC** is installed.
To build and execute:

```bash
cd tower_of_hanoi
ghc -O2 tower_of_hanoi.hs -o tower_of_hanoi
./tower_of_hanoi <n>
```

Example:

```bash
cd tower_of_hanoi
ghc -O2 tower_of_hanoi.hs -o tower_of_hanoi
./tower_of_hanoi 3
```

Output:

```
'A' --> 'C'
'A' --> 'B'
'C' --> 'B'
'A' --> 'C'
'B' --> 'A'
'B' --> 'C'
'A' --> 'C'
```

Each line represents a single move from one peg to another.

## 🧩 How It Works

* Uses recursion to solve the Tower of Hanoi problem for `n` disks.
* `move n s d t` recursively transfers disks from source peg `s` to destination peg `d` using temporary peg `t`.
* The base case (`move 1 s d t`) performs a single move.
* The function call pattern follows the recurrence relation:
  [
  T(n) = 2T(n-1) + 1
  ]

## 🧠 Notes

* The total number of moves required is (2^n - 1).
* Works well for small to medium `n`; output grows exponentially with larger values.
* Pegs are labeled `'A'`, `'B'`, and `'C'`.

## 🪪 Author

**Abhrankan Chakrabarti**
