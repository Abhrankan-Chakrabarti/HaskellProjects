# n_queen

An **N-Queens solver** implemented in **Haskell**.
This program finds all solutions to the N-Queens problem for a given board size `n`.

## 🛠 Build & Run

Make sure you have **GHC** installed. You can build and run the project like this:

```bash
cd n_queen
ghc n_queen.hs -o n_queen
./n_queen <n>
```

* `<n>` — size of the chessboard (number of queens).

Example:

```bash
cd n_queen
ghc n_queen.hs -o n_queen
./n_queen 8
```

Output:

```
Solution 1: [1,5,8,6,3,7,2,4]
Solution 2: [1,6,8,3,7,4,2,5]
...
```

> Each solution shows the positions of queens in each column of the board.

## 📜 Notes

* The program uses **backtracking** to explore all possible placements.
* Run-time increases significantly for larger values of `n` due to the combinatorial nature of the problem.
* Command-line argument `<n>` is required; interactive input is supported if no argument is provided.

