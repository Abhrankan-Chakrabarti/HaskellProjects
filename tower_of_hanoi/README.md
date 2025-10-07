```
==========================================
        T O W E R S   O F   H A N O I
==========================================

A simple Haskell program to solve the classic Towers of Hanoi puzzle using recursion.

⚙️ Build & Run
------------------------------------------
ghc -O2 tower_of_hanoi.hs -o tower_of_hanoi
./tower_of_hanoi <n>

<n> : Number of disks to solve for (e.g., 3)

Example:
------------------------------------------
./tower_of_hanoi 3

Output:
A --> C
A --> B
C --> B
A --> C
B --> A
B --> C
A --> C

🧩 How It Works
------------------------------------------
* Uses recursive strategy:
    1. Move n-1 disks from source to auxiliary.
    2. Move the largest disk from source to target.
    3. Move n-1 disks from auxiliary to target.
* 'move' function prints each move as: source --> target.
* Accepts the number of disks as a command-line argument.

🪪 Author
------------------------------------------
Abhrankan Chakrabarti
```