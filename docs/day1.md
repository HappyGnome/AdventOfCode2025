# Day 1 - Going In Circles

**SPOILERS AHEAD**

In [this](https://adventofcode.com/2025/day/1) simple puzzle, we are given a series of rotations (moves) of a dial, in 100ths of a turn.

## Part 1
**Problem:** How many times do we land on 0 after a move?

Solution:
* Simulate the rotations (simple use of modulo arithmetic) and count...

## Part 2
**Problem:** How many times do we pass 0, while completing all the moves?

**Solution(s):**
* You could simulate each 100th of a turn and count (I didn't try it that way, but it would be a much quicker way to code a solution than the below); or
* you could work out how many times 0 was passed per move in the input. Here we want to count crossings of 0 when starting at `at` and moving `x` 100ths:

    ![Better solution Day 1](images/Day1_OK.png)
  * The key is to realise that the formula for the first case (`x > 0`) is only correct when `0 <= at < 100`
  * "Reflecting" the same forumla (`at` -> `100 - at`, `x` -> `-x`) for `x < 0` works, but only when `0 <= 100 - at < 100`
  * So we have to deal with the case `at == 0` separately.

* Or... what I did at first:

    ![Bad solution Day 1](images/Day1_bad.png)

    ... Oh dear, better luck tomorrow!
