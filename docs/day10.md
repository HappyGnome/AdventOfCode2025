# Day 10 - Still Thinking

**SPOILERS AHEAD**

In [this](https://adventofcode.com/2025/day/10) puzzle, we are given a set of problems ("machines"), each with a list of lights (boolean 0 or 1), vectors, and a single target vector (positive integer entries.)
The vectors have entries of 1s and 0s only are given in a sparse format with the indices of all the 1s.

Part 2 was very tricky and I'm still trying to find ways to solve it non-slowly.

## Part 1
**Problem:** For each machine, find the smallest* linear combination (with positive integer coefficients) of the vectors such that the parity of the resulting vector matches the lights (0-> even 1-> odd).

*i.e. minimise l1 norm of the coefficients

**Solution**:
A breadth-first search with memoization works here, because we're only trying to hit a target modulo 2, so the search space turns out not to be too big.

## Part 2
**Problem:** 
For each machine, what is the minimal linear combination (in the same sense) that gives the target vector?

**Solution:**
(Tricky! - I'm still not sure if there's a fast way)
* Feels like Integer Linear Programming... Time to ~~freak out~~ ~~give up~~ git gud

* Essentially my solution was (eventually) to do a depth-first recursive search (this is fine - it's Haskell), but doing a completely naive (memo-ized) search is not going to run in *finite time*.
* To slightly improve things (back to *finite time*), I used the following approach:
    1. Count the occurrences `Nx` of each coordinate `x` in the sparse vectors
    2. Take the target number `Mx` of each coordinate
    3. The degrees of freedom at the coordinate are `Mx + Nx - 1 Choose Nx - 1`
        * A.k.a the number of ways to make `Mx` as a sum of `Nx` distinguishable positive integers
        * See [throwing balls into boxes](https://en.wikipedia.org/wiki/Balls_into_bins_problem)
    4. The *Active index* is any of the coordinates with the lowest (non-zero) degrees of freedom
    4. Start a phase of the search, just trying combinations of vectors containing the *Active index*
    5. For the last vector searched in a phase, we are constrained to take its coefficient to make sure the target at the active index is met. If this is not possible without violating the other target constraints then we can terminate a branch of the search.
    6. If the target constraint at the *Active index* is made, start a new phase, finding the coordinate with the fewest degrees of freedom after subtracting the current coeficients at the search site.


This is obviously justy a sketch not a whole algorithm!

![Very Slow](images/Day10_opt_needed.png)

More work needed to find a good approach. Maybe an `A*` algorithm using the simplex method to make the heuristics? Better start building a little sparse linear algebra library...
