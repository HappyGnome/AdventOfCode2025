# Day 11 - Let Me Count The Ways

**SPOILERS AHEAD**

In [this](https://adventofcode.com/2025/day/11) puzzle, we are given a directed graph (unweighted) with nodes identified by 3 letters.

## Part 1
**Problem:** How many paths exist from vertex `you` to vertex `out`? (The vertex names probably have some relation to the story).

**Solution**:
* I went with a recursive depth-first search here.
* No need for any algorithm from my "library", it's only a few lines to write a little recursive routine.

The general idea of the algorithm is:

1. The number of the ways from `you` to `out` is the sum of the number of ways to `out` from each direct child of `you`.
2. Initially `you` is the *current vertex*.
3. Run this algorithm recursively, starting at each child of the *current vertex*, this gives the count of ways to `out` from each child. The number of ways to `out` form the *current vertex*, is just the sum of the results of the algorithm applied to the children.  
4. It's eash to memoize this (doing the naive recursion means we could visit the same vertex many times). Once a vertex is visited, we already know how many paths exist starting there and ending in `out`, so we can just look that up for each vertex visited and recurse only when needed.
5. Each edge only appears once in our search due to memo-ization, so we know the algorithm will terminate in a *finite time*.

* We didn't need to worry about loops in this example, in general that could be something else to account for.

## Part 2
**Problem:** How many ways are there from `svr` to `out` that pass through `dac` and `fft`?

**Solution:**

* Notice that you're not starting at `you` anymore. It's quite important to notice that.
* All we have to do is:

    * Count paths from `svr` to `dac`, `svr` to `fft`, `dac` to `fft`, `fft` to `dac`, `dac` to `out`, and `fft` to `out`
    * We can do each in the same way as we did for part 1
    * Ways from `svr` to `out` via `dac` *then* `fft` are: `#(svr to dac) * #(dac to fft) * #(fft to out)`, for example.
    * The only other option is paths that visit `fft` *then* `dac`.
* Again, there appeared to be no loops, but there could theoretically be some that were not part of any valid path. The algorithm could get stuck on one of these.
    * We just added a detection of loops and counted any vertex appearing in a loop to have `0` routes to `out` (based on the assumption that the question is solvable a.k.a. the *kindly examiner heuristic*).


**Pro tip:** Don't forget not to make mistakes in the memo-ization, otherwise you might be fooled into thinking there's something else going on here, like some tricky loopy edge case. TLEC! There isn't, it's just you... 
