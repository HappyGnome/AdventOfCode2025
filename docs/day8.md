# Day 8 - ... Kruskal's (or adventures in K1000)

**SPOILERS AHEAD**

In [this](https://adventofcode.com/2025/day/8) puzzle, we are given a list of vertices in 3D with integer coordinates. Distances betweeen vertices for this puzzle are Euclidean distances. Below, _edges_ means edges in the complete (undirected, weighted) graph on these vertices, with weights given by Euclidean distances. 

There are 1000 vertices in the input, so we're looking at subgraphs of [**K1000**](https://en.wikipedia.org/wiki/Complete_graph), probably the least planar graph since K1001. Not that it matters for today's puzzle.


## Part 1
**Problem:** Take the shortest 1000 edges. Find the largest 3 connected components of the graph with these edged. The product oof the components' sizes is the solution.

**Solution**:
* It may have been a slow way to code it, in retrospect, but saw an excuse to use my library implementation of [Kruskal's](https://en.wikipedia.org/wiki/Kruskal%27s_algorithm) algorithm and based my solution on that. 
* It's surprisingly slow hooking up a novel problem to existing code sometimes, if the data shapes don't match neatly. I realised that some extra library methods to put the output of my implementation of the algrithm into a more useful shape would be handy.
* The problem basically walks you through the algorithm to implement (it's essentially a weak form of Kruskal's), so it would have been better to just follow it naively, perhaps.

Kruskal's algoithm finds minimal spanning trees greedily, adding the shortest edge that maintains the forest property (i.e. without creating cycles) at each step. We don't really care about minimality, but we do want to find the connected components. You see, the algorithm works on disconnected graphs, and creates one tree per connected component.

Without details on the annoying data-reshaping:
1. Find the lengthe of all edges
2. Take the top 1000 edges
3. Run Kruskal's Algorithm on the graph with these edges, and all vertices in the input
4. Find the size of each tree in the output of the algorithm.
5. Take the 3 largest trees (by number of vertices), and calculate the solution.

## Part 2
**Problem:** Find the minimal spanning tree of the whole K1000 (I.e. the same process as part 1 but without taking a subset of the edges at the start). What is the product of the `x` coordinates of each end of the last edge added?  

**Solution:**
* In the question is was not really clear if we needed to take both ends of the last edge added, or the last two vertices added, but it was meant to be the former.
* This was quite simple, because we always add edges from shortest to longest, so the last edge added is the longest edge in the minimal spanning tree output by Kruskal's algorithm.



