# Day 12 - The Final Pieces

**SPOILERS AHEAD**

In [this](https://adventofcode.com/2025/day/12) puzzle, we are given:

* Some shapes of puzzle pieces ("presents" in the lore). Each a selection of cells from a 3x3 grid
* A list of rectngles with the number of each shape to try to fit into them

## Part 1
**Problem:** How many of the rectangles can fit all the puzzle pieces listed with the multipliciities given? (Puzzle pieces can be rotated or reflected, to be placed in the grid, but when placed must not overlap).

**Solution**:

This one took some thought.

* To start with, it's good to work out all the shapes of the puzzle pieces up to the allowed 8 isometries (rotations and reflections).
    * The complication with this, is that the multiplicities now refer to a set of isometric shapes not just one
    * It does mean that we don't have to consider isometries in the rest of the algorithm though.
* An initial search pattern based on placing tiles shape by shape did not work, even when trying to find ways to place a shape that meshes best with the shapes already placed. 
    * This was probably a flaw in the algorithm, but I had also forgotten about strictness in Haskell, so switching to strict folds in places may have bounded memory usage from big *thunks*, and caused the algorithm not to crash.

* Instead, what did work pretty well was to **add spaces as a shape** and **move the origin of each shape** so that the translating the shape to `(x,y)` would occupy that cell (and no cells previously visited).
    * In this way, we visit cells in the rectangle, either adding a space or filling it with the corner of some puzzle piece.
    * It's still a depth-first search.
    * If on a branch, we run out of puzzle pieces (or spaces) from the target count, we can stop searching that branch.
    * Detecting cases when there are more cells to fill with puzzle pieces than the area of the rectangle is free (because we are already calculating how many spaces we can place).
    * A careful choice of the origin of each puzzle tile means that they can never intersect with the spaces we've placed, so no need to track collisions with placed spaces and puzzle tiles separately.
    * Additional "Filled" cells were added at the edges of the rectangle, so that the same collision detection logic would prevent the placing of pieces outside the rectangle, without further code.


```
#########
#1___233#
#1112223#
#_41.233#
#444....#
#.4.....#
#.......#
#.......#
#.......#
#.......#
#.......#
#.......#
#########
```

See above:
* Go top - bottom, left - right, placing a puzzle tile (`1`,`2`,`3` & `4`) or a space `_` on each blank `.` tile encountered.
* Each tile is translated so that only that cell, cells to the right or on rows below are filled each time.
* Pieces cannot be places to intersect previously placed pieces or the rectangle boundary.


## Part 2
**Problem:** Click a button

**Solution:**
Click the button
