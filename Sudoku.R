#####################
### Sudoku Solver ###
#####################

# Test Puzzle IM 54
# This solves in 7 iterations.
Puzzle <- matrix(
  c(0,0,0,0,4,0,9,0,0,
    3,9,8,0,0,0,0,0,6,
    0,0,0,0,0,0,7,5,1,
    0,0,0,0,1,6,0,0,2,
    6,0,4,0,0,0,1,0,8,
    7,0,0,5,3,0,0,0,0,
    5,1,6,0,0,0,0,0,0,
    2,0,0,0,0,0,3,9,4,
    0,0,3,0,2,0,0,0,0), 
  nrow=9)

# Test Puzzle IM 56
# This solves in 5 iterations.
Puzzle <- matrix(
  c(8,5,0,0,0,0,0,0,0,
    6,0,0,1,3,9,0,0,0,
    0,2,1,0,0,0,7,0,0,
    7,0,0,6,8,0,0,0,2,
    4,0,0,0,0,0,0,0,5,
    9,0,0,0,1,5,0,0,3,
    0,0,6,0,0,0,2,1,0,
    0,0,0,2,7,4,0,0,6,
    0,0,0,0,0,0,0,8,9),
  nrow=9)

# Test Puzzle IM 60
Puzzle <- matrix(
  c(8,0,5,0,1,0,9,0,0,
    0,0,2,4,9,3,0,0,0,
    0,0,0,0,0,0,4,0,6,
    1,0,9,0,0,0,0,0,0,
    4,0,0,7,0,2,0,0,1,
    0,0,0,0,0,0,3,0,5,
    3,0,6,0,0,0,0,0,0,
    0,0,0,5,2,4,7,0,0,
    0,0,4,0,3,0,8,0,9),
  nrow=9)

# Test Puzzle EX 100
# This solves in 9 iterations.
Puzzle <- matrix(
  c(6,0,0,9,0,5,3,0,0,
    1,0,4,0,0,0,0,0,0,
    0,0,0,0,3,0,0,0,6,
    0,0,2,0,0,0,6,0,0,
    8,0,0,0,6,0,0,0,5,
    0,0,7,0,0,0,9,0,0,
    5,0,0,0,8,0,0,0,0,
    0,0,0,0,0,0,4,0,1,
    0,0,6,4,0,9,0,0,2),
  nrow = 9
)

# Test Puzzle Master 15
# The code does not solve this puzzle yet.
Puzzle <- matrix(
  c(0,1,0,0,4,0,0,7,0,
    9,0,7,0,0,0,0,0,0,
    3,0,0,7,9,0,0,2,0,
    0,7,5,0,0,4,0,0,1,
    0,4,0,0,0,0,0,6,0,
    6,0,0,1,0,0,5,8,0,
    0,2,0,0,5,6,0,0,8,
    0,0,0,0,0,0,3,0,7,
    0,3,0,0,8,0,0,5,0),
  nrow = 9
)

# This will clear the puzzle, if that's what you're trying to do.
Puzzle <- matrix(c(rep(0, 81)), nrow = 9)

# Create the 3d matrix of possibilities.
cell.poss <- array(rep(1:9, 81), dim = c(9, 9, 9))
# Create the 3d matrix of memory.
record <- array(rep(0, 81), dim = c(9, 9, 9))
# Establish vectors that will be used in the loop.
rs <- cs <- rep(NA, 3)
iterations <- 0

# Run the loop
while (0 %in% Puzzle) {
  for (rw in 1:9) {
    # For every row
    for (cl in 1:9) {
      # For every column
      if (rw < 4) {
        rs <- c(1:3)
      }
      if (rw > 3 & rw < 7) {
        rs <- c(4:6)
      }
      if (rw > 6) {
        rs <- c(7:9)
      }
      if (cl < 4) {
        cs <- c(1:3)
      }
      if (cl > 3 & cl < 7) {
        cs <- c(4:6)
      }
      if (cl > 6) {
        cs <- c(7:9)
      }
      # If there's a value entered, remove it from possibilities for ...
      if (Puzzle[rw, cl] != 0) {
        value <- Puzzle[rw, cl]
        # ... the row,
        cell.poss[value, rw,] <- NA
        # ... the column,
        cell.poss[value, , cl] <- NA
        # ... and the sector.
        cell.poss[value, rs, cs] <- NA
        # Also, remove all other possibilities for this cell.
        cell.poss[-(value), rw, cl] <- NA
      } else {
        # If there isn't a value entered ...
        Puzzle[rw, cl] <-
          # If only one value is possible for the cell, enter that value. Otherwise, leave it as zero.
          ifelse(length(na.omit(unique(cell.poss[, rw, cl]))) == 1, na.omit(cell.poss[, rw, cl]), 0)
        for (o in 1:9) {
          # If there's only one place in this sector for this value to go, enter it there.
          if (sum(cell.poss[o, rs, cs], na.rm = TRUE) / o == 1) {
            pos <- which(cell.poss[, rs, cs] == o, arr.ind = T)
            Puzzle[(pos[2] + min(rs) - 1), (pos[3] + min(cs) - 1)] <-
              pos[1]
          }
          # If there's only one place in this row for this value to go, enter it there.
          if (sum(cell.poss[o, rw,], na.rm = TRUE) / o == 1) {
            pos <- which(cell.poss[, rw,] == o, arr.ind = T)
            Puzzle[rw, pos[2]] <- pos[1]
          }
          # If there's only one place in this column for this value to go, enter it there.
          if (sum(cell.poss[o, , cl], na.rm = TRUE) / o == 1) {
            pos <- which(cell.poss[, , cl] == o, arr.ind = T)
            Puzzle[pos[2], cl] <- pos[1]
          }
          # If two cells can only take the same two values, remove those from the possibilities
          # of other cells in the ...
          if (length(na.omit(unique(cell.poss[, rw, cl]))) == 2) {
            val1 <- na.omit(unique(cell.poss[, rw, cl]))[1]
            val2 <- na.omit(unique(cell.poss[, rw, cl]))[2]
            # row.
            for (i in 1:9) {
              if (length(na.omit(unique(cell.poss[, rw, i]))) == 2 & i != cl) {
                if (val1 %in% na.omit(unique(cell.poss[, rw, i])) &
                    val2 %in% na.omit(unique(cell.poss[, rw, i]))) {
                  others <- c(1:9)
                  others[cl] <- others[i] <- NA
                  cell.poss[val1, rw, others] <- NA
                  cell.poss[val2, rw, others] <- NA
                }
              }
            }
            # column.
            for (i in 1:9) {
              if (length(na.omit(unique(cell.poss[, i, cl]))) == 2 & i != rw) {
                if (val1 %in% na.omit(unique(cell.poss[, i, cl])) &
                    val2 %in% na.omit(unique(cell.poss[, i, cl]))) {
                  others <- c(1:9)
                  others[rw] <- others[i] <- NA
                  cell.poss[val1, others, cl] <- NA
                  cell.poss[val2, others, cl] <- NA
                }
              }
            }
            # sector.
            for (i in 1:9) {
              sector.poss <- matrix(cell.poss[, rs, cs], nrow = 9)
              if (length(na.omit(unique(sector.poss[, i]))) == 2 &
                  i != ((cl - min(cs)) * 3 + rw - min(rs) + 1)) {
                if (val1 %in% na.omit(unique(sector.poss[, i])) &
                    val2 %in% na.omit(unique(sector.poss[, i]))) {
                  ri <- ((i - 1) %% 3) + min(rs)
                  ci <- (i - 1) %/% 3 + min(cs)
                  # Erase those two possible values from sector, except in those 2 cells.
                  cell.poss[val1, rs, cs] <- NA
                  cell.poss[val1, rw, cl] <- val1
                  cell.poss[val1, ri, ci] <- val1
                  cell.poss[val2, rs, cs] <- NA
                  cell.poss[val2, rw, cl] <- val2
                  cell.poss[val2, ri, ci] <- val2
                }
              }
            }
          }
        }
      }
    }
  }
  iterations <- iterations + 1 # Report back to the human at the end of each iteration.
  cat("After iteration",
      iterations,
      "there are",
      length(Puzzle[Puzzle == 0]),
      "zeros left.\n")
  record[iterations, , ] <- Puzzle # For the record ...
}

####################
### Explanations ###
####################

# This script builds a 3 dimensional matrix with a length of 9 in each direction.
# This includes all possible (and many impossible) final configurations for the puzzle.
# The script takes the matrix you input as "Puzzle" in the beginning and begins deducing
# the solution from there.
# You can call this 3-dimensional matrix with this format:  cell.poss[digit,row,column]
# If the script has already deduced that digit as impossible for that column, it will
# return NA.

# It also maintains a record of its attempts. YOu can call the record with this format:
# record[attempt,row,column]
