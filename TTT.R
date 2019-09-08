###################
### Tic Tac Toe ###
###################

# Clear the board
board <- matrix(c(rep(".", 9)), nrow = 3)

# Setup
win<-FALSE
moves<-0
cat(
  " Welcome to Chris's best attempt at tic tac toe for R!\n",
  "The positions on the board are identified like this:\n",
  "1 | 4 | 7\n",
  "---------\n",
  "2 | 5 | 8\n",
  "---------\n",
  "3 | 6 | 9\n",
  "To begin playing, run the code on line 22."
)

while ("." %in% board) {
  ### Human's turn
  n <- as.integer(readline(prompt = "Choose your next move: "))
  if (board[n] == ".") {
    board[n] <- "X"
    c.moved <- FALSE
  } else {
    cat("That is not a valid move.\n")
  }
  
  if (moves==4){
    cat("It's a draw!\n")
    win<-TRUE
  }
  moves<-moves+1
  
  ### Computer's turn
  diag1 <- c(board[1, 3], board[2, 2], board[3, 1])
  diag2 <- c(board[1, 1], board[2, 2], board[3, 3])
  # Taking center, if available
  if (board[2, 2] == ".") {
    board[2, 2] <- "O"
    c.moved <- TRUE
  }
  # Defense
  # columns
  for (i in 1:3) {
    if (sum(board[, i] %in% "X") == 2 &
        sum(board[, i] %in% ".") == 1 & !c.moved) {
      board[board[, i] %in% ".", i] <- "O"
      c.moved <- TRUE
    }
  }
  # rows
  for (i in 1:3) {
    if (sum(board[i,] %in% "X") == 2 &
        sum(board[i,] %in% ".") == 1 & !c.moved) {
      board[i, board[i,] %in% "."] <- "O"
      c.moved <- TRUE
    }
  }
  # diagonals
  if(sum(diag1 %in% "X")==2 & "." %in% diag1 & !c.moved){
    pick <- which(diag1 == ".")
    board[pick, 4 - pick] <- "O"
    c.moved <- TRUE
  }
  if(sum(diag2 %in% "X")==2 & "." %in% diag2 & !c.moved){
    pick <- which(diag2 == ".")
    board[pick, pick] <- "O"
    c.moved <- TRUE
  }
  # Finishing
  # columns
  for (i in 1:3) {
    if (sum(board[,i] %in% "O")==2 & "." %in% board[,i] & !c.moved) {
      col <- board[, i]
      board[which(col == "."), i] <- "O"
      c.moved <- TRUE
    }
  }
  # rows
  for (i in 1:3) {
    if (sum(board[i,] %in% "O")==2 & "." %in% board[i,] & !c.moved) {
      row <- board[i,]
      board[i,which(row == ".")] <- "O"
      c.moved <- TRUE
    }
  }
  # negative diagonal
  if (sum(diag1 %in% "O")==2 & "." %in% diag1 & !c.moved) {
    pick <- which(diag1 == ".")
    board[pick, 4 - pick] <- "O"
    c.moved <- TRUE
  }
  # positive diagonal
  if (sum(diag2 %in% "O")==2 & "." %in% diag2 & !c.moved) {
    pick <- which(diag2 == ".")
    board[pick, pick] <- "O"
    c.moved <- TRUE
  }
  # Passive movement
  # columns
  for (i in 1:3) {
    if (!("X" %in% board[, i]) & "." %in% board[, i] & !c.moved) {
      col <- board[, i]
      board[sample(which(col == "."), 1), i] <- "O"
      c.moved <- TRUE
    }
  }
  # rows
  for (i in 1:3) {
    if (!("X" %in% board[i, ]) & "." %in% board[i, ] & !c.moved) {
      row <- board[i, ]
      board[i, sample(which(row == "."), 1)] <- "O"
      c.moved <- TRUE
    }
  }
  # negative diagonal
  if (!("X" %in% diag1) & !c.moved) {
    pick <- sample(which(diag1 == "."), 1)
    board[pick, 4 - pick] <- "O"
    c.moved <- TRUE
  }
  # positive diagonal
  if (!("X" %in% diag2) & !c.moved) {
    pick <- sample(which(diag2 == "."), 1)
    board[pick, pick] <- "O"
    c.moved <- TRUE
  }
  # If no other move is planned, make a random move.
  if (!c.moved) {
    board[sample(which(board == "."), 1)] <- "O"
    c.moved <- TRUE
  }
  # Show board
  cat(
    "",
    board[1, 1],
    "|",
    board[1, 2],
    "|",
    board[1, 3],
    "\n",
    "---------- \n",
    board[2, 1],
    "|",
    board[2, 2],
    "|",
    board[2, 3],
    "\n",
    "---------- \n",
    board[3, 1],
    "|",
    board[3, 2],
    "|",
    board[3, 3],
    "\n"
  )
  # Check for wins
  diag1 <- c(board[1, 3], board[2, 2], board[3, 1])
  diag2 <- c(board[1, 1], board[2, 2], board[3, 3])
  for (i in 1:3) {
    if (length(unique(board[, i])) == 1) {
      if (board[1, i] == "X") {
        cat("Human wins!\n")
        win <- TRUE
      }
      if (board[1, i] == "O") {
        cat("Computer wins!\n")
        win <- TRUE
      }
    }
    if (length(unique(board[i, ])) == 1) {
      if (board[i, 1] == "X") {
        cat("Human wins!\n")
        win <- TRUE
      }
      if (board[i, 1] == "O") {
        cat("Computer wins!\n")
        win <- TRUE
      }
    }
  }
  if (length(unique(diag1)) == 1) {
    if (diag1[1] == "X") {
      cat("Human wins!\n")
      win <- TRUE
    }
    if (diag1[1] == "O") {
      cat("Computer wins!\n")
      win <- TRUE
    }
  }
  if (length(unique(diag2)) == 1) {
    if (diag2[1] == "X") {
      cat("Human wins!\n")
      win <- TRUE
    }
    if (diag2[1] == "O") {
      cat("Computer wins!\n")
      win <- TRUE
    }
  }
  if (win) {
    break
  }
}
