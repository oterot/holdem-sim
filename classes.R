rm(list = ls())

suits <- c("c", "d", "h", "s")
ranks <-
  c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
cards <- expand.grid(ranks, suits, stringsAsFactors = FALSE)
names(cards) <- c("rank", "suit")
cards$display <- paste0(cards$rank, cards$suit)
values <-
  c(
    "A" = 14,
    "K" = 13,
    "Q" = 12,
    "J" = 11,
    "T" = 10,
    "9" = 9,
    "8" = 8,
    "7" = 7,
    "6" = 6,
    "5" = 5,
    "4" = 4,
    "3" = 3,
    "2" = 2
  )
cards$value <- values[cards$rank]

DECK <- cards$display

SHUFFLE <- function() {
  DECK <<- sample(cards$display, size = 52, replace = FALSE)
}

DEAL <- function() {
  card <- DECK[1]
  DECK <<- DECK[-1]
  card
}

player <- sample(x = 1:2,
                 size = 1,
                 replace = FALSE)
switch <- c(2, 1)

START_DEAL <- function() {
  SHUFFLE()
  players <- list(c(NA, NA), c(NA, NA))
  for (i in 1:2) {
    player <- switch[player]
    players[[player]][i] <- DEAL()
    player <- switch[player]
    players[[player]][i] <- DEAL()
  }
  hero <<- players[[1]]
  villain <<- players[[2]]
  burn <<- DEAL()
  board <<- replicate(n = 3, expr = DEAL())
  hand <<- c(board, hero)
  cat("\014")
  cat("Hero:", paste(players[[1]], sep = " "), "\n")
  cat("Villain:", paste(players[[2]], sep = " "), "\n")
  cat("Board:", paste(board, sep = " "), "\n")
  
}

START_DEAL()

count.suits <- function(hand) {
  inhand <- (cards$display %in% hand)
  suits <- cards$suit[inhand]
  count <- table(suits)
  count[count == max(count)]
}

prob.flush <- function(hand) {
  count <- count.suits(hand)
  unknown <- 52 - length(hand)
  suits <- as.integer(count[1])
  flush <- 13 - suits
  nonflush <- unknown - flush
  if (length(hand) == 5) {
    if (suits == 5) {
      prob.one <- 100
      prob.two <- 100
    } else if (suits == 4) {
      prob.one <- flush / unknown
      prob.two <-
        1 - (nonflush / unknown) * (nonflush - 1) / (unknown - 1)
    } else if (suits == 3) {
      prob.one <- 0
      prob.two <- flush / unknown * (flush - 1) / (unknown - 1)
    } else {
      prob.one <- 0
      prob.two <- 0
    }
  }
  else if (length(hand) == 6) {
    prob.two <- NA
    if (suits > 4) {
      prob.one <- 100
    } else if (suits == 4) {
      prob.one <- flush / unknown
    } else {
      prob.one <- 0
    }
  }
  else if (length(hand) == 7 && suits > 4) {
    prob.one <- 100
  }
  else{
    prob.one <- 0
    prob.two <- NA
  }
  
  cat("Probability of flush on next card:", round(prob.one, 2), "\n")
  cat("Probability of flush on next two cards:",
      round(prob.two, 2),
      "\n")
}

next.card <- function() {
  burn <<- c(burn, DEAL())
  board <<- c(board, DEAL())
  hand <<- c(hero, board)
  cat("\014")
  cat("Hero:", paste(hero, sep = " "), "\n")
  cat("Villain:", paste(villain, sep = " "), "\n")
  cat("Board:", paste(board, sep = " "), "\n")
  prob.flush(hand)
}

prob.flush(hand)
