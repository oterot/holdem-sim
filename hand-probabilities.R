prob.one <- function(outs, unknown) {
  outs / unknown
}

prob.two <- function(outs, unknown) {
  nonouts <- unknown - outs
  1 - (nonouts / unknown) * (nonouts - 1) / (unknown - 1)
}

prob.both <- function(outs, unknown) {
  outs / unknown * (outs - 1) / (unknown - 1)
}

count.suits <- function(hand) {
  suits <- gsub(pattern = "[2-9AKGJT]",
                replacement = "",
                x = hand)
  suits <- table(suits)
  suits
}

count.ranks <- function(hand) {
  ranks <- gsub(pattern = "[cdhs]",
                replacement = "",
                x = hand)
  ranks <- table(ranks)
  ranks
}

prob.flush <- function(hand) {
  p1 <- NA
  p2 <- NA
  flush <- 0
  size <- length(hand)
  unknown <- 52 - size
  suits <- max(count.suits(hand))
  outs <- 13 - suits
  if (suits >= 5) {
    flush <- 1
  } else {
    if (size == 5) {
      if (suits == 4) {
        p1 <- prob.one(outs, unknown)
        p2 <- prob.two(outs, unknown)
      } else if (suits == 3) {
        p2 <- prob.both(outs, unknown)
      }
    } else if (size == 6) {
      if (suits == 4) {
        p1 <- prob.one(outs, unknown)
      }
    }
  }
  result <- round(as.numeric(c(p1, p2, flush)),2)
  result
}

prob.quads <- function(hand) {
  p1 <- NA
  p2 <- NA
  quads <- 0
  size <- length(hand)
  unknown <- 52 - size
  ranks <- max(count.ranks(hand))
  same <- sum(count.ranks(hand) == ranks)
  if (ranks == 4) quads <- 1
  else if(ranks == 3) {
    outs <- 1
    if (size == 5){
      p1 <- prob.one(out, unknown)
      p2 <- prob.two(out, unknown)
    }
    if (size == 6){
      p1 <- prob.one(outs, unknown)
    }
  }
  else if(ranks == 2) {
    outs <- 2
    if (size == 5){
      p2 <- prob.both(outs, unknown)
    }
  }
  result <- round(as.numeric(c(p1,p2,quads)),2)
  result
  }
  
  
# Write a function that determines what the actual cards are that you need to get your hand
# example: flush.outs(hand) returns a list of cards that give you a flush
# TODO: royal flush, straight flush, four of a kind, full house, straight, three of a kind, two pair, pair
