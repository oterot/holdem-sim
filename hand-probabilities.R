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

suits.table <- function(hand) {
  suits <- gsub(pattern = "[2-9AKGJT]",
                replacement = "",
                x = hand)
  suits <- table(suits)
  suits
}

prob.flush <- function(hand) {
  p1 <- NA
  p2 <- NA
  flush <- 0
  size <- length(hand)
  unknown <- 52 - size
  suits <- max(suits.table(hand))
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
  result <- as.numeric(c(p1, p2, flush))
  result
}