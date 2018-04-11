suits.table <- function(hand) {
  suits <- gsub(pattern = "[2-9AKGJT]",
                replacement = "",
                x = hand)
  suits <- table(suits)
  suits
}

prob.flush <- function(hand) {
  prob.one <- NA
  prob.two <- NA
  flush <- 0
  size <- length(hand)
  unknown <- 52 - size
  suits <- suits.table(hand)
  suits.max <- max(suits)
  suits.left <- 13 - suits.max
  if (suits.max >= 5) {
    flush <- 1
  } else {
    if (size == 5) {
      if (suits.max == 4) {
        prob.one <- suits.left / unknown
        prob.two <-
          1 - (unknown - suits.left) / unknown * (unknown - suits.left - 1) / (unknown - 1)
      } else if (suits.max == 3) {
        prob.two <- suits.left / unknown * (suits.left - 1) / (unknown - 1)
      }
    } else if (size == 6) {
      if (suits.max == 4) {
        prob.one <- suits.left / unknown
      }
    }
  }
  result <- as.numeric(c(prob.one, prob.two, flush))
  result
}