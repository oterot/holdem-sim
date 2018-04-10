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

hand <- sample(x = DECK, size = 7, replace = FALSE)
