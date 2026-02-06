# Define an object that contains data on white and black keys.
# Note names use the numbered Helmholtz pitch notation where a standard
# 88 key piano ranges from A2 to c5
keys <- local({
  # white keys
  c_major <- c("C", "D", "E", "F", "G", "A", "B")
  n8 <- 7
  white <- dplyr::tibble(
    name = paste0(
      c(
        utils::tail(c_major, 2), rep(c_major, 2),
        tolower(rep(c_major, 5)), "c"
      ),
      c(rep(2, 2), rep(1, 7), rep("", 14), rep(1:4, each = 7), 5)
    ),
    xmin = c(
      c(-2, -1),
      rep(0:6, n8) + 7 * rep(0:(n8 - 1), each = 7),
      6 + 7 * (n8 - 1) + 1
    ),
    width = 1,
    ymin = 0,
    ymax = 6.5
  )


  # black keys
  sharps <- c_major[c(1:2, 4:6)]
  flats <- c_major[c(2:3, 5:7)]
  black <- dplyr::tibble(
    name_sharp = paste0(
      c(
        utils::tail(sharps, 1), rep(sharps, 2),
        tolower(rep(sharps, 5))
      ),
      "#",
      c(2, rep(1, 5), rep("", 10), rep(1:4, each = 5))
    ),
    name_flat = paste0(
      c(
        utils::tail(flats, 1), rep(flats, 2),
        tolower(rep(flats, 5))
      ),
      "b",
      c(2, rep(1, 5), rep("", 10), rep(1:4, each = 5))
    ),
    xmin = c(
      -2,
      rep(c(0, 1, 3:5), n8) + 7 * rep(0:(n8 - 1), each = 5)
    ) + (1 - 0.52/2),
    width = 0.52,
    ymin = 2.2,
    ymax = 6.5
  )

  list(white = white, black = black)
})
