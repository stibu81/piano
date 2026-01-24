# Define an object that contains data on white and black keys.
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
        toupper(c(utils::tail(flats, 1), rep(flats, 2))),
        rep(flats, 5)
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


# filter keys inbetween a lower and upper limit

filter_keys <- function(lower, upper, error_call = rlang::caller_env()) {

  # find the key names in the table of white keys. Only white keys are allowed.
  i_lower <- which(keys$white == lower)
  if (length(i_lower) == 0) {
    cli::cli_abort("'{lower}' is not a valid lower key.", .envir = error_call)
  }
  i_upper <- which(keys$white == upper)
  if (length(i_upper) == 0) {
    cli::cli_abort("'{upper}' is not a valid upper key.", .envir = error_call)
  }
  if (i_upper <= i_lower) {
    cli::cli_abort("lower must be below upper.", .envir = error_call)
  }

  # filter the white keys first, then select the black keys that lie inbetween
  white <- keys$white[i_lower:i_upper, ]
  black <- keys$black %>%
    dplyr::filter(.data$xmin > min(white$xmin), .data$xmin < max(white$xmin))

  list(white = white, black = black)
}
