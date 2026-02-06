# We need notes_toupper() from notes.R
#' @include notes.R

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


# create two sequences of three octaves of note names that can be used to
# extract major scales.

notes <- local({

  # pick the x-coordinate of C1 and c1. This is the range we want to cover
  xcs <- keys$white$xmin[keys$white$name %in% c("C", "c1")]

  generate_note_sequence <- function(accidentals) {
    dplyr::bind_rows(
        keys$white %>% dplyr::select("name", "xmin"),
        keys$black %>%
          dplyr::select(name = paste0("name_", accidentals), "xmin")
      ) %>%
      dplyr::filter(dplyr::between(.data$xmin, xcs[1], xcs[2])) %>%
      dplyr::arrange(.data$xmin) %>%
      dplyr::select(-"xmin") %>%
      # remove numbers in note names and convert to upper case
      dplyr::mutate(name = stringr::str_remove(.data$name, "\\d$") %>%
                      notes_toupper()) %>%
      dplyr::mutate(rank = seq(1, by = 0.5, length.out = nrow(.)))
  }

  # generate sequences using flats and sharps
  flat <- generate_note_sequence("flat")
  sharp <- generate_note_sequence("sharp")

  list(flat = flat, sharp = sharp)
})


# Define the keys that are written with sharps and flats
major_keys <- list(flat = c("F", "Bb", "Eb", "Ab", "Db", "Gb"),
                   sharp = c("G", "D", "A", "E", "B", "F#"))
