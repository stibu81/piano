#' Get the Notes of a Major Scale
#' 
#' @param key character indicating the key. Supported are
#'  keys with up to six accidentals.
#' @param n_octave integer indicating the number of octaves to 
#'  return.
#' 
#' @export

get_major_scale <- function(key, n_octave = 1L) {
  # check input for n_octave
  if (!is.numeric(n_octave)) {
    cli::cli_abort("'n_octave' must be a numeric value.")
  }
  n_octave <- as.integer(n_octave)
  if (n_octave < 1) {
    cli::cli_abort("'n_octave' must be a positive integer.")
  }

  # check whether the key uses sharps or flats.
  # for "C" it does not matter => use sharps
  if (key %in% c("C", major_keys_data$sharp)) {
    notes <- notes_data$sharp
  } else if (key %in% major_keys_data$flat) {
    notes <- notes_data$flat
  } else {
    cli::cli_abort("'{key}' is not a valid key.")
  }

  # extract one octave
  major_intervals <- c(0, 1, 2, 2.5, 3.5, 4.5, 5.5, 6)
  root_rank <- notes$rank[notes$name == key][1]
  scale_ranks <- root_rank + major_intervals
  scale <- notes$name[notes$rank %in% scale_ranks]

  # handle enharmonic equivalences
  if (key == "Gb") {
    scale[scale == "B"] <- "Cb"
  } else if (key == "F#") {
    scale[scale == "F"] <- "E#"
  }

  # if multiple octaves are requested, add them now
  if (n_octave > 1) {
    scale <- c(scale, rep(scale[-1], n_octave - 1))
  }

  scale
}
