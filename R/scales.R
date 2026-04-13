#' Get the Notes of a Major Scale
#' 
#' `get_major_scale()` returns the note names for any major key with
#' a root involving at most one accidental for an arbitrary number of
#' octaves. `get_major_scale_with_alt()` returns two octaves of any major
#' key with the alterations that can be used in chord definitions.
#' 
#' @param key character indicating the key. All keys with roots
#' involving at most one accidental are supported.
#' @param n_octave integer indicating the number of octaves to 
#'  return.
#' 
#' @returns
#' a named vector. The values are the note names (written with capital
#' letters), the names correspond to the interval from the root
#' starting at `"1"`.
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

  key <- notes_toupper(key)

  # scales involving double accidentals are created by
  # getting the scale without accidental and then moving it
  # by a semi-tone. Prepare this here.
  post_process <- \(x) x
  if (key %in% "Gb") {
    key <- "G"
    post_process <- flatten
  } else if (key %in% c("F#", "C#", "G#", "D#", "A#")) {
    key <- stringr::str_remove(key, "#")
    post_process <- sharpen
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

  # apply the post-processing
  scale <- post_process(scale)

  # if multiple octaves are requested, add them now
  if (n_octave > 1) {
    scale <- c(scale, rep(scale[-1], n_octave - 1))
  }

  names(scale) <- as.character(seq_along(scale))

  scale
}


#' @rdname get_major_scale
#' @export

get_major_scale_with_alt <- function(key) {

  scale <- get_major_scale(key, n_octave = 2)

  # compute the alterations
  i_alt_flat <- c(3, 5, 7, 9, 13)
  alt_flat <- flatten(scale[i_alt_flat])
  names(alt_flat) <- paste0("b", i_alt_flat)
  i_alt_sharp <- c(5, 9, 11)
  alt_sharp <- sharpen(scale[i_alt_sharp])
  names(alt_sharp) <- paste0("#", i_alt_sharp)

  # define a numeric vector that can be used for sorting
  srt_vec <- c(1:15, i_alt_flat - .5, i_alt_sharp + .5)

  c(scale, alt_flat, alt_sharp)[order(srt_vec)]
  
}
