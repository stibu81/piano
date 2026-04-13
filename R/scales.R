#' Get the Notes of a Major Scale
#'
#' `get_major_scale()` returns the note names for any major key with
#' a root involving at most one accidental for an arbitrary number of
#' octaves. `get_major_scale_with_alt()` returns two octaves of any major
#' key with the alterations that can be used in chord definitions.
#'
#' @param tonic character indicating the tonic (musical key). All tonics
#' involving at most one accidental are supported.
#' @param n_octaves integer indicating the number of octaves to return.
#'
#' @returns
#' a named vector. The values are the notes (written with capital
#' letters), the names correspond to the scale degree starting at `"1"`.
#'
#' @export

get_major_scale <- function(tonic, n_octaves = 1L) {
  # check input for n_octaves
  if (!is.numeric(n_octaves)) {
    cli::cli_abort("'n_octaves' must be a numeric value.")
  }
  n_octaves <- as.integer(n_octaves)
  if (n_octaves < 1) {
    cli::cli_abort("'n_octaves' must be a positive integer.")
  }

  tonic <- notes_toupper(tonic)

  # scales involving double accidentals are created by
  # getting the scale without accidental and then moving it
  # by a semi-tone. Prepare this here.
  post_process <- \(x) x
  if (tonic %in% "Gb") {
    tonic <- "G"
    post_process <- flatten
  } else if (tonic %in% c("F#", "C#", "G#", "D#", "A#")) {
    tonic <- stringr::str_remove(tonic, "#")
    post_process <- sharpen
  }

  # check whether the key uses sharps or flats.
  # for "C" it does not matter => use sharps
  if (tonic %in% c("C", major_keys_data$sharp)) {
    notes <- notes_data$sharp
  } else if (tonic %in% major_keys_data$flat) {
    notes <- notes_data$flat
  } else {
    cli::cli_abort("'{tonic}' is not a valid tonic.")
  }

  # extract one octave
  major_intervals <- c(0, 1, 2, 2.5, 3.5, 4.5, 5.5, 6)
  root_rank <- notes$rank[notes$name == tonic][1]
  scale_ranks <- root_rank + major_intervals
  scale <- notes$name[notes$rank %in% scale_ranks]

  # apply the post-processing
  scale <- post_process(scale)

  # if multiple octaves are requested, add them now
  if (n_octaves > 1) {
    scale <- c(scale, rep(scale[-1], n_octaves - 1))
  }

  names(scale) <- as.character(seq_along(scale))

  scale
}


#' @rdname get_major_scale
#' @export

get_major_scale_with_alt <- function(tonic) {
  scale <- get_major_scale(tonic, n_octaves = 2)

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
