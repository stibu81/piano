#' Generate the Notes or Keys of a Chord from Scale Degrees
#'
#' Create a chord voicing by selecting scale degrees from a major scale with
#' alterations.
#'
#' @param left,right character indicating the scale degrees to include in
#'   the left and right hand, respectively. Valid values are the scale degrees
#'   returned by [get_major_scale_with_alt()]: all integers from 1 to 15 and
#'   the alterations b3, b5, #5, b7, b9, #9, #11, and b13.
#' @param root_note a single note indicating the root note of the chord. Scale
#'   degrees are interpreted relative to this root note.
#' @param scale the output of [get_major_scale_with_alt()] for the key corresponding
#'   to the desired root note. This is mainly provided to improve performance, if many
#'   chords with the same root need to be computed, since `get_major_scale_with_alt()`
#'   is where the functions spends most its time.
#' @param upper,lower a single key that is used for the conversion to keys. If both
#'   are omitted, the function will return notes, if exactly one of them is given, it
#'   will return keys. Keys will be selected such that they all lie above (below)
#'   `upper` (`lower`).
#'
#' @returns
#' A list with components `left` and `right`, each containing a character
#' vector of notes.
#'
#' @export

get_chord <- function(
  left = c(),
  right = c(),
  root_note = "C",
  scale = NULL,
  lower = NULL,
  upper = NULL
) {
  # if no scale is given, compute it from the root_note
  if (is.null(scale)) {
    scale <- get_major_scale_with_alt(root_note)
  }

  degrees <- list(left = left, right = right)

  notes <- purrr::map(degrees, \(x) degrees_to_notes(x, scale = scale))

  if (!is.null(lower) || !is.null(upper)) {
    i_left <- seq_along(left)
    keys <- notes_to_keys(c(notes$left, notes$right), lower = lower, upper = upper)
    list(left = keys[i_left], right = keys[-i_left])
  } else {
    notes
  }
}
