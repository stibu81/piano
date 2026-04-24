#' Generate the Notes or Keys of a Chord from Scale Degrees
#'
#' Create a chord voicing by selecting scale degrees from a major scale with
#' alterations.
#'
#' @param left,right character indicating the scale degrees to include in
#'   the left and right hand, respectively. Valid values are the scale degrees
#'   returned by [get_major_scale_with_alt()]: all integers from 1 to 15 and
#'   the alterations b3, b5, #5, b7, b9, #9, #11, and b13.
#'
#'   In order to skip an octave you can insert `NA` anywhere in these vectors.
#'   When converting notes to keys, an octave will be skipped for every `NA`.
#'   Leading and trailing `NA`s are ignored. `NA`s have no effect, if `lower`
#'   and `upper` are not given as the function then returns notes which are
#'   not assigned to a specific octave.
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
#' vector of notes (if `upper` and `lower` are not given) or keys (if either
#' `upper` or `lower` is given).
#' The length of each character vector is equal to the number
#' of non-missing values in the inputs for `left` and `right`.
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

  # combine both hands into one vector such that missing values can be
  # handled correctly. Trailing NAs must be handled first.
  left <- trim_na(left, "left")
  right <- trim_na(right, "right")
  i_left <- seq_along(left)
  degrees <- c(left, right)
  degrees_no_na <- vctrs::vec_fill_missing(degrees, direction = "up")

  result <- degrees_to_notes(degrees_no_na, scale = scale)

  # if an upper or lower boundary is given, convert the notes to keys
  if (!is.null(lower) || !is.null(upper)) {
    result <- notes_to_keys(result, lower = lower, upper = upper)
  }

  # split again into left and right hand
  # also remove the notes/keys that correspond to a missing value in the input
  list(
    left = result[i_left][!is.na(left)],
    right = result[-i_left][!is.na(right)]
  )
}
