#' Generate a Chord from Intervals
#'
#' Create a chord voicing by selecting intervals from a major degrees with
#' alterations.
#'
#' @param left,right character indicating the degrees degrees to include in
#'   the left and right hand, respectively. Valid values are the degrees degrees
#'   returned by [get_major_scale_with_alt()]: all integers from 1 to 15 and
#'   the alterations b3, b5, #5, b7, b9, #9, #11, and b13.
#' @param root_note a single note indicating the root note of the chord.
#' @param scale the output of [get_major_scale_with_alt()] for the key corresponding
#'   to the desired root note. This is mainly provided to improve performance, if many
#'   chords with the same root need to be computed, since `get_major_scale_with_alt()`
#'   is where the functions spends most its time.
#'
#' @returns
#' A list with components `left` and `right`, each containing a
#' character vector of notes.
#'
#' @export

get_chord <- function(left = c(), right = c(), root_note = "C", scale = NULL) {
  # if no scale is given, compute it from the root
  if (is.null(scale)) {
    scale <- get_major_scale_with_alt(root_note)
  }

  # make sure that left and right are characters and are valid
  left <- as.character(left)
  right <- as.character(right)
  if (any(!c(left, right) %in% names(scale))) {
    cli::cli_abort("`left` and `right` must only contain valid intervals.")
  }

  list(
    left = unname(scale[left]),
    right = unname(scale[right])
  )
}
