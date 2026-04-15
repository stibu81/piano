#' Generate a Chord from Intervals
#'
#' Create a chord voicing by selecting intervals from a major degrees with
#' alterations.
#'
#' @param left,right character indicating the degrees degrees to include in
#'   the left and right hand, respectively. Valid values are the degrees degrees
#'   returned by [get_major_scale_with_alt()]: all integers from 1 to 15 and
#'   the alterations b3, b5, #5, b7, b9, #9, #11, and b13.
#' @param root_note a single note indicating the root note of the chord. Scale
#'   degrees are interpreted relative to this root note.
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
  # if no scale is given, compute it from the root_note
  if (is.null(scale)) {
    scale <- get_major_scale_with_alt(root_note)
  }

  list(
    left = degrees_to_notes(left, scale = scale),
    right = degrees_to_notes(right, scale = scale)
  )
}
