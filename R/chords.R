#' Generate a Chord from Intervals
#'
#' Create a chord voicing by selecting intervals from a major scale with
#' alterations.
#'
#' @param left,right character indicating the intervals to include in
#'   the left and right hand, respectively. Valid values are the interval names
#'   returned by [get_major_scale_with_alt()]: all integers from 1 to 15 and
#'   the alterations b3, b5, #5, b7, b9, #9, #11, and b13.
#' @param root Either a single note name indicating the root key, or the output
#'   of [get_major_scale_with_alt()] for a precomputed scale.
#'
#' @returns
#' A list with components `left` and `right`, each containing a 
#' character vector of note names.
#'
#' @export

get_chord <- function(left = c(), right = c(), root = "C") {
  
  # root can either be a single note or the output of get_major_scale_with_alt()
  len <- length(root)
  scale <- if (len == 1) {
    get_major_scale_with_alt(root)
  } else if (len == 23) {
    root 
  } else {
    cli::cli_abort(
      c(
        "x" = "Invalid input for `root`.",
        "i" = "Must be a single note or the output of `get_major_scale_with_alt()`."
      )
    )
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
