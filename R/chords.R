#' Generate the Notes or Keys of a Chord from Scale Degrees
#'
#' Create a chord voicing by selecting scale degrees from a major scale with
#' alterations.
#'
#' @param left,right character indicating the scale degrees to include in
#'   the left and right hand, respectively. Valid values are the scale degrees
#'   returned by [get_major_scale_with_alt()]: 1, 2, b3, 3, 4, b5, 5, #5, 6, b7, 7,
#'   b9, 9, #9, 11, #11, b13, 13.
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


#' Analyse a Chord Specified as Scale Degrees
#'
#' Analyse a chord that is specified as scale degrees for the left and
#' right hand.
#'
#' @param left,right character vectors of scale degrees for the left and
#'  right hand.
#'
#' @returns
#' A tibble with one row and the following columns:
#'
#' * `rooted`: is it a rooted chord, i.e., is the first note in the left hand
#'   the root.
#' * `left_hand`: is it a left-handed chord, i.e., is the right hand empty.
#' * `top_degree`: scale degree of the top note in the chord. For a left handed chord,
#'   this is the top note of the left hand.
#' * `alterations`: character vector of alterations used in the chord, sorted in
#'  increasing order.
#'
#' @export

analyse_chord <- function(left = character(), right = character()) {
  left_hand <- length(right) == 0
  if (left_hand && length(left) == 0) {
    cli::cli_abort("At least one hand must contain some scale degrees")
  }
  rooted <- length(left) >= 1 && left[1] == "1"
  top_degree <- if (left_hand) utils::tail(left, n = 1) else utils::tail(right, n = 1)

  # determine and sort the alterations
  alterations <- stringr::str_subset(c(left, right), "^#|b") %>%
    # flat 3 and flat 7 are not alterations
    setdiff(c("b3", "b7")) %>%
    sort_degrees()

  dplyr::tibble(
    rooted = rooted,
    left_hand = left_hand,
    top_degree = top_degree,
    alterations = list(alterations)
  )
}


#' Check Plausibility of the Scale Degrees in a Chord
#'
#' Check that the scale degrees contained in a chord are plausible for the
#' given type. For example, a chord of type "dominant" should not contain
#' b3 or 7. The function only checks the absence of implausible scale degrees,
#' but not the presence of expected ones. So, a "dominant" chord is considered
#' valid even if 3 and/or b7 are missing. This allows to, say, also define a
#' sus4 chord as a "dominant" chord. See 'Details' for more information.
#'
#' @inheritParams analyse_chord
#' @param type the type of chord that was entered. One of "major", "minor",
#'  "dominant", "half-diminished", "diminished", or "augmented". The type determines
#'   which degrees are invalid. See 'Details' for more information.
#'
#' @details
#' In general, the following scale degrees are allowed in chords:
#' 1, 2, b3, 3, 4, b5, 5, #5, 6, b7, 7, b9, 9, #9, 11, #11, b13, 13.
#'
#' Depending on the chord type, some degrees are not allowed:
#' * "major": b3, b7, b5
#' * "minor": 3
#' * "dominant": b3, 7
#' * "half-diminished": 3, 5, 7
#' * "diminished": 3, 5, 7
#' * "augmented": b3, 5
#'
#' @returns
#' a logical indicating whether the chord only contains valid scale degrees.
#'
#' @export

is_valid_chord <- function(
  left = character(),
  right = character(),
  type = c("major", "minor", "dominant", "half-diminished", "diminished", "augmented")
) {
  type <- match.arg(type)

  # depnding on the type, some degrees should not be in the chord
  forbidden_degrees <- switch(
    type,
    "major" = c("b3", "b7", "b5"),
    "minor" = c("3"),
    "dominant" = c("b3", "7"),
    "half-diminished" = c("3", "5", "7"),
    "diminished" = c("3", "5", "7"),
    "augmented" = c("b3", "5")
  )
  valid_degrees <- setdiff(valid_chord_degrees_data, forbidden_degrees)

  all(c(left, right) %in% valid_degrees)
}
