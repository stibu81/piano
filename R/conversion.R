#' Convert Between Scale Degrees, Notes, and Keys
#'
#' @param degrees character vector of scale degrees. Valid values are the degrees
#'   returned by [get_major_scale_with_alt()]: all integers from 1 to 15 and
#'   the alterations b3, b5, #5, b7, b9, #9, #11, and b13.
#' @param notes character vector of notes. They will be converted to upper case.
#' @param root_note a single note indicating the root note relative to which the
#'   scale degrees are interpreted.
#' @param scale the output of [get_major_scale_with_alt()] for the key corresponding
#'   to the desired root note. This is mainly provided to improve performance, if many
#'   chords with the same root need to be computed, since `get_major_scale_with_alt()`
#'   is where the functions spends most its time.
#' @param upper,lower a single key that is used for the conversion to keys.
#'   Key will be selected such that they all lie above (below) `upper` (`lower`).
#'
#' @returns
#' a character vector of the same length as `notes` or `degrees`, respectively.
#'
#' @export

degrees_to_notes <- function(degrees, root_note = "C", scale = NULL) {
  # if no scale is given, compute it from the root_note
  if (is.null(scale)) {
    scale <- get_major_scale_with_alt(root_note)
  }

  # check that degrees are valid
  degrees <- as.character(degrees)
  if (any(!degrees %in% names(scale))) {
    cli::cli_abort("`degrees` must only contain valid scale degrees.")
  }

  unname(scale[degrees])
}


#' @rdname degrees_to_notes
#' @export

notes_to_keys <- function(notes, lower = NULL, upper = NULL) {
  if ((is.null(lower) && is.null(upper)) || (!is.null(lower) && !is.null(upper))) {
    cli::cli_abort("Exactly one of `lower` and `upper` must be specified")
  }

  is_upper <- is.null(lower)
  limit <- if (is_upper) upper else lower
  do_notes_to_keys(notes, limit, notes_keys_data, is_upper = is_upper)
}


# helper function for the actual conversion
do_notes_to_keys <- function(notes, limit, mapping, is_upper = FALSE) {
  verify_keys(limit)

  # if the limit is "upper", some objects must be reversed.
  # => define a transformation function that is either rev() or identity
  tr <- if (is_upper) rev else \(x) x
  notes <- tr(notes_toupper(notes))
  # using vectors instead of a data frame is much faster
  mkey <- tr(mapping$key)
  mnote <- tr(mapping$note)

  keys <- character(length(notes))
  i_key <- which(mkey == limit) - 1
  for (i in seq_along(notes)) {
    mkey <- mkey[(i_key + 1):length(mkey)]
    mnote <- mnote[(i_key + 1):length(mnote)]
    i_key <- min(which(mnote == notes[i]))
    keys[i] <- mkey[i_key]
  }

  tr(keys)
}
