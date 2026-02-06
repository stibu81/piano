#' Convert Note Names to Uppercase or Lowercase
#'
#' Convert note names to upper case or lower case and ensure that the accidental
#' "b" remains lower case.
#'
#' @param notes character vector of note names. The function does not check
#'  whether these are valid names.
#'
#' @returns
#' a character vector with transformed note names
#'
#'  @export

notes_toupper <- function(notes) {
  toupper(notes) %>%
    # replace all Bs that appear after an upper case note name to lower case
    stringr::str_replace("([A-G])B", "\\1b")
}

#' @rdname notes_toupper
#' @export

notes_tolower <- function(notes) {
  tolower(notes)
}
