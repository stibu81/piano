#' Get the Enharmonic Equivalent of a Note
#' 
#' This function returns the enharmonic equivalent of notes. Notes with 
#' accidentals are either changed to a note with the other type of 
#' accidental (e.g., F# => Gb, Bb => A#) or to a note without 
#' accidental (e.g., Cb => B, E# => F). Notes without accidentals are 
#' left unchanged unless there is a enharmonic equivalent with one
#' accidental (e.g., B => Cb, F => E#).
#' 
#' @param notes a vector of valid note names.
#' 
#' @returns
#' character vector with note names representing the enharmonic
#' equivalents of `notes`.
#' 
#' @export

get_enharmonic_equivalent <- function(notes) {

  verify_key_names(notes)

  # compile a table of enharmonic equivalents
  equiv_table <- dplyr::bind_rows(
    keys_data$white %>% 
      dplyr::select("name", equiv = "enh_equiv") %>% 
      dplyr::filter(.data$equiv != ""),
    keys_data$white %>% 
      dplyr::select(name = "enh_equiv", equiv = "name") %>% 
      dplyr::filter(.data$name != ""),
    keys_data$black %>% 
      dplyr::select(name = "name_sharp", equiv = "name_flat"),
    keys_data$black %>% 
      dplyr::select(name = "name_flat", equiv = "name_sharp")
  )

  # use a join to determine the equivalents. If no result is found
  # in the table, keep the note as it is.
  dplyr::tibble(name = notes) %>% 
    dplyr::left_join(equiv_table, by = "name") %>% 
    dplyr::mutate(equiv = dplyr::coalesce(.data$equiv, .data$name)) %>% 
    dplyr::pull("equiv")

}


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
