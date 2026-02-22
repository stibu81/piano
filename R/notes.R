#' Flatten or Sharpen a Note
#' 
#' A character vector of note names is flattened or sharpened. Double
#' accidentals are converted to notes in the C major scales.
#' 
#' @param notes a character vector of valid note names.
#' 
#' @returns
#' a character vector of valid note names that are either a half-tone
#' lower or higher than `notes`.
#' 
#' @export

flatten <- function(notes) {

  verify_key_names(notes)
  c_major_scale <- get_major_scale("C", 2L)

  # there are multiple types of notes that need to be treated separately:
  # 1. notes containing a sharp => remove the sharp
  # 2. notes without an accidental => add a flat
  # 3. notes with a flat corresponding to a black key
  #   => enharmonic equivalent can be handled like 1.
  # 4. notes with a flat corresponding to a white key, i.e. Fb and Cb
  #   => enharmonic equivalent can be handled like 2.
  has_flat <- has_accidental(notes, "flat")
  notes[has_flat] <- get_enharmonic_equivalent(notes[has_flat])
  dplyr::case_when(
    has_accidental(notes, "sharp") ~ stringr::str_remove(notes, "#"),
    !has_accidental(notes, "flat") ~ add_accidental(notes, "b")
  )
}


#' @rdname flatten
#' @export

sharpen <- function(notes) {

  verify_key_names(notes)
  c_major_scale <- get_major_scale("C", 2L)

  # there are multiple types of notes that need to be treated separately:
  # 1. notes containing a flat => remove the flat
  # 2. notes without an accidental => add a sharp
  # 3. notes with a sharp corresponding to a black key
  #   => enharmonic equivalent can be handled like 1.
  # 4. notes with a sharp corresponding to a white key, i.e. Fb and Cb
  #   => enharmonic equivalent can be handled like 2.
  has_sharp <- has_accidental(notes, "sharp")
  notes[has_sharp] <- get_enharmonic_equivalent(notes[has_sharp])
  dplyr::case_when(
    has_accidental(notes, "flat") ~ stringr::str_remove(notes, "b"),
    !has_accidental(notes, "sharp") ~ add_accidental(notes, "#")
  )
}


# helper function to add an accidental to a note
add_accidental <- function(notes, accidental) {
  stringr::str_replace(notes, "^([A-Ga-g])", paste0("\\1", accidental))
}


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
#' @param use_double_accidentals logical indicating whether enharmonic
#'  equivalents involving double accidentals (i.e. D => C##) should be
#'  returned. For white keys that have an enharmonic equivalent with
#'  only accidental that one is preferred independent of the setting of
#'  `use_double_accidentals`. E.g., for C, B# is preferred over Dbb.
#' 
#' @returns
#' character vector with note names representing the enharmonic
#' equivalents of `notes`.
#' 
#' @export

get_enharmonic_equivalent <- function(
  notes,
  use_double_accidentals = c("none", "sharp", "flat")
) {

  verify_key_names(notes)
  use_double_accidentals <- match.arg(use_double_accidentals)
  
  # compile a table of enharmonic equivalents
  equiv_table <- dplyr::bind_rows(
    keys_data$white %>% 
      dplyr::select(name = "name_sharp", equiv = "name"),
    keys_data$white %>% 
      dplyr::select(name = "name_flat", equiv = "name"),
    keys_data$white %>% 
      dplyr::select(name = "name", equiv = "name_sharp"),
    keys_data$white %>% 
      dplyr::select(name = "name", equiv = "name_flat"),
    keys_data$black %>% 
      dplyr::select(name = "name_sharp", equiv = "name_flat"),
    keys_data$black %>% 
      dplyr::select(name = "name_flat", equiv = "name_sharp")
  )

  # filter the equivalents depending on the choice for the double accidentals
  # create a pattern that matches the double accidentals to DROP.
  dbl_acc_pattern <- paste0(
    "^[A-Ga-g](",
    if (use_double_accidentals %in% c("none", "sharp")) "bb",
    if (use_double_accidentals == "none") "|",
    if (use_double_accidentals %in% c("none", "flat")) "##",
    ")"
  )
  equiv_table <- equiv_table %>% 
    dplyr::filter(!stringr::str_detect(.data$equiv, dbl_acc_pattern))
  
  # if a note has two equivalents, prefer the one with a single accidental
  # notes with double accidentals contain three non-numeric characters.
  if (use_double_accidentals != "none") {
    dups <- equiv_table$name[duplicated(equiv_table$name)]
    equiv_table <- equiv_table %>% 
      dplyr::filter(
        !.data$name %in% dups |
          stringr::str_count(equiv_table$equiv, "[^1-9]") < 3
      )
  }

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


#' Does a Note Name Contain an Accidental
#' 
#' @param notes a vector of notes names
#' @param which character indicating the type of accidental to look for.
#'  One of "any", "sharp", "flat".
#' @param number character indicating the number of accidentals that are expected.
#'  One of "any", "single", "double".
#' 
#' @return
#' a logical vector indicating whether the note names in `notes`
#' contain an accidental.
#' 
#' @export

has_accidental <- function(notes,
which = c("any", "sharp", "flat"),
                           number = c("any", "single", "double")) {

  which <- match.arg(which)
number <- match.arg(number)

  verify_key_names(notes)

  # create the pattern for the accidentals: first, pick the appropriate
  # accidental, afterwards fix the multiplier.
  which_pattern <- switch(which,
                        "any" = "(b|#)",
                        "sharp" = "#",
                        "flat" = "b")
number_pattern <- switch(number,
                           "any" = "",
                           "single" = "($|[^#b])",
                           "double" = "{2}")
  
  stringr::str_detect(notes, paste0("^[A-Ga-g]", which_pattern, number_pattern))

}
