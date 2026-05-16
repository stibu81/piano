#' Read a Chord Database from a csv File
#'
#' @param file the path to a csv file containing the chord database. The column
#' delimiter must be a comma (`,`). See 'Details' for more information on the
#' file format.
#'
#' @details
#' The csv file must contain the column titles in the first row. Each row contains
#' the data for one chord voicing. Note that voicings are defined by their scale
#' degrees not by specific notes or even keys. The file must contain all the required
#' columns, but some values are allowed to be empty. The order of the columns is
#' irrelevant and additional columns are allowed but will be ignored in the output.
#'
#' The following lists the 5 required columns and specifies whether a value must
#' be provided or is optional:
#'
#' * "type" (required): the type of chord, one of "major", "minor", "dominant",
#'   "half-diminished", "diminished" or "augmented". The function checks that a valid
#'   value is passed and verifies that the scale degrees match the type
#'   (see [is_valid_chord()]).
#' * "notation" (optional): notation for the chord (e.g., "maj7" or "7"). This is simply
#'   used as an identifier and any value is allowed.
#' * "name" (optional): a name for the voicing. This is simply used as an identifier
#'    and any value is allowed.
#' * "left", "right" (required): the scale degrees for the left and right hand
#'    separated by spaces (e.g., "3 b7 9"). At least on of the two must have a value,
#'    but it is allowed to leave the other empty. This allows to define left-hand
#'    voicings. The function checks that all scale degrees are valid and plausible
#'    for the given chord type.
#'
#' @returns
#' a tibble with one row for each chord. It contains all the columns from the csv file
#' ("type", "notation", "name", "left", "right") and the column that are returned
#' by [analyse_chord()] ("rooted", "left_hand", "top_degree", "alterations").
#'
#'
#' @export

read_chords_csv <- function(file) {
  raw <- readr::read_csv(file, col_types = readr::cols(.default = "c"))

  # check that all relevant columns are present
  db_cols <- c("type", "notation", "name", "left", "right")
  if (!all(db_cols %in% names(raw))) {
    cli::cli_abort("The csv file must contain the columns {db_cols}.")
  }

  # check the chord types
  valid_types <- c(
    "major",
    "minor",
    "dominant",
    "half-diminished",
    "diminished",
    "augmented"
  )
  if (any(!raw$type %in% valid_types)) {
    cli::cli_abort(
      "The file contains invalid chord types. Valid types are {valid_types}."
    )
  }

  db <- raw[db_cols] %>%
    dplyr::mutate(
      left = split_degrees(.data$left),
      right = split_degrees(.data$right)
    )

  # process the chords one by one
  analysed_chords <- db %>%
    dplyr::nest_by(i = dplyr::row_number(), .keep = TRUE) %>%
    dplyr::reframe(process_chord(.data$data)) %>%
    dplyr::select(-"i")

  db <- dplyr::bind_cols(db, analysed_chords)

  db
}


process_chord <- function(row, error_call = rlang::caller_env()) {
  left <- row$left[[1]]
  right <- row$right[[1]]
  is_valid <- is_valid_chord(
    left = left,
    right = right,
    type = row$type[[1]]
  )
  if (!is_valid) {
    cli::cli_abort(
      "The chord number {row$i[[1]]} contains invalid scale degrees.",
      call = error_call
    )
  }
  analyse_chord(left = left, right = right)
}

# helper function to split a string of scale degrees separated by spaces.
# NA is converted to an empty character vector
split_degrees <- function(degrees) {
  split <- stringr::str_split(degrees, "\\s")
  split[is.na(degrees)] <- list(character(0))
  split
}
