# filter keys inbetween a lower and upper limit

filter_key_range <- function(keys, lower, upper, error_call = rlang::caller_env()) {
  # find the key names in the table of white keys. Only white keys are allowed.
  i_lower <- which(keys$white == lower)
  if (length(i_lower) == 0) {
    cli::cli_abort("'{lower}' is not a valid lower key.", call = error_call)
  }
  i_upper <- which(keys$white == upper)
  if (length(i_upper) == 0) {
    cli::cli_abort("'{upper}' is not a valid upper key.", call = error_call)
  }
  if (i_upper <= i_lower) {
    cli::cli_abort("lower must be below upper.", call = error_call)
  }

  # filter the white keys first, then select the black keys that lie inbetween
  white <- keys$white[i_lower:i_upper, ]
  black <- keys$black %>%
    dplyr::filter(.data$xmin > min(white$xmin), .data$xmin < max(white$xmin))

  list(white = white, black = black)
}


# verify that a character vector contains valid key names.
# Since notes are also valid keys, this function also works for them.

verify_keys <- function(
  key_names,
  type = c("all", "white", "black"),
  error_call = rlang::caller_env()
) {
  type <- match.arg(type)

  valid_names <- c(
    if (type %in% c("all", "white")) {
      c(keys_data$white$name, keys_data$white$name_flat, keys_data$white$name_sharp)
    },
    if (type %in% c("all", "black")) {
      c(keys_data$black$name_sharp, keys_data$black$name_flat)
    }
  )

  bad_names <- setdiff(key_names, valid_names)
  if (length(bad_names) > 0) {
    cli::cli_abort(
      "{bad_names} {?is/are} not {?a/} valid note name{?s}."
    )
  }

  return(TRUE)
}


# decompose key name into the note name and the numeric part
decompose_key_names <- function(key_names) {
  list(
    notes = stringr::str_remove(key_names, "\\d"),
    numbers = stringr::str_remove(key_names, "[A-Ga-g#]+")
  )
}
