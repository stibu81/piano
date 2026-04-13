# Get the coordinates for key markers

get_key_markers <- function(keys = c()) {
  # to avoid unnecessary computations, return immediately for empty keys
  if (length(keys) == 0) {
    return(dplyr::tibble(name = character(0), x = numeric(0), y = numeric(0)))
  }

  verify_keys(keys)

  # prepare the data for the relevant keys
  markers <- dplyr::bind_rows(
    keys_data$white,
    keys_data$black %>%
      dplyr::rename(name = "name_sharp"),
    keys_data$black %>%
      dplyr::rename(name = "name_flat")
  ) %>%
    dplyr::filter(.data$name %in% keys)

  # compute the coordinates for the dots
  markers <- markers %>%
    dplyr::mutate(
      x = .data$xmin + .data$width / 2,
      y = .data$ymin + 1.1
    ) %>%
    dplyr::select("name", "x", "y")

  markers
}
