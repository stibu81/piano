# Get the coordinates for key markers

get_key_markers <- function(key_names = c()) {

  # to avoid unnecessary computations, return immediately for empty key_names
  if (length(key_names) == 0) {
    return(dplyr::tibble(name = character(0), x = numeric(0), y = numeric(0)))
  }

  verify_key_names(key_names)

  # prepare the data for the relevant keys
  markers <- dplyr::bind_rows(
      keys$white,
      keys$black %>%
        dplyr::rename(name = "name_sharp"),
      keys$black %>%
        dplyr::rename(name = "name_flat")
    ) %>%
    dplyr::filter(.data$name %in% key_names)

  # compute the coordinates for the dots
  markers <- markers %>%
    dplyr::mutate(
      x = .data$xmin + .data$width / 2,
      y = .data$ymin + 1.1
    ) %>%
    dplyr::select("name", "x", "y")

  markers
}
