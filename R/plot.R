#' Plot a Piano Keyboard
#'
#' @param lower,upper the lowest and hightes key of the piano keyboard
#'  to plot. Both values must correspond to a white key, i.e. a note of the
#'  C major scale, between A2 and c5 and `upper` must be at least one key above
#'  `lower`.
#'
#' @returns
#' a `ggplot` object
#'
#' @export

plot_piano <- function(lower = "A2", upper = "c5") {
  # filter the range of keys. For some reason, ggplot does not manage to plot
  # a single rectangle. However, doubling the black key works.
  keys_f <- filter_keys(lower, upper)
  if (nrow(keys_f$black) == 1) {
    keys_f$black <- dplyr::bind_rows(keys_f$black, keys_f$black)
  }

  piano_plot <- ggplot2::ggplot(
      mapping = ggplot2::aes(xmin = .data[["xmin"]], width = .data[["width"]],
                             ymin = .data[["ymin"]], ymax = .data[["ymax"]])
    ) +
    ggplot2::geom_rect(data = keys_f$white, colour = "black", fill = "white") +
    ggplot2::geom_rect(data = keys_f$black, colour = "black", fill = "black") +
    ggplot2::coord_equal() +
    ggplot2::theme_void()

  piano_plot
}
