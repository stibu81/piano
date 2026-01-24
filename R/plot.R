#' Plot a Piano Keyboard
#'
#' @returns
#' a `ggplot` object
#'
#' @export

plot_piano <- function() {
  piano_plot <- ggplot2::ggplot(
      mapping = ggplot2::aes(xmin = .data[["xmin"]], width = .data[["width"]],
                             ymin = .data[["ymin"]], ymax = .data[["ymax"]])
    ) +
    ggplot2::geom_rect(data = keys$white, colour = "black", fill = "white") +
    ggplot2::geom_rect(data = keys$black, colour = "black", fill = "black") +
    ggplot2::coord_equal() +
    ggplot2::theme_void()

  piano_plot
}
