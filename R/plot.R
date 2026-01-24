#' Plot a Piano Keyboard
#'
#' @param lower,upper the lowest and hightes key of the piano keyboard
#'  to plot. Both values must correspond to a white key, i.e. a note of the
#'  C major scale, between A2 and c5 and `upper` must be at least one key above
#'  `lower`.
#' @param labels character indicating whether labels with note names should
#'  be plotted. Use `"white"` or `"black"` to only show the laels for white
#'  or black keys, respectively and `"all"` to show labels for all keys.
#' @param black_labels character indicating the names to be used for the labels
#'  of the black keys.
#'
#' @returns
#' a `ggplot` object
#'
#' @export

plot_piano <- function(lower = "A2", upper = "c5",
                       labels = c("none", "white", "black", "all"),
                       black_labels = c("sharp", "flat")) {


  labels <- match.arg(labels)
  black_labels <- match.arg(black_labels)

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

  piano_plot <- add_key_labels(piano_plot, keys_f, labels, black_labels)

  piano_plot
}



add_key_labels <- function(plot, keys_f, labels, black_labels) {

  if (labels == "none") return(plot)

  if (labels %in% c("white", "all")) {
    plot <- plot +
      ggplot2::geom_text(
        data = keys_f$white,
        ggplot2::aes(x = .data[["xmin"]] + .data[["width"]] / 2,
                     y = -.6, label = .data[["name"]])
      )
  }
  # if black keys labels are to be shown, their y coordinate depends on whether
  # also the white key labels are shown.
  if (labels %in% c("black", "all")) {
    plot <- plot +
      ggplot2::geom_text(
        data = keys_f$black,
        ggplot2::aes(x = .data[["xmin"]] + .data[["width"]] / 2,
                     y = if (labels == "all") -1.4 else -.6,
                     label = .data[[paste0("name_", black_labels)]])
      )
  }

  # to prevent labels from beeing cut off, add a transparent point annotation
  plot <- plot +
    ggplot2::annotate("point", x = min(keys_f$white$xmin),
                      y = if (labels == "all") -1.5 else -.7,
                      alpha = 0)

  plot
}
