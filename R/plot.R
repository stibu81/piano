#' Plot a Piano Keyboard
#'
#' @param lower,upper the lowest and highest key of the piano keyboard
#'  to plot in the numbered
#'  [Helmholtz pitch notation](https://en.wikipedia.org/wiki/Helmholtz_pitch_notation).
#'  Both values must correspond to a white key, i.e. a note of the
#'  C major scale, between A2 and c5 and `upper` must be at least one key above
#'  `lower`.
#' @param labels character indicating whether labels with note names should
#'  be plotted. Use `"white"` or `"black"` to only show the labels for white
#'  or black keys, respectively and `"all"` to show labels for all keys.
#' @param black_labels character indicating the names to be used for the labels
#'  of the black keys.
#' @param mark_left,mark_right mark keys for the left and right hand with
#'  coloured dots. Markers that lie outside the plotted keyboard range or omitted.
#' @param colour_left,colour_right the colours to be used to mark keys. By
#'  default, a blue colour is used for the left hand and a red colour for the
#'  right hand.
#'
#' @returns
#' a `ggplot` object
#'
#' @export

plot_piano <- function(lower = "A2", upper = "c5",
                       labels = c("none", "white", "black", "all"),
                       black_labels = c("sharp", "flat"),
                       mark_left = c(),
                       mark_right = c(),
                       colour_left = "deepskyblue",
                       colour_right = "firebrick1") {

  labels <- match.arg(labels)
  black_labels <- match.arg(black_labels)

  # filter the range of keys. For some reason, ggplot does not manage to plot
  # a single rectangle. However, doubling the black key works.
  keys_f <- filter_key_range(keys, lower, upper)
  if (nrow(keys_f$black) == 1) {
    keys_f$black <- dplyr::bind_rows(keys_f$black, keys_f$black)
  }

  # don't put the aesthetic mappings inside ggplot() because drawing the dots
  # below need different mappings.
  piano_plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data[["xmin"]], width = .data[["width"]],
                   ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
      data = keys_f$white, colour = "black", fill = "white"
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data[["xmin"]], width = .data[["width"]],
                   ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
      data = keys_f$black, colour = "black", fill = "black"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()

  piano_plot <- add_key_labels(piano_plot, keys_f, labels, black_labels)

  # prepare the markers for each hand
  x_range = range(keys_f$white$xmin)
  markers <- dplyr::bind_rows(
      get_key_markers(mark_left) %>%
        dplyr::mutate(hand = "left"),
      get_key_markers(mark_right) %>%
        dplyr::mutate(hand = "right")
    ) %>%
    # drop markers that are outside of the keyboard range
    dplyr::filter(dplyr::between(.data$x, x_range[1], x_range[2]))

  # add the markers. Draw them as circles, not points, in order to be able to
  # specify their size in the same units as the key width.
  piano_plot <- piano_plot +
    ggforce::geom_circle(
      ggplot2::aes(
        x0 = .data[["x"]],
        y0 = .data[["y"]],
        fill = .data[["hand"]],
        r = 0.25
      ),
      data = markers,
      colour = NA
    ) +
    ggplot2::scale_fill_manual(
      values = c("left" = colour_left, right = colour_right),
      guide = "none"
    )

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
