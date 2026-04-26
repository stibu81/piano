#' Sort a Vector of Scale Degrees
#'
#' Sort a character vector of scale degrees.
#'
#' @param degrees character vector of scale degrees. The function does not
#'  check whether all scale degrees are valid for performance reasons.
#' @param decreasing logical indicating the sort order. Default is `FALSE`
#'  which implies increasing order.
#'
#' @returns
#' the sorted character of scale degrees.
#'
#' @export

sort_degrees <- function(degrees, decreasing = FALSE) {
  degrees[order(rank_degrees(degrees), decreasing = decreasing)]
}

# helper function to rank scale degrees.
rank_degrees <- function(degrees) {
  as.numeric(stringr::str_remove(degrees, "^(#|b)")) +
    0.4 * stringr::str_detect(degrees, "#") -
    0.4 * stringr::str_detect(degrees, "b")
}
