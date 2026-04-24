# Various internal helper functions

# remove trailing NAs from the beginning and/or end of a vector
trim_na <- function(x, which = c("both", "left", "right")) {
  which <- match.arg(which)

  # return NULL unmodified
  if (is.null(x)) {
    return(x)
  }

  not_na <- which(!is.na(x))

  # special case: when the vector is empty or all values are NA,
  # this must be handled separately
  if (length(not_na) == 0) {
    return(vector(class(x)))
  }
  switch(
    which,
    left = x[min(not_na):length(x)],
    right = x[1:max(not_na)],
    both = x[min(not_na):max(not_na)]
  )
}
