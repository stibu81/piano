test_that("get_chord() works with a root note", {
  expect_equal(
    get_chord(left = c("1", "b3", "5"), right = c("8", "b13"), root = "C"),
    list(
      left = c("C", "Eb", "G"),
      right = c("C", "Ab")
    )
  )
})


test_that("get_chord() works with a precomputed scale", {
  scale <- get_major_scale_with_alt("E")

  expect_equal(
    get_chord(left = c("1", "5"), right = c("8", "9", "#11"), root = scale),
    list(
      left = c("E", "B"),
      right = c("E", "F#", "A#")
    )
  )
})


test_that("get_chord() error messages work", {
  expect_error(
    get_chord(root = c("C", "D")),
    "Invalid input for `root`"
  )
  expect_error(
    get_chord(left = "16"),
    "`left` and `right` must only contain valid intervals."
  )
})
