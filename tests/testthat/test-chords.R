test_that("get_chord() works with a root note", {
  expect_equal(
    get_chord(left = c("1", "b3", "5"), right = c("8", "b13"), root = "D"),
    list(
      left = c("D", "F", "A"),
      right = c("D", "Bb")
    )
  )
})


test_that("get_chord() works with a precomputed scale", {
  scale <- get_major_scale_with_alt("E")

  expect_equal(
    get_chord(left = c("1", "5"), right = c("8", "9", "#11"), scale = scale),
    list(
      left = c("E", "B"),
      right = c("E", "F#", "A#")
    )
  )
})
