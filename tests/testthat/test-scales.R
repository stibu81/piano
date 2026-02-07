test_that("get_major_scale() works for one octave", {
  expect_equal(get_major_scale("C"),
               c("C", "D", "E", "F", "G", "A", "B", "C"))
  expect_equal(get_major_scale("E"),
               c("E", "F#", "G#", "A", "B", "C#", "D#", "E"))
  expect_equal(get_major_scale("Ab"),
               c("Ab", "Bb", "C", "Db", "Eb", "F", "G", "Ab"))
  expect_equal(get_major_scale("Gb"),
               c("Gb", "Ab", "Bb", "Cb", "Db", "Eb", "F", "Gb"))
  expect_equal(get_major_scale("F#"),
               c("F#", "G#", "A#", "B", "C#", "D#", "E#", "F#"))
})


test_that("get_major_scale() works for multiple octaves", {
  for (n in c(2, 3, 5, 10)) {
    expect_equal(
      get_major_scale("C", n),
      c("C", rep(c("D", "E", "F", "G", "A", "B", "C"), n))
    )
  }
  expect_equal(get_major_scale("Eb", 2.7), get_major_scale("Eb", 2))
})


test_that("error messages for get_major_scale() work", {
  expect_error(get_major_scale("Bb", "1"), "'n_octave' must be a numeric value.")
  expect_error(get_major_scale("D#"), "'D#' is not a valid key")
  expect_error(get_major_scale("C", 0), "'n_octave' must be a positive integer")
})
