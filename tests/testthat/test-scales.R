test_that("get_major_scale() works for one octave", {
  expect_equal(
    get_major_scale("C"),
    setNames(c("C", "D", "E", "F", "G", "A", "B", "C"), 1:8)
  )
  expect_equal(
    get_major_scale("E"),
    setNames(c("E", "F#", "G#", "A", "B", "C#", "D#", "E"), 1:8)
  )
  expect_equal(
    get_major_scale("Ab"),
    setNames(c("Ab", "Bb", "C", "Db", "Eb", "F", "G", "Ab"), 1:8)
  )
  expect_equal(
    get_major_scale("Gb"),
    setNames(c("Gb", "Ab", "Bb", "Cb", "Db", "Eb", "F", "Gb"), 1:8)
  )
  expect_equal(
    get_major_scale("F#"),
    setNames(c("F#", "G#", "A#", "B", "C#", "D#", "E#", "F#"), 1:8)
  )
  expect_equal(
    get_major_scale("G#"),
    setNames(c("G#", "A#", "B#", "C#", "D#", "E#", "F##", "G#"), 1:8)
  )
  expect_equal(
    get_major_scale("A#"),
    setNames(c("A#", "B#", "C##", "D#", "E#", "F##", "G##", "A#"), 1:8)
  )
})


test_that("get_major_scale() works for multiple octaves", {
  for (n in c(2, 3, 5, 10)) {
    expect_equal(
      get_major_scale("C", n),
      setNames(
        c("C", rep(c("D", "E", "F", "G", "A", "B", "C"), n)),
        seq_len(7 * n + 1)
      )
    )
  }
  expect_equal(get_major_scale("Eb", 2.7), get_major_scale("Eb", 2))
})


test_that("error messages for get_major_scale() work", {
  expect_error(get_major_scale("Bb", "1"), "'n_octaves' must be a numeric value")
  expect_error(get_major_scale("B#"), "'B#' is not a valid tonic")
  expect_error(get_major_scale("C", 0), "'n_octaves' must be a positive integer")
})


test_that("get_major_scale_with_alt() works for one octave", {
  # fmt: skip
  alt_names <- c("1", "2", "b3", "3", "4", "b5", "5", "#5", "6", "b7", "7", "8",
                 "b9", "9", "#9", "10", "11", "#11", "12", "b13", "13", "14", "15")
  expect_equal(
    get_major_scale_with_alt("C"),
    setNames(
      # fmt: skip
      c("C", "D", "Eb", "E", "F", "Gb", "G", "G#", "A", "Bb", "B", "C",
        "Db", "D", "D#", "E", "F", "F#", "G", "Ab", "A", "B", "C"),
      alt_names
    )
  )
  expect_equal(
    get_major_scale_with_alt("E"),
    setNames(
      # fmt: skip
      c("E", "F#", "G", "G#", "A", "Bb", "B", "B#", "C#", "D", "D#", "E",
        "F", "F#", "F##", "G#", "A", "A#", "B", "C", "C#", "D#", "E"),
      alt_names
    )
  )
  expect_equal(
    get_major_scale_with_alt("Ab"),
    setNames(
      # fmt: skip
      c("Ab", "Bb", "Cb", "C", "Db", "Ebb", "Eb", "E", "F", "Gb", "G", "Ab",
        "Bbb", "Bb", "B", "C", "Db", "D", "Eb", "Fb", "F", "G", "Ab"),
      alt_names
    )
  )
})
