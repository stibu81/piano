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


test_that("get_chord() generates correct keys", {
  expect_equal(
    get_chord(left = c("1", "b3", "5"), right = c("8", "b13"), root = "D", lower = "c"),
    list(
      left = c("d", "f", "a"),
      right = c("d1", "bb1")
    )
  )
  expect_equal(
    get_chord(left = c("1", "1"), right = c("5", "1", "3"), root = "E", upper = "c2"),
    list(
      left = c("E", "e"),
      right = c("b", "e1", "g#1")
    )
  )
})


test_that("get_chord() skips keys with NA in different positions", {
  expect_equal(
    get_chord(
      left = c("1", "b3", NA, "5"),
      right = c("8", NA, "b13"),
      root = "D",
      lower = "c"
    ),
    list(
      left = c("d", "f", "a1"),
      right = c("d2", "bb3")
    )
  )
  expect_equal(
    get_chord(
      left = c("1", "1", NA),
      right = c("5", "1", "3"),
      root = "E",
      upper = "c2"
    ),
    list(
      left = c("E1", "E"),
      right = c("b", "e1", "g#1")
    )
  )
  expect_equal(
    get_chord(
      left = c("1", "1"),
      right = c(NA, "5", "1", "3"),
      root = "E",
      upper = "c2"
    ),
    list(
      left = c("E1", "E"),
      right = c("b", "e1", "g#1")
    )
  )
  expect_equal(
    get_chord(
      left = c("1", "1", NA, NA),
      right = c(NA, "5", "1", "3"),
      root = "E",
      lower = "C"
    ),
    list(
      left = c("E", "e"),
      right = c("b3", "e4", "g#4")
    )
  )
  expect_equal(
    get_chord(
      left = c(NA, "1", "1"),
      right = c("5", "1", "3", NA),
      root = "E",
      upper = "c2"
    ),
    list(
      left = c("E", "e"),
      right = c("b", "e1", "g#1")
    )
  )
})
