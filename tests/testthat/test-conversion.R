test_that("degrees_to_notes() works with a root note", {
  expect_equal(
    degrees_to_notes(c("2", "#11", "b7", "10", "b13"), "Ab"),
    c("Bb", "D", "Gb", "C", "Fb")
  )
})


test_that("degrees_to_notes() works with a precomputed scale", {
  scale <- get_major_scale_with_alt("G")
  expect_equal(
    degrees_to_notes(c("2", "b5", "7", "b9", "#11"), scale = scale),
    c("A", "Db", "F#", "Ab", "C#")
  )
})


test_that("degrees_to_notes() throws error for invalid degrees", {
  expect_error(
    degrees_to_notes(c("2", "b11", "7"), "A"),
    "`degrees` must only contain valid scale degrees"
  )
})
