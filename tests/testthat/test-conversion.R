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


test_that("notes_to_keys() works with lower", {
  expect_equal(
    notes_to_keys(c("Gb", "C", "F", "Bb", "Eb"), lower = "C"),
    c("Gb", "c", "f", "bb", "eb1")
  )
  expect_equal(
    notes_to_keys(c("Gb", "C", "F", "Bb", "Eb"), lower = "f1"),
    c("gb1", "c2", "f2", "bb2", "eb3")
  )
  expect_equal(
    notes_to_keys(c("C", "c", "c"), lower = "c1"),
    c("c1", "c2", "c3")
  )
})


test_that("notes_to_keys() works with upper", {
  expect_equal(
    notes_to_keys(c("Gb", "C", "F", "Bb", "Eb"), upper = "d2"),
    c("Gb", "c", "f", "bb", "eb1")
  )
  expect_equal(
    notes_to_keys(c("Gb", "C", "F", "Bb", "Eb"), upper = "e3"),
    c("gb1", "c2", "f2", "bb2", "eb3")
  )
  expect_equal(
    notes_to_keys(c("C", "c", "c"), upper = "c3"),
    c("c1", "c2", "c3")
  )
})


test_that("notes_to_keys() throws errors", {
  expect_error(notes_to_keys(c("C", "G")), "Exactly one.*specified")
  expect_error(notes_to_keys(c("C", "G"), "c1", "c3"), "Exactly one.*specified")
  expect_error(notes_to_keys(c("C", "G"), lower = "x3"), "x3 is not")
  expect_error(notes_to_keys(c("C", "G"), upper = "x3"), "x3 is not")
})
