test_that("notes_toupper() works", {
  expect_equal(
    notes_toupper(c("a", "C1", "d#", "gb2", "bb", "eb3")),
    c("A", "C1", "D#", "Gb2", "Bb", "Eb3")
  )
})

test_that("notes_tolower() works", {
  expect_equal(
    notes_tolower(c("a", "C1", "D#", "Gb2", "Bb", "Eb3")),
    c("a", "c1", "d#", "gb2", "bb", "eb3")
  )
})
