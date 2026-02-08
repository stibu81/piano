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

test_that("get_enharmonic_equivalent() works", {
  expect_equal(
    get_enharmonic_equivalent(
      c("C1", "d", "f#2", "bb1", "F", "G", "db2", "a4")
    ),
    c("B#2", "d", "gb2", "a#1", "E#", "G", "c#2", "a4")
  )
})
