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


test_that("has_accidental() works", {
  expect_equal(
    has_accidental(c("C#1", "Bb1", "d3", "eb", "bb3", "g2", "Bb", "bb", "f#2")),
    c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
  )
  expect_equal(
    has_accidental(c("C#1", "Bb1", "d3", "eb", "bb3", "g2", "Bb", "bb", "f#2"),
                   "flat"),
    c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  )
  expect_equal(
    has_accidental(c("C#1", "Bb1", "d3", "eb", "bb3", "g2", "Bb", "bb", "f#2"),
                   "sharp"),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})
