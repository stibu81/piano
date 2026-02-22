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
  # without double accidentals
  expect_equal(
    get_enharmonic_equivalent(
      c("C1", "d", "f#2", "bb1", "F", "G", "e1", "gbb1", "db2", "f##3", "a4")
    ),
    c("B#2", "d", "gb2", "a#1", "E#", "G", "fb1", "f1", "c#2", "g3", "a4")
  )
  # with double flats
  expect_equal(
    get_enharmonic_equivalent(
      c("C1", "d", "f#2", "bb1", "F", "G", "e1", "gbb1", "db2", "f##3", "a4"),
      use_double_accidentals = "flat"
    ),
    c("B#2", "ebb", "gb2", "a#1", "E#", "Abb", "fb1", "f1", "c#2", "g3", "bbb4")
  )
  # with double sharps
  expect_equal(
    get_enharmonic_equivalent(
      c("C1", "d", "f#2", "bb1", "F", "G", "e1", "gbb1", "db2", "f##3", "a4"),
      use_double_accidentals = "sharp"
    ),
    c("B#2", "c##", "gb2", "a#1", "E#", "F##", "fb1", "f1", "c#2", "g3", "g##4")
  )
})


test_that("has_accidental() works for any number of accidentals", {
  expect_equal(
    has_accidental(
      c("D1", "C#1", "Bb1", "d3", "eb2", "bb3", "b#3", "Gbb1", "C##1", "bbb2", "d##3")
    ),
    c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
expect_equal(
    has_accidental(
      c("D1", "C#1", "Bb1", "d3", "eb2", "bb3", "b#3", "Gbb1", "C##1", "bbb2", "d##3"),
      which = "sharp"
    ),
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
  )
expect_equal(
    has_accidental(
      c("D1", "C#1", "Bb1", "d3", "eb2", "bb3", "b#3", "Gbb1", "C##1", "bbb2", "d##3"),
      which = "flat"
    ),
    c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )
})


test_that("has_accidental() works for single accidentals", {
  expect_equal(
    has_accidental(
      c("D1", "C#1", "Bb1", "d3", "eb2", "bb3", "b#3", "Gbb1", "C##1", "bbb2", "d##3"),
      number = "single"
    ),
    c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
expect_equal(
    has_accidental(
      c("D1", "C#1", "Bb1", "d3", "eb2", "bb3", "b#3", "Gbb1", "C##1", "bbb2", "d##3"),
      which = "sharp",
      number = "single"
    ),
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
expect_equal(
    has_accidental(
      c("D1", "C#1", "Bb1", "d3", "eb2", "bb3", "b#3", "Gbb1", "C##1", "bbb2", "d##3"),
      which = "flat",
      number = "single"
    ),
    c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})


test_that("has_accidental() works for double accidentals", {
  expect_equal(
    has_accidental(
      c("D1", "C#1", "Bb1", "d3", "eb2", "bb3", "b#3", "Gbb1", "C##1", "bbb2", "d##3"),
      number = "double"
    ),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
expect_equal(
    has_accidental(
      c("D1", "C#1", "Bb1", "d3", "eb2", "bb3", "b#3", "Gbb1", "C##1", "bbb2", "d##3"),
      which = "sharp",
      number = "double"
    ),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
  )
expect_equal(
    has_accidental(
      c("D1", "C#1", "Bb1", "d3", "eb2", "bb3", "b#3", "Gbb1", "C##1", "bbb2", "d##3"),
      which = "flat",
      number = "double"
    ),
    c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )
})


test_that("flatten() works", {
  expect_equal(
    flatten(c("C1", "Eb", "f#", "e#", "fb1", "bb1", "b#2", "cb3", "a#4")),
    c("Cb1", "D", "f", "e", "eb1", "a1", "b2", "bb2", "a4")
  )
  expect_equal(flatten(get_major_scale("D")), get_major_scale("Db"))
})


test_that("sharpen() works", {
  expect_equal(
    sharpen(c("C1", "Eb", "f#", "e#", "fb1", "bb1", "b#2", "cb3", "a#4")),
    c("C#1", "E", "g", "f#", "f1", "b1", "c#3", "c3", "b4")
  )
  expect_equal(sharpen(get_major_scale("F")), get_major_scale("F#"))
})
