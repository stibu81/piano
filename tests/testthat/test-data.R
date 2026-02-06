library(dplyr, warn.conflicts = FALSE)
library(stringr)

test_that("the number of keys is correct", {
  expect_equal(nrow(keys$white) + nrow(keys$black), 88)
})


test_that("the notes data is correct", {
  # same ranks for flats and sharps
  expect_equal(notes$sharp$rank, notes$flat$rank)
  # difference between notes must be 0.5
  expect_all_equal(diff(notes$sharp$rank), 0.5)
  expect_all_equal(diff(notes$flat$rank), 0.5)
  # notes must cover two octaves
  expect_equal(diff(range(notes$sharp$rank)), 2 * 6)
  expect_equal(diff(range(notes$flat$rank)), 2 * 6)
  # note names must be upper case
  expect_match(notes$sharp$name, "^[A-G]")
  expect_match(notes$flat$name, "^[A-G]")
  # the notes without accidentals must be identical
  expect_equal(notes$sharp %>% filter(!str_detect(name, "#")),
               notes$flat %>% filter(!str_detect(name, "b")))
})


test_that("the major_keys data is correct", {
  expect_named(major_keys, c("flat", "sharp"), ignore.order = TRUE)
  expect_all_equal(lengths(major_keys), 6)
  # the last entry must correspond to the same keyboard keys (F# = Gb)
  expect_equal(
    keys$black %>% filter(name_flat == tail(major_keys$flat, n = 1)),
    keys$black %>% filter(name_sharp == tail(major_keys$sharp, n = 1))
  )
})
