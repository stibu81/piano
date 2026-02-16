library(dplyr, warn.conflicts = FALSE)
library(stringr)

test_that("the number of keys is correct", {
  expect_equal(nrow(keys_data$white) + nrow(keys_data$black), 88)
})


test_that("number of accidentals on white keys", {
  # for the flats, the first character must be ignored because it can
  # also be a b.
  expect_equal(
    str_count(str_sub(keys_data$white$name_flat, 2), "b"),
    c(2, 1, 2, rep(c(2, 1, 2, 2, 2, 1, 2), 7))
  )
  expect_equal(
    str_count(keys_data$white$name_sharp, "#"),
    c(2, 2, 1, rep(c(2, 2, 1, 2, 2, 2, 1), 7))
  )
})


test_that("the notes data is correct", {
  # same ranks for flats and sharps
  expect_equal(notes_data$sharp$rank, notes_data$flat$rank)
  # difference between notes must be 0.5
  expect_all_equal(diff(notes_data$sharp$rank), 0.5)
  expect_all_equal(diff(notes_data$flat$rank), 0.5)
  # notes must cover two octaves
  expect_equal(diff(range(notes_data$sharp$rank)), 2 * 6)
  expect_equal(diff(range(notes_data$flat$rank)), 2 * 6)
  # note names must be upper case
  expect_match(notes_data$sharp$name, "^[A-G]")
  expect_match(notes_data$flat$name, "^[A-G]")
  # the notes without accidentals must be identical
  expect_equal(notes_data$sharp %>% filter(!str_detect(name, "#")),
               notes_data$flat %>% filter(!str_detect(name, "b")))
})


test_that("the major_keys data is correct", {
  expect_named(major_keys_data, c("flat", "sharp"), ignore.order = TRUE)
  expect_all_equal(lengths(major_keys_data), 6)
  # the last entry must correspond to the same keyboard keys (F# = Gb)
  expect_equal(
    keys_data$black %>% filter(name_flat == tail(major_keys_data$flat, n = 1)),
    keys_data$black %>% filter(name_sharp == tail(major_keys_data$sharp, n = 1))
  )
})
