library(dplyr)
library(readr)
library(withr)

db_file <- system.file("example", "chord_database.csv", package = "piano")
db_ref <- tibble(
  type = c("major", "minor", "dominant", "half-diminished", "diminished", "augmented"),
  notation = c("maj7", "m7", "7", "7b5", "o7", "7"),
  name = NA_character_,
  # fmt: skip
  left = list(c("3", "13", "9"), c("1", "5", "b3"), c("b7", "3", "9"),
              c("1", "b3", "b7"), c("1", "1"), c("1", "1")),
  # fmt: skip
  right = list(c("5", "7", "9"), c("11", "b7", "9", "11"), character(),
               c("9", "#11", "b7"), c("1", "b3", "b5", "6"), c("b7", "3", "#5")),
  rooted = c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),
  left_hand = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
  top_degree = c("9", "11", "9", "b7", "6", "#5"),
  alterations = list(character(), character(), character(), c("#11"), c("b5"), c("#5"))
)

test_that("read_chords_db() works for the example file", {
  expect_equal(read_chords_csv(db_file), db_ref)
})


test_that("read_chord_csv() handles additional columns", {
  local_file("test.csv")
  read_csv(db_file, col_types = cols(.default = "c")) %>%
    mutate(additional = 1:6) %>%
    write_csv("test.csv")
  expect_equal(read_chords_csv("test.csv"), db_ref)
})


test_that("read_chord_csv() handles missing columns", {
  local_file("test.csv")
  read_csv(db_file, col_types = cols(.default = "c")) %>%
    select(-"right") %>%
    write_csv("test.csv")
  expect_error(read_chords_csv("test.csv"), "file must contain the columns")
})


test_that("read_chord_csv() handles invalid_chord_type", {
  local_file("test.csv")
  read_csv(db_file, col_types = cols(.default = "c")) %>%
    mutate(type = if_else(type == "minor", "bad-type", type)) %>%
    write_csv("test.csv")
  expect_error(read_chords_csv("test.csv"), "file contains invalid chord types")
})


test_that("read_chord_csv() handles invalid_scale_degrees", {
  local_file("test.csv")
  read_csv(db_file, col_types = cols(.default = "c")) %>%
    mutate(right = if_else(type == "dominant", "b3 5", right)) %>%
    write_csv("test.csv")
  expect_error(
    read_chords_csv("test.csv"),
    "chord number 3 contains invalid scale degrees"
  )
})
