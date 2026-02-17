test_that("filter_key_range() works", {
  expect_equal(filter_key_range(keys_data, "A2", "c5"), keys_data)
  expect_equal(filter_key_range(keys_data, "A1", "e")$white$name,
               c("A1", "B1", "C", "D", "E", "F", "G", "A", "B", "c", "d", "e"))
  expect_equal(filter_key_range(keys_data, "A1", "e")$black$name_sharp,
               c("A#1", "C#", "D#", "F#", "G#", "A#", "c#", "d#"))
  expect_equal(filter_key_range(keys_data, "A1", "e")$black$name_flat,
               c("Bb1", "Db", "Eb", "Gb", "Ab", "Bb", "db", "eb"))
})


test_that("filter_key_range() error messages work", {
  expect_error(filter_key_range(keys_data, "A3", "c5"),
               "'A3' is not a valid lower key")
  expect_error(filter_key_range(keys_data, "A1", "e5"),
               "'e5' is not a valid upper key")
  expect_error(filter_key_range(keys_data, "c3", "c3"),
               "lower must be below upper")
  expect_error(filter_key_range(keys_data, "c3", "e2"),
               "lower must be below upper")
})


test_that("verify_key_names() works", {
  expect_true(verify_key_names(c("C", "d", "eb4", "g#2")))
  expect_true(verify_key_names(c("cb2", "B#1", "e#", "fb3")))
  expect_true(verify_key_names(c("bbb", "G##1", "dbb2", "f##3")))
  expect_error(verify_key_names(c("D1", "C2")),
               "C2 is not a valid note name")
  expect_error(verify_key_names(c("D1", "C2", "a3", "d5")),
               "C2 and d5 are not valid note names")
  expect_error(verify_key_names(c("C", "d", "eb4", "g#2"), type = "white"),
               "eb4 and g#2 are not valid note names")
  expect_error(verify_key_names(c("C", "d", "eb4", "g#2"), type = "black"),
               "C and d are not valid note names")
})


test_that("decompose_key_names() works", {
  expect_equal(
    decompose_key_names(c("C", "Eb1", "F#1", "e", "g#2", "bb3", "a4", "")),
    list(notes = c("C", "Eb", "F#", "e", "g#", "bb", "a", ""),
         numbers = c("", "1", "1", "", "2", "3", "4", ""))
  )
})
