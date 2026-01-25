test_that("the number of keys is correct", {
  expect_equal(nrow(keys$white) + nrow(keys$black), 88)
})

test_that("filter_key_range() works", {
  expect_equal(filter_key_range(keys, "A2", "c5"), keys)
  expect_equal(filter_key_range(keys, "A1", "e")$white$name,
               c("A1", "B1", "C", "D", "E", "F", "G", "A", "B", "c", "d", "e"))
  expect_equal(filter_key_range(keys, "A1", "e")$black$name_sharp,
               c("A#1", "C#", "D#", "F#", "G#", "A#", "c#", "d#"))
  expect_equal(filter_key_range(keys, "A1", "e")$black$name_flat,
               c("Bb1", "Db", "Eb", "Gb", "Ab", "Bb", "db", "eb"))
})


test_that("filter_key_range() error messages work", {
  expect_error(filter_key_range(keys, "A3", "c5"),
               "'A3' is not a valid lower key")
  expect_error(filter_key_range(keys, "A1", "e5"),
               "'e5' is not a valid upper key")
  expect_error(filter_key_range(keys, "c3", "c3"),
               "lower must be below upper")
  expect_error(filter_key_range(keys, "c3", "e2"),
               "lower must be below upper")
})
