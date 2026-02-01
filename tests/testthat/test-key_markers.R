library(dplyr, warn.conflicts = FALSE)

test_that("get_key_markers() works", {
  expect_setequal(
    get_key_markers(c("C", "c", "ab2", "A2", "d#3"))$name,
    c("C", "c", "ab2", "A2", "d#3")
  )
  expect_all_equal(get_key_markers(c("C", "e", "a1", "g3"))$y, 1.1)
  expect_all_equal(get_key_markers(c("C#", "eb", "a#1", "gb3"))$y, 3.3)
  ref <- tibble(
    name = c("A2", "C", "f", "d#3", "ab2"),
    x = c(-1.5, 7.5, 17.5, 37, 33),
    y = c(1.1, 1.1, 1.1, 3.3, 3.3)
  )
  expect_equal(get_key_markers(c("C", "f", "ab2", "A2", "d#3")), ref)
  expect_equal(get_key_markers(), ref[integer(0), ])
})

test_that("get_key_markers() handles invalid input", {
  expect_error(get_key_markers(c("C", "x")), "x is not a valid note name")
})
