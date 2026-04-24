test_that("trim_na() works on the left", {
  expect_equal(trim_na(c(NA, NA, 1, 2, 3, NA), "left"), c(1, 2, 3, NA))
  expect_equal(trim_na(c(NA, "a", "b", "c", NA, NA), "left"), c("a", "b", "c", NA, NA))
  expect_equal(trim_na(c(NA, NA, 1, NA, 2, 3, NA), "left"), c(1, NA, 2, 3, NA))
  expect_equal(
    trim_na(c(NA, "a", "b", NA, "c", NA, NA), "left"),
    c("a", "b", NA, "c", NA, NA)
  )
})


test_that("trim_na() works on the right", {
  expect_equal(trim_na(c(NA, NA, 1, 2, 3, NA), "right"), c(NA, NA, 1, 2, 3))
  expect_equal(trim_na(c(NA, "a", "b", "c", NA, NA), "right"), c(NA, "a", "b", "c"))
  expect_equal(trim_na(c(NA, NA, 1, NA, 2, 3, NA), "right"), c(NA, NA, 1, NA, 2, 3))
  expect_equal(
    trim_na(c(NA, "a", "b", NA, "c", NA, NA), "right"),
    c(NA, "a", "b", NA, "c")
  )
})


test_that("trim_na() works on both sides", {
  expect_equal(trim_na(c(NA, NA, 1, 2, 3, NA), "both"), c(1, 2, 3))
  expect_equal(trim_na(c(NA, "a", "b", "c", NA, NA), "both"), c("a", "b", "c"))
  expect_equal(trim_na(c(NA, NA, 1, NA, 2, 3, NA), "both"), c(1, NA, 2, 3))
  expect_equal(
    trim_na(c(NA, "a", "b", NA, "c", NA, NA), "both"),
    c("a", "b", NA, "c")
  )
})


test_that("trim_na() works when there are no trailing NAs", {
  expect_equal(trim_na(c(1, 2, 3), "both"), c(1, 2, 3))
  expect_equal(trim_na(c("a", "b", "c"), "both"), c("a", "b", "c"))
  expect_equal(trim_na(c(1, NA, 2, 3), "both"), c(1, NA, 2, 3))
  expect_equal(trim_na(c("a", "b", NA, "c"), "both"), c("a", "b", NA, "c"))
})


test_that("trim_na() works when all values are NA", {
  expect_equal(trim_na(c(NA, NA)), logical(0))
  expect_equal(trim_na(c(NA_real_, NA_real_)), numeric(0))
  expect_equal(trim_na(c(NA_integer_, NA_integer_)), integer(0))
  expect_equal(trim_na(c(NA_character_, NA_character_)), character(0))
})


test_that("trim_na() works with empty vectors", {
  expect_equal(trim_na(c()), c())
  expect_equal(trim_na(logical(0)), logical(0))
  expect_equal(trim_na(numeric(0)), numeric(0))
  expect_equal(trim_na(integer(0)), integer(0))
  expect_equal(trim_na(character(0)), character(0))
})
