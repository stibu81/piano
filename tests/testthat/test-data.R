test_that("the number of keys is correct", {
  expect_equal(nrow(keys$white) + nrow(keys$black), 88)
})
