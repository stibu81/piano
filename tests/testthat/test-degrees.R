test_that("rank_degrees() works", {
  expect_equal(
    rank_degrees(c("1", "b3", "3", "b5", "5", "b9", "9", "#9", "#11", "b13")),
    c(1, 2.6, 3, 4.6, 5, 8.6, 9, 9.4, 11.4, 12.6)
  )
})


test_that("sort_degrees() works", {
  expect_equal(
    sort_degrees(c("7", "#9", "3", "b13", "#11", "9", "b7", "b9", "8", "b3")),
    c("b3", "3", "b7", "7", "8", "b9", "9", "#9", "#11", "b13")
  )
  expect_equal(
    sort_degrees(c("#9", "3", "9", "3", "#9", "3", "b9")),
    c("3", "3", "3", "b9", "9", "#9", "#9")
  )
})


test_that("sort_degrees() works in decreasing order", {
  expect_equal(
    sort_degrees(
      c("7", "#9", "3", "b13", "#11", "9", "b7", "b9", "8", "b3"),
      decreasing = TRUE
    ),
    c("b13", "#11", "#9", "9", "b9", "8", "7", "b7", "3", "b3")
  )
  expect_equal(
    sort_degrees(c("#9", "3", "9", "3", "#9", "3", "b9"), decreasing = TRUE),
    c("#9", "#9", "9", "b9", "3", "3", "3")
  )
})
