library(vdiffr)

test_that("plot_piano() works with default settings", {
  expect_doppelganger("plot_piano() with default settings", plot_piano())
})


test_that("plot_piano() works with reduced range", {
  expect_doppelganger("plot_piano() with reduced range",
                      plot_piano("D1", "f2"))
  expect_doppelganger("plot_piano() for only three keys",
                      plot_piano("d1", "e1"))
  expect_doppelganger("plot_piano() for only two keys",
                      plot_piano("B", "c"))
})


test_that("plot_piano() works with labels", {
  expect_doppelganger("plot_piano() with labels for white keys",
                      plot_piano("D1", "f2", labels = "white"))
  expect_doppelganger("plot_piano() with labels for black keys",
                      plot_piano("D1", "f2", labels = "black"))
  expect_doppelganger(
    "plot_piano() with labels for black keys with flats",
    plot_piano("D1", "f2", labels = "black", black_labels = "flat")
  )
  expect_doppelganger("plot_piano() with labels for all keys",
                      plot_piano("D1", "f2", labels = "all"))
})
