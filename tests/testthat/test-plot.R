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


test_that("plot_piano() works with markers", {
  expect_doppelganger("plot_piano() with markers for left hand",
                      plot_piano(mark_left = c("D", "F#", "A", "c")))
  expect_doppelganger("plot_piano() with markers for right hand",
                      plot_piano(mark_right = c("g1", "bb1", "d2", "f2")))
  expect_doppelganger("plot_piano() with markers for both hands",
                      plot_piano(mark_left = c("e", "bb", "d1"),
                                 mark_right = c("e1", "a1", "c2")))
  expect_doppelganger("plot_piano() works with custom marker colours",
                      plot_piano(mark_left = c("e", "bb", "d1"),
                                 mark_right = c("e1", "a1", "c2"),
                                 colour_left = "orange",
                                 colour_right = "#ef45d2"))
})

test_that("plot_piano() works with markers and range restriction", {
  expect_doppelganger(
    "plot_piano() with range restriction and markers within range",
    plot_piano("A1", "c1", mark_left = c("D", "F#"), mark_right = c("A", "c"))
  )
  expect_doppelganger(
    "plot_piano() with range restriction and markers outside range",
     plot_piano("A1", "c1",
                mark_left = c("E1", "D", "F#"),
                mark_right = c("A", "c", "d2"))
  )
})


