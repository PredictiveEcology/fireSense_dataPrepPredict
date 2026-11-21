## ELFlabels() is the set of ELFs a raster labels, from fireSenseUtils::elfCellLabels() (the cell lookup
## fireSense_spreadPredict uses too).

test_that("ELFlabels() gives a categorical raster's labels, or a plain raster's values, without NA", {
  expect_identical(ELFlabels(NULL), character(0))
  r <- terra::rast(nrows = 1, ncols = 4, vals = c(2, 1, 2, NA))
  expect_setequal(ELFlabels(r), c("1", "2"))
  levels(r) <- data.frame(id = 1:2, ELFind = c("A", "B"))
  expect_setequal(ELFlabels(r), c("A", "B"))
})
