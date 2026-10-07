## `ageNonForest()` is the one helper in this module with no dependencies at all: it takes
## two rasters and returns one. Every value below is worked out by hand from the toy TSD.

test_that("ageNonForest adds one year everywhere when nothing burned", {
  TSD <- toyRast(c(5, rep(30, 13), 5, 30))
  out <- ageNonForest(TSD = TSD, rstCurrentBurn = NULL, timeStep = 1)
  expect_s4_class(out, "SpatRaster")
  ## hand-computed: every cell + 1
  expect_identical(as.vector(terra::values(out)), c(6, rep(31, 13), 6, 31))
  ## geometry must survive: the values are written back with setValues()
  expect_true(terra::compareGeom(out, TSD, stopOnError = FALSE))
})

test_that("ageNonForest resets burned pixels to zero and ages the rest", {
  TSD <- toyRast(c(5, rep(30, 13), 5, 30))
  ## cell 2 burned (1); cell 3 is NA (no burn data, i.e. did not burn); cell 16 burned
  burn <- toyRast(c(0, 1, NA, rep(0, 12), 1))
  out <- ageNonForest(TSD = TSD, rstCurrentBurn = burn, timeStep = 1)
  ## hand-computed: 5+1; burned -> 0; NA is unburned so 30+1; cell 15 (TSD 5, unburned) -> 6;
  ## cell 16 burned -> 0
  expect_identical(as.vector(terra::values(out)), c(6, 0, rep(31, 12), 6, 0))
})

test_that("ageNonForest treats NA and 0 in rstCurrentBurn identically", {
  TSD <- toyRast(rep(10, 16))
  allNA <- toyRast(rep(NA_real_, 16))
  allZero <- toyRast(rep(0, 16))
  expect_identical(as.vector(terra::values(ageNonForest(TSD, allNA, 1))), rep(11, 16))
  expect_identical(as.vector(terra::values(ageNonForest(TSD, allNA, 1))),
                   as.vector(terra::values(ageNonForest(TSD, allZero, 1))))
})

test_that("ageNonForest ignores timeStep, as documented", {
  ## `timeStep` is accepted but the raster always advances by exactly one year. Asserted so
  ## that honouring it becomes a deliberate, visible change rather than a silent one.
  TSD <- toyRast(rep(10, 16))
  expect_identical(as.vector(terra::values(ageNonForest(TSD, NULL, timeStep = 1))),
                   as.vector(terra::values(ageNonForest(TSD, NULL, timeStep = 10))))
})
