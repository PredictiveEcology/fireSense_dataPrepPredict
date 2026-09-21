## `getCurrentClimate()` picks this year's layer out of each element of
## `projectedClimateRasters`. The toy climate encodes both the year and the cell in the
## value -- layer `year<Y>` of MDC holds `(Y - 2000) * 100 + cellNumber` -- so an assertion
## on a value proves exactly which layer of which variable was taken.

test_that("getCurrentClimate takes the layer for time(sim), one per climate variable", {
  sim <- toyPrepSim(times = list(start = 2002, end = 2002))
  sim <- getCurrentClimate(sim)

  ## one layer per element of projectedClimateRasters, named for the variable not the year
  expect_identical(names(sim$currentClimateRasters), c("MDC", "Tmax"))
  ## hand-computed from the toy encoding: year2002 MDC cell n = 200 + n; Tmax is its negative
  expect_identical(as.vector(terra::values(sim$currentClimateRasters[["MDC"]])), 200 + 1:16)
  expect_identical(as.vector(terra::values(sim$currentClimateRasters[["Tmax"]])), -(200 + 1:16))
})

test_that("getCurrentClimate leaves a supplied currentClimateRasters untouched", {
  objs <- toyObjects()
  ## a raster that matches pixelGroupMap's geometry but holds a value no toy year produces
  objs$currentClimateRasters <- toyRast(rep(999, 16))
  names(objs$currentClimateRasters) <- "MDC"
  sim <- toyPrepSim(objs, times = list(start = 2002, end = 2002))
  sim <- getCurrentClimate(sim)
  expect_identical(names(sim$currentClimateRasters), "MDC")
  expect_identical(as.vector(terra::values(sim$currentClimateRasters)), rep(999, 16))
})

test_that("getCurrentClimate errors when the climate does not match pixelGroupMap", {
  objs <- toyObjects()
  ## a 2 x 2 pixelGroupMap over the same extent: same extent and CRS, different resolution
  objs$pixelGroupMap <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 4,
                                    ymin = 0, ymax = 4, vals = 1, crs = "EPSG:3005")
  sim <- toyPrepSim(objs)
  expect_error(getCurrentClimate(sim), "mismatch in resolution detected")
})

test_that("getCurrentClimate errors on a supplied currentClimateRasters of the wrong geometry", {
  ## the second compareGeom() guard, on the branch where nothing is built
  objs <- toyObjects()
  objs$currentClimateRasters <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 4,
                                            ymin = 0, ymax = 4, vals = 1, crs = "EPSG:3005")
  sim <- toyPrepSim(objs)
  expect_error(getCurrentClimate(sim), "mismatch in resolution detected")
})

test_that("getCurrentClimate messages when the requested year is beyond the projection", {
  ## toy climate covers 2001-2003; asking for 2005 must warn the user that a layer is reused
  objs <- toyObjects()
  objs$projectedClimateRasters <- toyClimate(2001:2003)
  sim <- toyPrepSim(objs, times = list(start = 2005, end = 2005))
  expect_message(try(getCurrentClimate(sim), silent = TRUE),
                 "re-using projected climate layers from")
})

test_that("KNOWN BUG: climateYear does not override time(sim) for layer selection", {
  ## Intended behaviour (module source, ~lines 314-318): `currentYear` is `sim$climateYear`
  ## when supplied, and `currentYear` chooses the climate layer. It does not: the inner
  ## lapply's formal `currentYear = time(sim)` shadows the outer `currentYear`, so the layer
  ## is always `year<time(sim)>` and `climateYear` only reaches the "beyond the projection"
  ## message. This test states the INTENDED assertion and records that it currently fails;
  ## when the shadowing is fixed, `expect_failure()` will itself fail and this test must be
  ## unwrapped. It deliberately does not assert that the present behaviour is correct.
  objs <- toyObjects()
  objs$climateYear <- 2003
  sim <- toyPrepSim(objs, times = list(start = 2001, end = 2001))
  sim <- getCurrentClimate(sim)
  expect_failure(
    ## year2003 MDC cell n = 300 + n; what is actually returned is year2001, i.e. 100 + n
    expect_identical(as.vector(terra::values(sim$currentClimateRasters[["MDC"]])), 300 + 1:16)
  )
})
