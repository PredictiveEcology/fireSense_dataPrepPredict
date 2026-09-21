## `getCurrentClimate()` picks this year's layer out of each element of
## `projectedClimateRasters`. The toy climate encodes both the year and the cell in the
## value -- layer `year<Y>` of MDC holds `(Y - 2000) * 100 + cellNumber` -- so an assertion
## on a value proves exactly which layer of which variable was taken.

test_that("getCurrentClimate takes the layer for time(sim), one per climate variable", {
  sim <- toyPrepSim(times = list(start = 2002, end = 2002))
  sim <- toyFunP(sim, "getCurrentClimate")(sim)

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
  sim <- toyFunP(sim, "getCurrentClimate")(sim)
  expect_identical(names(sim$currentClimateRasters), "MDC")
  expect_identical(as.vector(terra::values(sim$currentClimateRasters)), rep(999, 16))
})

test_that("getCurrentClimate errors when the climate does not match pixelGroupMap", {
  objs <- toyObjects()
  ## a 2 x 2 pixelGroupMap over the same extent: same extent and CRS, different resolution
  objs$pixelGroupMap <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 4,
                                    ymin = 0, ymax = 4, vals = 1, crs = "EPSG:3005")
  sim <- toyPrepSim(objs)
  expect_error(toyFunP(sim, "getCurrentClimate")(sim), "mismatch in resolution detected")
})

test_that("getCurrentClimate errors on a supplied currentClimateRasters of the wrong geometry", {
  ## the second compareGeom() guard, on the branch where nothing is built
  objs <- toyObjects()
  objs$currentClimateRasters <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 4,
                                            ymin = 0, ymax = 4, vals = 1, crs = "EPSG:3005")
  sim <- toyPrepSim(objs)
  expect_error(toyFunP(sim, "getCurrentClimate")(sim), "mismatch in resolution detected")
})

test_that("getCurrentClimate messages when the requested year is beyond the projection", {
  ## toy climate covers 2001-2003; asking for 2005 must warn the user that a layer is reused
  objs <- toyObjects()
  objs$projectedClimateRasters <- toyClimate(2001:2003)
  sim <- toyPrepSim(objs, times = list(start = 2005, end = 2005))
  expect_message(try(toyFunP(sim, "getCurrentClimate")(sim), silent = TRUE),
                 "re-using projected climate layers from")
})

## Two defects in getCurrentClimate(), both silent: the wrong year is used, and the right year is
## used only once. The toy climate stack makes either visible, because layer "year<yyyy>" is
## constant at yyyy - 1900.

test_that("climateYear selects the climate layers, not time(sim)", {
  sim <- toySimInitP(objects = toyInputsP(climateYear = 2003), start = 2001, end = 2001)
  sim <- toyFunP(sim, "getCurrentClimate")(sim)
  ## 2003 -> 103 everywhere. Before the fix the inner function's `currentYear = time(sim)`
  ## formal shadowed the outer value and 2001 (101) was used.
  expect_identical(unique(as.vector(terra::values(sim$currentClimateRasters))), 103)
})

test_that("without climateYear, time(sim) selects the climate layers", {
  sim <- toySimInitP(start = 2002, end = 2002)
  sim <- toyFunP(sim, "getCurrentClimate")(sim)
  expect_identical(unique(as.vector(terra::values(sim$currentClimateRasters))), 102)
})

test_that("the climate rasters are rebuilt when the year changes", {
  ## Run through spades(), one year at a time: the module remembers in `mod` that it built the
  ## object and for which year, and `mod` only persists inside spades().
  sim <- toySimInitP(start = 2001, end = 2001)
  sim <- toyRunEventsP(sim, c("init", "getClimateRasters"))
  expect_identical(unique(as.vector(terra::values(sim$currentClimateRasters))), 101)

  ## `currentClimateRasters` is a module output, so it survives into the next year: an
  ## `is.null()` guard left it stale (101) forever. It must now follow the year.
  SpaDES.core::end(sim) <- 2002
  sim <- toyRunEventsP(sim, "getClimateRasters")
  expect_identical(unique(as.vector(terra::values(sim$currentClimateRasters))), 102)

  SpaDES.core::end(sim) <- 2003
  sim <- toyRunEventsP(sim, "getClimateRasters")
  expect_identical(unique(as.vector(terra::values(sim$currentClimateRasters))), 103)
})

test_that("the rasters are not rebuilt twice within the same year", {
  ## run through spades() so that the module's `mod` (where the year is remembered) exists
  sim <- toySimInitP(start = 2001, end = 2001)
  sim <- toyRunEventsP(sim, c("init", "getClimateRasters"))
  expect_identical(unique(as.vector(terra::values(sim$currentClimateRasters))), 101)
  expect_identical(as.numeric(toyModP(sim)$currentClimateYear), 2001)

  ## poison the cached object and run the event again at the same time: the year has not
  ## changed, so the rasters must be returned untouched rather than rebuilt
  sim$currentClimateRasters <- terra::setValues(sim$currentClimateRasters, rep(-1, 16))
  sim <- SpaDES.core::scheduleEvent(sim, SpaDES.core::start(sim), toyModuleName,
                                    "getClimateRasters")
  sim <- toyRunEventsP(sim, "getClimateRasters")
  expect_identical(unique(as.vector(terra::values(sim$currentClimateRasters))), -1)
})

test_that("a multi-year run advances the climate rasters every year", {
  sim <- toySimInitP(start = 2001, end = 2003)
  sim <- toyRunEventsP(sim, c("init", "getClimateRasters"))
  ## end(sim) is 2003, and getClimateRasters reschedules itself annually
  expect_identical(unique(as.vector(terra::values(sim$currentClimateRasters))), 103)
})

test_that("a currentClimateRasters supplied every year is left alone and reaches the covariates", {
  ## In the project the `climateYear` module runs before this one every year and sets
  ## `sim$currentClimateRasters` (preferring historical rasters). Simulate it: before each
  ## year's events, replace the object with values no layer of `projectedClimateRasters` holds
  ## (5000 + the projected value). A guard keyed on the year alone rebuilds from
  ## `projectedClimateRasters` and silently discards the supplied rasters.
  supplied <- function(y) {
    r <- c(toyRast(5000 + (y - 2000) * 100 + 1:16), toyRast(-(5000 + (y - 2000) * 100 + 1:16)))
    names(r) <- c("MDC", "Tmax")
    r
  }
  objs <- toyObjects()
  objs$currentClimateRasters <- supplied(2001)
  sim <- toyPrepSim(objs, times = list(start = 2001, end = 2001))
  for (y in 2001:2003) {
    sim$currentClimateRasters <- supplied(y)
    SpaDES.core::end(sim) <- y
    sim <- SpaDES.core::spades(sim, debug = FALSE)
    expect_identical(as.vector(terra::values(sim$currentClimateRasters[["MDC"]])),
                     5000 + (y - 2000) * 100 + 1:16)
    expect_identical(covDF(sim$fireSense_SpreadCovariates)$MDC, 5000 + (y - 2000) * 100 + 1:15)
    ## the 2 x 2 aggregate of the supplied layer, as in test-covariates.R
    expect_equal(covDF(sim$fireSense_igAndEscapePred_Covariates)$MDC,
                 5000 + (y - 2000) * 100 + c(3.5, 5.5, 11.5))
    expect_identical(unique(covDF(sim$fireSense_igAndEscapePred_Covariates)$year), as.numeric(y))
  }
})
