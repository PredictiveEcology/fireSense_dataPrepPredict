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
  sim <- toySimInitP(start = 2001, end = 2003)
  sim <- toyFunP(sim, "getCurrentClimate")(sim)
  expect_identical(unique(as.vector(terra::values(sim$currentClimateRasters))), 101)

  ## `currentClimateRasters` is a module output, so it survives into the next year: an
  ## `is.null()` guard left it stale (101) forever. It must now follow the year.
  SpaDES.core::time(sim) <- 2002
  sim <- toyFunP(sim, "getCurrentClimate")(sim)
  expect_identical(unique(as.vector(terra::values(sim$currentClimateRasters))), 102)

  SpaDES.core::time(sim) <- 2003
  sim <- toyFunP(sim, "getCurrentClimate")(sim)
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
