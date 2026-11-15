## Inside one year (the reps of a hindcast, `fireTimeStep` < 1) the covariates do not change, so they are
## built once. A change to an input they are made from (land cover, vegetation, climate) rebuilds them.

countFuelClassCalls <- function(code) {
  n <- 0L
  orig <- fireSenseUtils::cohortsToFuelClasses
  testthat::local_mocked_bindings(
    cohortsToFuelClasses = function(...) { n <<- n + 1L; orig(...) },
    .package = "fireSenseUtils")
  force(code)
  n
}

repTimes <- list(start = 2001, end = 2001.75)
repParams <- function(what) list(fireTimeStep = 0.25, whichModulesToPrepare = what)

for (what in c("fireSense_ignitionPredict", "fireSense_spreadPredict")) {
  covName <- if (what == "fireSense_ignitionPredict") "fireSense_igAndEscapePred_Covariates" else "fireSense_SpreadCovariates"

  test_that(paste("four same-year steps build the covariates once, as one step does:", what), {
    n <- countFuelClassCalls(reps <- toyPrepRun(toyObjects(), times = repTimes, params = repParams(what)))
    once <- toyPrepRun(toyObjects(), times = list(start = 2001, end = 2001),
                       params = list(whichModulesToPrepare = what))
    ## the tables are the first step's (their `year` label is 2001), not rebuilt at 2001.75
    expect_identical(n, 1L) # was 4
    expect_identical(covDF(reps[[covName]]), covDF(once[[covName]]))
  })

  test_that(paste("a vegetation change between steps of one year rebuilds the covariates:", what), {
    sim <- SpaDES.core::spades(toyPrepSim(toyObjects(), times = list(start = 2001, end = 2001),
                                          params = repParams(what)), debug = FALSE)
    first <- data.table::copy(sim[[covName]])
    sim$cohortData$B <- sim$cohortData$B * 2L
    ## the three more steps of the year: the first rebuilds, the others reuse it
    SpaDES.core::end(sim) <- 2001.75
    n <- countFuelClassCalls(sim <- SpaDES.core::spades(sim, debug = FALSE))
    expect_identical(n, 1L)
    expect_false(isTRUE(all.equal(covDF(first), covDF(sim[[covName]]))))
  })
}

test_that("a new year rebuilds the covariates", {
  n <- countFuelClassCalls(toyPrepRun(toyObjects(), times = list(start = 2001, end = 2003),
                                      params = list(whichModulesToPrepare = "fireSense_spreadPredict")))
  expect_identical(n, 3L)
})

## the first step of the year, then `change(sim)`, then the other three: the number of fuel-class builds
## they make, and the covariates before and after
stepsAfterChange <- function(objs, change, what, params = NULL) {
  p <- c(list(fireTimeStep = 0.25), if (!is.null(what)) list(whichModulesToPrepare = what), params)
  sim <- SpaDES.core::spades(toyPrepSim(objs, times = list(start = 2001, end = 2001), params = p), debug = FALSE)
  first <- list(ig = data.table::copy(sim$fireSense_igAndEscapePred_Covariates),
                sp = data.table::copy(sim$fireSense_SpreadCovariates))
  sim <- change(sim)
  SpaDES.core::end(sim) <- 2001.75
  n <- countFuelClassCalls(sim <- SpaDES.core::spades(sim, debug = FALSE))
  list(n = n, first = first, after = list(ig = sim$fireSense_igAndEscapePred_Covariates,
                                          sp = sim$fireSense_SpreadCovariates))
}

test_that("ignition and spread prepared together build once per year, as in a hindcast", {
  n <- countFuelClassCalls(reps <- toyPrepRun(toyObjects(), times = repTimes,
                                              params = list(fireTimeStep = 0.25)))
  once <- toyPrepRun(toyObjects(), times = list(start = 2001, end = 2001))
  expect_identical(n, 1L) # one year, both events, was 8 events' worth of builds (2 per step, shared)
  expect_identical(covDF(reps$fireSense_SpreadCovariates), covDF(once$fireSense_SpreadCovariates))
  expect_identical(covDF(reps$fireSense_igAndEscapePred_Covariates), covDF(once$fireSense_igAndEscapePred_Covariates))
})

test_that("a change that keeps the sum (two pixels swapped) rebuilds", {
  r <- stepsAfterChange(toyObjects(), function(sim) {
    v <- terra::values(sim$pixelGroupMap, mat = FALSE)
    v[c(1, 3)] <- v[c(3, 1)]
    sim$pixelGroupMap <- terra::setValues(sim$pixelGroupMap, v)
    sim
  }, "fireSense_spreadPredict")
  expect_identical(r$n, 1L)
  expect_false(isTRUE(all.equal(covDF(r$first$sp), covDF(r$after$sp))))
})

test_that("a land-cover change between steps of one year rebuilds, with several ELFs too", {
  o <- toyObjects()
  o$landcoverDT <- NULL
  o$sppEquivs <- list(
    data.table::data.table(LandR = c("Pice_mar", "Pinu_ban", "Popu_tre"), FuelClass = c("class1", "class1", "class2")),
    data.table::data.table(LandR = c("Pice_mar", "Pinu_ban", "Popu_tre"), FuelClass = c("Pice_mar", "Pinu_ban", "decid")))
  o$nonForestedLCCGroupsList <- list(list(nfLCC_wetland = 19L, nfLCC_grass = 16L), list(nfLCC_wetgrs = c(16L, 19L)))
  o$missingLCCgroupList <- list("nfLCC_grass", "nfLCC_wetgrs")
  r <- stepsAfterChange(o, function(sim) { # a forest pixel and a wetland pixel swap land cover
    v <- terra::values(sim$rstLCC_RTM, mat = FALSE)
    v[c(1, 9)] <- v[c(9, 1)]
    sim$rstLCC_RTM <- terra::setValues(sim$rstLCC_RTM, v)
    sim
  }, "fireSense_spreadPredict")
  expect_identical(r$n, 2L) # the rebuild, one per ELF
  expect_false(isTRUE(all.equal(covDF(r$first$sp), covDF(r$after$sp))))
})

test_that("a change of the simulated ELF rebuilds", {
  r <- stepsAfterChange(toyObjects(), function(sim) { sim$.ELFind <- "7"; sim }, "fireSense_spreadPredict")
  expect_identical(r$n, 1L)
})
