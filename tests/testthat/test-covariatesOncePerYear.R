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
