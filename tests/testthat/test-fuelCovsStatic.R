## The part of the coarse ignition covariates that does not change from year to year (the coarse mask and
## the non-flammable pixels, `fireSenseUtils::fuelCovsStatic()`) is kept in `mod` per fuel set and passed
## back to `prepare_FuelCovsCoarse()` each year, which hands back the one it used.

staticOf <- function(sim) sim[[".modObjs"]][[moduleName]]$fuelCovsStatic

countStaticBuilds <- function(code) {
  n <- 0L
  orig <- fireSenseUtils::fuelCovsStatic
  testthat::local_mocked_bindings(
    fuelCovsStatic = function(...) { n <<- n + 1L; orig(...) },
    .package = "fireSenseUtils")
  force(code)
  n
}

test_that("the ignition event keeps the static and the later years reuse it", {
  sim <- NULL
  n <- countStaticBuilds(sim <- toyPrepRun(toyObjects(), times = list(start = 2001, end = 2003),
                                           params = list(whichModulesToPrepare = "fireSense_ignitionPredict")))
  expect_identical(n, 1L) # three years, one build
  st <- staticOf(sim)
  expect_length(st, 1L)
  expect_true(fireSenseUtils::fuelCovsStaticIsFor(st[[1]], st[[1]]$pixelID, sim$flammableRTM, fact = 2))
})

test_that("a static kept in mod is replaced when land cover changes", {
  sim <- toyPrepRun(toyObjects(), times = list(start = 2001, end = 2001),
                    params = list(whichModulesToPrepare = "fireSense_ignitionPredict"))
  before <- staticOf(sim)[[1]]
  ldt <- sim$landcoverDT
  sim$landcoverDT <- ldt[seq_len(nrow(ldt))[-1], , drop = FALSE] # one pixel is no longer in land cover
  SpaDES.core::end(sim) <- 2002
  after <- NULL
  n <- countStaticBuilds(after <- SpaDES.core::spades(sim, debug = FALSE))
  expect_gte(n, 1L)
  expect_false(identical(staticOf(after)[[1]]$pixelID, before$pixelID))
  expect_identical(staticOf(after)[[1]]$pixelID, setdiff(before$pixelID, ldt$pixelID[1]))
})

test_that("the covariates are the same with the static as they were when it was built each year", {
  rebuilt <- toyPrepRun(toyObjects(), times = list(start = 2001, end = 2003),
                        params = list(whichModulesToPrepare = "fireSense_ignitionPredict"))
  ## a static that never fits (as before the module kept one): each year rebuilds
  testthat::local_mocked_bindings(
    fuelCovsStaticIsFor = function(...) FALSE, .package = "fireSenseUtils")
  asBefore <- toyPrepRun(toyObjects(), times = list(start = 2001, end = 2003),
                         params = list(whichModulesToPrepare = "fireSense_ignitionPredict"))
  expect_identical(covDF(rebuilt$fireSense_igAndEscapePred_Covariates),
                   covDF(asBefore$fireSense_igAndEscapePred_Covariates))
})
