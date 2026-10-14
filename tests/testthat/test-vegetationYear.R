## `vegetationYear`: take one year's vegetation from `fireSense_dataPrepFit`'s per-year lists and hold it
## static, instead of the last year's singletons aged by `ageNonForest`.
##
## The singletons in `toyObjects()` are the decoy ("last year", what NA uses). Year 2002 is the year
## asked for and differs from them in: cohort biomass (Pice_mar in pixel group 1 is 5000, not 2000),
## flammability (cell 15 is not flammable) and non-forest time since disturbance (wetland cell 9
## burned 3 years ago).

yearLists <- function() {
  decoy <- toyObjects()
  yrs <- paste0("year", 2001:2003)
  perYear <- function(x) stats::setNames(rep(list(x), length(yrs)), yrs)

  cd <- toyCohortData()
  cd$B[cd$pixelGroup == 1L & cd$speciesCode == "Pice_mar"] <- 5000L
  flam <- toyRast(c(rep(1, 14), 0, 0))
  tsd <- toyRast(c(5, rep(30, 13), 5, 30))
  tsd[9] <- 3
  lcDT <- data.table::data.table(pixelID = 1:14, wetland = as.integer(1:14 %in% c(9, 10)),
                                 grass = as.integer(1:14 %in% c(11, 12)))

  lists <- list(cohortDatas = perYear(decoy$cohortData),
                pixelGroupMaps = perYear(decoy$pixelGroupMap),
                landcoverDTs = perYear(decoy$landcoverDT),
                nonForest_timeSinceDisturbances = perYear(decoy$nonForest_timeSinceDisturbance),
                rstLCCs = perYear(toyLCC()),
                flammableRTMs = perYear(decoy$flammableRTM),
                standAgeMaps = perYear(decoy$standAgeMap))
  lists$cohortDatas$year2002 <- cd
  lists$landcoverDTs$year2002 <- lcDT
  lists$nonForest_timeSinceDisturbances$year2002 <- tsd
  lists$flammableRTMs$year2002 <- flam
  ## the decoy's own `rstLCCs` (unnamed, one element) is replaced by the per-year list
  utils::modifyList(decoy, lists, keep.null = TRUE)
}

hindcast <- function(objs = yearLists(), vegetationYear = 2002L, end = 2003) {
  toyPrepRun(objs, params = list(vegetationYear = vegetationYear), times = list(start = 2001, end = end))
}

test_that("vegetationYear takes that year's elements of the per-year lists", {
  sim <- hindcast()
  df <- covDF(sim$fireSense_SpreadCovariates)
  ## year 2002's flammableRTM has 14 flammable cells, the singleton 15
  expect_identical(df$pixelID, 1:14)
  ## year 2002's TSD: wetland cell 9 burned 3 y ago, so it is young (and so not wetland)
  expect_identical(which(df$youngAge == 1), c(5L, 6L, 9L))
  expect_identical(which(df$wetland == 1L), 10L)
  ## year 2002's cohorts: pixel group 1 has more class1 biomass than the singleton's
  base <- covDF(toyPrepRun()$fireSense_SpreadCovariates)
  expect_gt(df$class1[1], base$class1[1])
  ## the objects other modules read are year 2002's too
  expect_identical(as.vector(terra::values(sim$flammableRTM)), c(rep(1, 14), 0, 0))
  expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance))[9], 3)
  expect_identical(max(sim$cohortData$B), 5000L)
})

test_that("vegetationYear keeps vegetation and time since disturbance static over simulated years", {
  sim <- hindcast()
  expect_false("ageNonForest" %in% as.data.frame(SpaDES.core::events(sim))$eventType)
  expect_false("ageNonForest" %in% as.data.frame(SpaDES.core::completed(sim))$eventType)
  expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance))[9], 3)
  ## the covariates built in the last simulated year are year 2002's too
  df <- covDF(sim$fireSense_SpreadCovariates)
  expect_identical(which(df$youngAge == 1), c(5L, 6L, 9L))
})

test_that("with vegetationYear NA the module behaves as before: the singletons, and TSD ages", {
  sim <- toyPrepRun(yearLists(), times = list(start = 2001, end = 2003))
  expect_identical(max(sim$cohortData$B), 2000L)
  expect_true("ageNonForest" %in% as.data.frame(SpaDES.core::completed(sim))$eventType)
  ## TSD of cell 15 is 5 at 2001, aged in 2002 and 2003
  expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance))[15], 7)
})

test_that("a vegetationYear missing from the per-year lists stops with a clear message", {
  expect_error(hindcast(vegetationYear = 2010L), "vegetationYear.*2010.*year2010")
  objs <- yearLists()
  objs$standAgeMaps <- NULL
  expect_error(hindcast(objs), "standAgeMaps")
  objs <- yearLists()
  objs$rstLCCs <- objs$rstLCCs[c("year2001", "year2003")]
  expect_error(hindcast(objs), "rstLCCs.*year2002")
})

test_that("vegetationYear works with decimal simulated times (several reps inside one year)", {
  ## three steps of 0.001 year, as a hindcast job runs its reps
  sim <- toyPrepRun(yearLists(), params = list(vegetationYear = 2002L, fireTimeStep = 0.001),
                    times = list(start = 2001, end = 2001.0025))
  done <- as.data.frame(SpaDES.core::completed(sim))
  expect_identical(sum(done$eventType == "prepSpreadPredictData"), 3L)
  expect_identical(sum(done$eventType == "prepIgAndEscPredictData"), 3L)
  expect_false("ageNonForest" %in% done$eventType)
  ## the climate layer is that of the calendar year (2001) at every step; MDC 2001 is 100 + cell
  df <- covDF(sim$fireSense_SpreadCovariates)
  expect_identical(df$MDC, 100 + 1:14)
  ## vegetation and TSD are year 2002's after the last step
  expect_identical(which(df$youngAge == 1), c(5L, 6L, 9L))
  expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance))[9], 3)
  ## the ignition table is built for the last step
  expect_gt(NROW(sim$fireSense_igAndEscapePred_Covariates), 0L)
})

test_that("a cached init restores the vegetation it took from the per-year lists", {
  ## `init` cached: a cache hit restores only the objects declared `createsOutput`, so every object
  ## `Init` sets (here those of `vegetationYear`) must be one, else it reverts to what was supplied.
  cachePath <- withr::local_tempdir()
  runInit <- function() {
    sim <- SpaDES.core::simInit(
      times = list(start = 2001, end = 2001, timeunit = "year"),
      modules = moduleName,
      params = stats::setNames(list(list(igAggFactor = 2, vegetationYear = 2002L, .useCache = "init")), moduleName),
      objects = yearLists(),
      paths = utils::modifyList(testPaths, list(cachePath = cachePath)))
    SpaDES.core::spades(sim, debug = FALSE)
  }
  first <- runInit()
  second <- runInit()
  expect_true(any(grepl("init", reproducible::showCache(cachePath)$tagValue)))
  for (sim in list(first, second)) {
    expect_identical(max(sim$cohortData$B), 5000L)
    expect_identical(as.vector(terra::values(sim$flammableRTM)), c(rep(1, 14), 0, 0))
    expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance))[9], 3)
    expect_identical(sim$landcoverDT, yearLists()$landcoverDTs$year2002)
  }
  expect_false(is.null(second$pixelGroupMap))
  expect_identical(terra::values(second$pixelGroupMap), terra::values(yearLists()$pixelGroupMaps$year2002))
})
