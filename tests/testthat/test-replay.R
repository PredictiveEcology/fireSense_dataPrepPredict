## A replay job (`replayFrom`) takes the spread-prediction inputs that do not depend on the spread parameters
## from a reference hindcast job (`replaySave = TRUE`), in place of building them.

replayObjs <- function() {
  pts <- terra::vect(cbind(x = c(100, 600), y = c(200, 900)), type = "points", crs = toyCRSp)
  pts$escaped <- c(TRUE, FALSE)
  poly <- terra::as.polygons(terra::ext(0, 500, 0, 500), crs = toyCRSp)
  poly$YEAR <- 2000L
  sfRow <- sf::st_as_sf(poly)
  sfRow$ELF <- "X"
  sfRow$p1 <- 0.5
  list(
    flammableRTM = toyRastP(c(rep(1, 15), 0), "flammable"),
    rasterToMatch = toyRastP(rep(1L, 16), "rtm"),
    rasterToMatchELF = toyRastP(rep(c(1L, 2L), 8), "ELF"),
    rasterToMatchLargeELF = toyRastP(rep(2L, 16), "large"),
    fireSense_SpreadCovariates = data.table::data.table(pixelID = 1:3, MDC = c(1, 2, 3), youngAge = c(0, 1, 0)),
    studyAreaWithSpreadParams = sfRow,
    nonEscapedFireSizesHa = c(1.5, 20, 300),
    ignitionFirePoints = pts,
    firePolys = list(poly, poly)
  )
}

## SpatRaster/SpatVector are pointers: compare what they hold
asData <- function(x) {
  if (inherits(x, "SpatRaster")) list(names(x), terra::values(x, mat = FALSE), as.vector(terra::ext(x)), terra::crs(x))
  else if (inherits(x, "SpatVector")) list(as.data.frame(x), terra::geom(x), terra::crs(x))
  else if (is.data.frame(x)) as.data.frame(x)
  else if (is.list(x)) lapply(x, asData)
  else x
}

test_that("without replaySave nothing is written, and the parameters default to no replay", {
  ref <- toyPrepRun(toyObjects(), params = list(whichModulesToPrepare = "fireSense_spreadPredict"))
  expect_length(list.files(SpaDES.core::outputPath(ref), pattern = "^replay_"), 0L)
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_true(is.na(md$parameters[md$parameters$paramName == "replayFrom", ]$default[[1]]))
  expect_false(md$parameters[md$parameters$paramName == "replaySave", ]$default[[1]])
})

test_that("saved replay inputs load back identical", {
  objs <- replayObjs()
  path <- withr::local_tempdir()
  saveReplay(objs, path)
  expect_true(file.exists(file.path(path, "replay_spreadInputs.qs2")))
  expect_true(file.exists(file.path(path, "replay_flammableRTM.tif")))
  back <- loadReplay(path)
  expect_setequal(names(back), names(objs))
  for (nm in setdiff(names(objs), "studyAreaWithSpreadParams"))
    expect_identical(asData(back[[nm]]), asData(objs[[nm]]), info = nm)
  expect_s3_class(back$studyAreaWithSpreadParams, "sf")
  expect_equal(sf::st_drop_geometry(back$studyAreaWithSpreadParams), sf::st_drop_geometry(objs$studyAreaWithSpreadParams),
               ignore_attr = TRUE)
})

test_that("a replay job takes the saved inputs and builds none", {
  objs <- toyInputsP()
  ref <- toyPrepRun(utils::modifyList(toyObjects(), replayObjs()[c("rasterToMatchELF", "rasterToMatchLargeELF",
                                                                    "nonEscapedFireSizesHa", "ignitionFirePoints", "firePolys")]),
                    params = list(whichModulesToPrepare = "fireSense_spreadPredict", replaySave = TRUE))
  path <- SpaDES.core::outputPath(ref)
  expect_true(file.exists(file.path(path, "replay_spreadInputs.qs2")))
  n <- 0L
  orig <- fireSenseUtils::cohortsToFuelClasses
  testthat::local_mocked_bindings(cohortsToFuelClasses = function(...) { n <<- n + 1L; orig(...) },
                                  .package = "fireSenseUtils")
  rep <- toyPrepRun(list(), params = list(whichModulesToPrepare = "fireSense_spreadPredict", replayFrom = path))
  expect_identical(n, 0L)
  expect_identical(covDF(rep$fireSense_SpreadCovariates), covDF(ref$fireSense_SpreadCovariates))
  for (nm in c("flammableRTM", "rasterToMatch", "rasterToMatchELF", "rasterToMatchLargeELF", "nonEscapedFireSizesHa",
               "ignitionFirePoints", "firePolys"))
    expect_identical(asData(rep[[nm]]), asData(ref[[nm]]), info = nm)
  ## and no build events were scheduled
  expect_false(any(c("getClimateRasters", "prepSpreadPredictData") %in% SpaDES.core::completed(rep)$eventType))
})

