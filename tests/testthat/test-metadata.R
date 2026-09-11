## The module's metadata is its public contract: a project using this module binds
## to these object names and classes. Renaming or retyping one breaks every caller,
## which is exactly the class of change the raster -> terra migration makes, so it is
## worth asserting here rather than discovering downstream.
##
## When a change is deliberate, update this file in the same commit and bump the
## module version to match: removed, renamed or retyped is a MAJOR bump.

test_that("module metadata parses", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_type(md, "list")
  expect_identical(md$name, moduleName)
})

test_that("inputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  inputs <- stats::setNames(md$inputObjects$objectClass, md$inputObjects$objectName)
  expect_identical(
    inputs[order(names(inputs))],
    c(climateVariablesForFire  = "list",
      climateYear              = "character",
      cohortData               = "data.table",
      currentClimateRasters    = "SpatRaster",
      fireSense_IgnitionFitted = "fireSense_IgnitionFit",
      flammableRTM             = "SpatRaster",
      landcoverDT              = "data.table",
      lightningMaps            = "SpatRaster",
      missingLCCgroup          = "character",
      nonForestedLCCGroups     = "list",
      pixelGroupMap            = "SpatRaster",
      projectedClimateRasters  = "list",
      propFlammable            = "SpatRaster",
      rasterToMatch            = "SpatRaster",
      rstCurrentBurn           = "SpatRaster",
      rstLCC_RTM               = "SpatRaster",
      sppEquiv                 = "data.table",
      standAgeMap              = "SpatRaster")
  )
})

test_that("outputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  outputs <- stats::setNames(md$outputObjects$objectClass, md$outputObjects$objectName)
  expect_identical(
    outputs[order(names(outputs))],
    c(currentClimateRasters                = "SpatRaster",
      fireSense_igAndEscapePred_Covariates = "data.table",
      fireSense_SpreadCovariates           = "data.table",
      nonForest_timeSinceDisturbance       = "SpatRaster")
  )
})

test_that("parameters are the expected names", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_identical(
    sort(md$parameters$paramName),
    sort(c(".plotInitialTime", ".plotInterval", ".runInitialTime", ".saveInitialTime",
           ".saveInterval", ".useCache", "cutoffForYoungAge", "dataYear",
           "fireTimeStep", "flammabilityThreshold", "forestedLCC", "fuelClassCol",
           "igAggFactor", "nonflammableLCC", "nonForestCanBeYoungAge", "sppEquivCol",
           "whichModulesToPrepare"))
  )
})
