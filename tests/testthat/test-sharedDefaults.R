## Root cause: fireSense_dataPrepPredict.R hard-coded its own copies of `forestedLCC`,
## `cutoffForYoungAge`, `nonForestCanBeYoungAge`, `flammabilityThreshold`, `fuelClassCol` and
## `igAggFactor`, the same defaults fireSense_dataPrepFit hard-codes separately. A fit and its
## predictions are only consistent if both modules use the same values, so each now takes its
## default from `fireSenseUtils`'s shared constants (`?fireSenseUtils::fireSenseSharedDefaults`),
## as `nonflammableLCC` already did.

test_that("parameter defaults come from fireSenseUtils's shared constants", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  default <- function(name) md$parameters$default[md$parameters$paramName == name][[1]]

  expect_identical(default("forestedLCC"), fireSenseUtils::fireSenseForestedLCC)
  expect_identical(default("cutoffForYoungAge"), fireSenseUtils::fireSenseYoungAgeCutoff)
  expect_identical(default("nonForestCanBeYoungAge"), fireSenseUtils::fireSenseNonForestCanBeYoungAge)
  expect_identical(default("flammabilityThreshold"), fireSenseUtils::fireSenseFlammabilityThreshold)
  expect_identical(default("fuelClassCol"), fireSenseUtils::fireSenseFuelClassCol)
  expect_identical(default("igAggFactor"), fireSenseUtils::fireSenseIgAggFactor)
  expect_identical(default("scanfiVersion"), fireSenseUtils::fireSenseSCANFIVersion)
})
