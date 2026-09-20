## The two covariate-assembly events. The structural facts -- which pixels appear, which
## climate layer and which year they carry, the column order, the non-forest columns -- are
## worked out by hand from the toy landscape. The fuel-class magnitudes come out of
## fireSenseUtils and are PINNED FROM AN OBSERVED RUN of this suite; they are identical on
## `origin/development` and on this branch (verified by running this file against both), but
## they are a regression pin on fireSenseUtils' internals, not a hand-derived truth.

test_that("prepSpreadPredictData builds one row per flammable pixel with this year's climate", {
  sim <- toyPrepRun()
  df <- covDF(sim$fireSense_SpreadCovariates)

  ## the 16th toy pixel is non-flammable, so exactly 15 rows at flammableRTM resolution
  expect_identical(df$pixelID, 1:15)
  ## the declared column order: pixelID, the climate layers, youngAge, then the fuels
  expect_identical(names(df)[1:3], c("pixelID", "MDC", "youngAge"))
  expect_setequal(names(df), c("pixelID", "MDC", "youngAge", "class1", "class2",
                               "wetland", "grass"))

  ## hand-computed: spread climate is MDC, and toy MDC for 2001 is 100 + cellNumber, so a
  ## value of 100 + pixelID proves both the right variable and the right year were taken
  expect_identical(df$MDC, 100 + 1:15)

  ## hand-computed from the toy map: cells 9, 10, 15 are wetland.
  expect_identical(which(df$wetland == 1L), c(9L, 10L, 15L))
  ## grass is 11 and 12 from the map, PLUS 13 and 14: those two are forested landcover (210)
  ## with no cohort in `pixelGroupMap`, and `missingLCCgroup` is "grass", so the module
  ## assigns them there. That is the documented behaviour of `missingLCCgroup`
  ## (metadata l.106-109), not an accident, so it is asserted as correct.
  expect_identical(which(df$grass == 1L), c(11L, 12L, 13L, 14L))

  ## hand-computed: youngAge is set where max cohort age <= cutoffForYoungAge (pixel group 3,
  ## cells 5 and 6, whose only cohort is 10 years old) and in non-forest pixels burned within
  ## the cutoff (cell 15, TSD 5). Nowhere else.
  expect_identical(which(df$youngAge == 1), c(5L, 6L, 15L))
})

test_that("spread fuel classes follow the sppEquiv fuel column (regression pin)", {
  ## REGRESSION PIN: these are the values observed from
  ## fireSenseUtils:::fireSenseCovariatesCreate() when this suite was written. They are
  ## identical on origin/development and on this branch (verified by running this file
  ## against both), but they are pinned observations of that package's internals, NOT
  ## hand-derived truths: only the structural assertions below are hand-checked. They are
  ## recorded so that a change in how biomass is mapped to fuel classes is visible here
  ## rather than downstream in a fitted model.
  sim <- toyPrepRun()
  df <- covDF(sim$fireSense_SpreadCovariates)
  expect_equal(df$class1[1:4], c(8.006368, 8.006368, 3.605170, 3.605170), tolerance = 1e-6)
  expect_equal(df$class2[1:4], c(3.605170, 3.605170, 6.214608, 6.214608), tolerance = 1e-6)
  ## structural, hand-checked, and independent of the pinning: pixel group 1 (cells 1-2) is
  ## the only group with two class1 cohorts, so it must carry the largest class1 value, and
  ## pixel group 2 (cells 3-4) is pure class2, so its class2 must exceed pixel group 1's
  expect_true(all(df$class1[1:2] > df$class1[3:4]))
  expect_true(all(df$class2[3:4] > df$class2[1:2]))
  ## a forested pixel with no cohorts (13, 14) gets the baseline, not NA
  expect_false(anyNA(df$class1))
  expect_false(anyNA(df$class2))
})

test_that("non-forest pixels never carry forest fuel (the module's own sanity check)", {
  sim <- toyPrepRun()
  df <- covDF(sim$fireSense_SpreadCovariates)
  nonForest <- df$wetland == 1L | df$grass == 1L
  baseline <- min(df$class1)
  expect_true(all(df$class1[nonForest] == baseline))
  expect_true(all(df$class2[nonForest] == min(df$class2)))
})

test_that("prepIgAndEscPredictData aggregates to the igAggFactor grid", {
  sim <- toyPrepRun()
  df <- covDF(sim$fireSense_igAndEscapePred_Covariates)

  ## igAggFactor 2 over a 4 x 4 grid gives a 2 x 2 grid, i.e. four coarse cells; the fourth
  ## contains the non-flammable pixel 16 and drops out, leaving three rows
  expect_identical(df$pixelID, 1:3)
  ## the year column carries time(sim), not the layer index
  expect_identical(unique(df$year), 2001)
  expect_setequal(names(df), c("pixelID", "MDC", "youngAge", "wetland", "grass",
                               "class1", "class2", "year", "lightningDays"))
  ## `ignitions` belongs to the Fit table and is dropped here
  expect_false("ignitions" %in% names(df))

  ## hand-computed: coarse cell 1 is fine cells 1, 2, 5, 6, whose MDC is 101, 102, 105, 106,
  ## mean 103.5; coarse cell 2 is 3, 4, 7, 8 -> 105.5; coarse cell 3 is 9, 10, 13, 14 -> 111.5
  expect_equal(df$MDC, c(103.5, 105.5, 111.5))
  ## hand-computed: youngAge is the mean over the coarse cell; cells 5 and 6 of four are
  ## young in coarse cell 1 -> 0.5; none in coarse cell 2; none in coarse cell 3 (cell 15 is
  ## in coarse cell 4, which is dropped)
  expect_equal(df$youngAge, c(0.5, 0, 0))
  ## hand-computed: coarse cell 3 covers wetlands 9, 10 and grasses 11? no -- fine cells
  ## 9, 10, 13, 14: 9 and 10 are wetland -> 0.5 wetland
  expect_equal(df$wetland, c(0, 0, 0.5))
})

test_that("igAggFactor 1 leaves the ignition covariates at flammableRTM resolution", {
  sim <- toyPrepRun(params = list(igAggFactor = 1))
  df <- covDF(sim$fireSense_igAndEscapePred_Covariates)
  ## no aggregation: the same 15 flammable pixels as the spread table, with the same climate
  expect_identical(df$pixelID, 1:15)
  expect_equal(df$MDC, 100 + 1:15)
  expect_identical(which(df$wetland == 1), c(9L, 10L, 15L))
})

test_that("a different ignition climate variable selects a different layer", {
  ## Tmax is the negative of MDC in the toy climate, so the sign of the aggregated value
  ## alone proves which variable the ignition table used.
  objs <- toyObjects()
  objs$climateVariablesForFire <- list(ignition = "Tmax", spread = "MDC")
  sim <- toyPrepRun(objs)
  ig <- covDF(sim$fireSense_igAndEscapePred_Covariates)
  sp <- covDF(sim$fireSense_SpreadCovariates)
  expect_true("Tmax" %in% names(ig))
  expect_false("MDC" %in% names(ig))
  expect_equal(ig$Tmax, c(-103.5, -105.5, -111.5))
  ## and the spread table is unaffected
  expect_identical(sp$MDC, 100 + 1:15)
})

test_that("a climate variable that is not in currentClimateRasters is an error", {
  objs <- toyObjects()
  objs$climateVariablesForFire <- list(ignition = "notAVariable", spread = "MDC")
  ## terra's own subsetting rejects it before the module's NULL guard can fire; asserted as
  ## an error with a message, so that silently returning an empty table would fail here
  expect_error(toyPrepRun(objs), "invalid name")
})

test_that("KNOWN BUG: the covariate tables do NOT advance with simulation time", {
  ## Intended behaviour: each year's covariates carry that year's climate. They do not.
  ## `getCurrentClimate()` (module source l.296) only builds `sim$currentClimateRasters` when
  ## it `is.null()`. The `getClimateRasters` event reschedules itself every year, but from the
  ## second year on the object is already populated, so the whole build is skipped and the
  ## climate stays frozen at `start(sim)`. Every later year's covariates are silently stale.
  ##
  ## The toy climate encodes the year in the value (year<Y> MDC cell n = (Y-2000)*100 + n),
  ## so this is unambiguous: after running 2001 -> 2003 the table still holds year2001 values.
  ## Below, the INTENDED assertions are stated and recorded as currently failing. When the
  ## staleness is fixed, `expect_failure()` will itself fail and these must be unwrapped.
  sim <- toyPrepRun(times = list(start = 2001, end = 2003))
  sp <- covDF(sim$fireSense_SpreadCovariates)
  ig <- covDF(sim$fireSense_igAndEscapePred_Covariates)

  ## INTENDED: the final year is 2003, so MDC should be 300 + pixelID
  expect_failure(expect_identical(sp$MDC, 300 + 1:15))
  expect_failure(expect_equal(ig$MDC, c(303.5, 305.5, 311.5)))

  ## What actually happens, recorded so the staleness is visible and the fix is detectable:
  ## the year2001 layer, unchanged after three simulated years.
  expect_identical(sp$MDC, 100 + 1:15)

  ## and the giveaway that this really is staleness rather than a mislabelled table: the
  ## `year` column DOES track time(sim), so the table claims 2003 while carrying 2001 climate
  expect_identical(unique(ig$year), 2003)
})

test_that("KNOWN BUG: lightningDays is NaN in the ignition table", {
  ## `lightningMaps` is supplied with a `lightningDays` layer holding 1:16, so the aggregated
  ## mean over each 2 x 2 coarse cell is finite and easily computed by hand (5.5, 7.5, 13.5).
  ## What comes back is NaN for every row. The layer is selected correctly -- single-bracket
  ## `sim$lightningMaps["lightningDays"]` (module source l.416) returns the right one-layer
  ## raster with the right values, checked directly -- so the loss happens inside
  ## `fireSenseUtils::mergePreparedCovs()`. Not diagnosed further here.
  ##
  ## The INTENDED assertion is stated and recorded as failing. This test does NOT assert that
  ## NaN is correct; it asserts that the column is present and that the intended value is not
  ## yet produced.
  sim <- toyPrepRun()
  ig <- covDF(sim$fireSense_igAndEscapePred_Covariates)
  expect_true("lightningDays" %in% names(ig))
  ## INTENDED: the mean of the fine-cell lightningDays over each coarse cell
  expect_failure(expect_equal(ig$lightningDays, c(5.5, 7.5, 13.5)))
  ## recorded actual state, so that producing ANY finite value here will trip this test
  expect_true(all(is.nan(ig$lightningDays)))
})

test_that("ageNonForest runs as an event and ages the TSD raster each year", {
  sim <- toyPrepRun(times = list(start = 2001, end = 2002))
  ## hand-computed: the toy TSD starts at c(5, 30 x 13, 5, 30) and the event fires once,
  ## at 2002, with no rstCurrentBurn supplied
  expect_identical(as.vector(terra::values(sim$nonForest_timeSinceDisturbance)),
                   c(6, rep(31, 13), 6, 31))
})
