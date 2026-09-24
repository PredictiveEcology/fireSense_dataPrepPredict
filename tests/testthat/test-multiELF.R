## Several fitted ELFs in one study area (the 2-ELF Mackenzie forecast, 2026-09). Each ELF's model predicts
## over its own pixels and a blend zone around them (fireSense_SpreadPredict), so every pixel needs every
## ELF's covariates: each ELF's fuel classes and non-forest groups, made from its own sppEquiv. Columns two
## ELFs share have the same name and so the same content.
##
## ELF A groups spruce and pine as "conifer", aspen as "decid"; non-forest groups wetland and grass.
## ELF B keeps spruce and pine apart, aspen as "decid" (shared with A); one non-forest group "wetgrs".
## Expectation, without hand numbers: the merged table holds the union of columns, and each column equals
## what a one-ELF run with that ELF's objects makes.

sppA <- function() data.table::data.table(LandR = c("Pice_mar", "Pinu_ban", "Popu_tre"),
                                          FuelClass = c("conifer", "conifer", "decid"))
sppB <- function() data.table::data.table(LandR = c("Pice_mar", "Pinu_ban", "Popu_tre"),
                                          FuelClass = c("Pice_mar", "Pinu_ban", "decid"))
nfA <- list(wetland = 19L, grass = 16L); nfB <- list(wetgrs = c(16L, 19L))

oneELF <- function(spp, nf, missing) {
  o <- toyObjects()
  o$sppEquiv <- spp(); o$nonForestedLCCGroups <- nf; o$missingLCCgroup <- missing
  o$landcoverDT <- NULL                          # made from this ELF's groups, as for each ELF below
  toyPrepRun(o)
}

test_that("with two ELFs, the covariates are both ELFs' own, side by side", {
  o <- toyObjects()
  o$landcoverDT <- NULL
  o$sppEquivs <- list(sppA(), sppB())
  o$nonForestedLCCGroupsList <- list(nfA, nfB)
  o$missingLCCgroupList <- list("grass", "wetgrs")
  both <- toyPrepRun(o)
  a <- oneELF(sppA, nfA, "grass"); b <- oneELF(sppB, nfB, "wetgrs")

  sp <- covDF(both$fireSense_SpreadCovariates)
  spA <- covDF(a$fireSense_SpreadCovariates); spB <- covDF(b$fireSense_SpreadCovariates)
  expect_setequal(names(sp), union(names(spA), names(spB)))
  expect_identical(sp$pixelID, spA$pixelID)
  for (cn in setdiff(names(spA), "pixelID")) expect_equal(sp[[cn]], spA[[cn]], info = cn)
  for (cn in setdiff(names(spB), "pixelID")) expect_equal(sp[[cn]], spB[[cn]], info = cn)
  expect_true(all(c("conifer", "Pice_mar", "Pinu_ban", "decid", "wetland", "grass", "wetgrs") %in% names(sp)))

  ## ignition covariates: the same union, each ELF's layers as in its own run
  ig <- as.data.frame(both$fireSense_igAndEscapePred_Covariates)
  igA <- as.data.frame(a$fireSense_igAndEscapePred_Covariates)
  igB <- as.data.frame(b$fireSense_igAndEscapePred_Covariates)
  expect_true(all(union(names(igA), names(igB)) %in% names(ig)))
  key <- intersect(c("pixelID", "year"), names(ig))
  ord <- function(d) d[do.call(order, d[key]), , drop = FALSE]
  ig <- ord(ig); igA <- ord(igA); igB <- ord(igB)
  for (cn in setdiff(names(igA), key)) expect_equal(ig[[cn]], igA[[cn]], info = cn)
  for (cn in setdiff(names(igB), key)) expect_equal(ig[[cn]], igB[[cn]], info = cn)
})

test_that("mismatched per-ELF lists stop with a message", {
  o <- toyObjects()
  o$sppEquivs <- list(sppA(), sppB()); o$nonForestedLCCGroupsList <- list(nfA); o$missingLCCgroupList <- list("grass", "wetgrs")
  expect_error(toyPrepRun(o), "one element per ELF")
})
