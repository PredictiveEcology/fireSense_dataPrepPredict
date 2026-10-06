## fireSense_spreadFit leaves in sim$studyAreaWithSpreadParams every ledger row whose polygon touches this
## ELF's study area: the ELF itself and its neighbours (NRV run of ELF 13.1, 2026-10-05: rows 4.2.2, 13.1,
## 14.3). fuelClassRolesForELF(sim, 1L) took row 1 by position, which was neighbour 4.2.2, an older fit with
## an `other_agb` term, and the prediction stopped. The rows must be picked by polygonID, and a neighbour
## from an older model generation dropped with a warning, never stopping the run.

## a ledger row: polygonID, one parameter set whose column names are the fitted terms, the fit's sppEquiv
## and covMinMax_spread
ownRowsLedgerRow <- function(id, termNames, sppEquiv = toySppEquiv()) {
  p <- matrix(1, nrow = 1, ncol = length(termNames), dimnames = list(NULL, termNames))
  data.frame(polygonID = id, params = I(list(as.data.frame(p))), sppEquiv = I(list(sppEquiv)),
             covMinMax_spread = I(list(list(MDC = c(0, 1)))))
}

## neighbour first, with an `other_agb` term; the simulated ELF second; a usable neighbour third
threeRows <- function() rbind(
  ownRowsLedgerRow("4.2.2", c("CMD", "youngAge", "dom_agb_class1", "sec_agb_class2", "other_agb")),
  ownRowsLedgerRow("13.1", c("CMD", "youngAge", "dom_agb_class1", "sec_agb_class2")),
  ownRowsLedgerRow("14.3", c("CMD", "youngAge", "dom_agb_class2", "sec_agb_class1"))
)

test_that("blendNeighbourELFs = FALSE keeps only the simulated ELF's own row", {
  sa <- selectSpreadParamRows(threeRows(), ownELF = "13.1", blend = FALSE,
                              placeableELFs = c("4.2.2", "13.1", "14.3"),
                              fuelInputELFs = c("4.2.2", "13.1", "14.3"), fuelClassCol = "FuelClass")
  expect_identical(sa$polygonID, "13.1")
})

test_that("blendNeighbourELFs = TRUE keeps the own row first and usable neighbours, warning for an old one", {
  expect_warning(
    sa <- selectSpreadParamRows(threeRows(), ownELF = "13.1", blend = TRUE,
                                placeableELFs = c("4.2.2", "13.1", "14.3"),
                                fuelInputELFs = c("4.2.2", "13.1", "14.3"), fuelClassCol = "FuelClass"),
    "4\\.2\\.2.*other_agb")
  expect_identical(sa$polygonID, c("13.1", "14.3"))
})

test_that("a neighbour fitted with per-fuel-class terms is dropped with a warning", {
  sa <- rbind(ownRowsLedgerRow("13.1", c("CMD", "dom_agb_class1", "sec_agb_class2")),
              ownRowsLedgerRow("14.3", c("CMD", "youngAge", "class1", "class2")))
  expect_warning(
    out <- selectSpreadParamRows(sa, ownELF = "13.1", blend = TRUE, placeableELFs = c("13.1", "14.3"),
                                 fuelInputELFs = c("13.1", "14.3"), fuelClassCol = "FuelClass"),
    "14\\.3.*class1")
  expect_identical(out$polygonID, "13.1")
})

test_that("a usable neighbour that cannot be placed or has no fuel inputs is dropped with a warning", {
  ## a single-ELF run: sim$rasterToMatchLargeELF labels only core and buffer, not ELFs
  expect_warning(
    out <- selectSpreadParamRows(threeRows()[2:3, ], ownELF = "13.1", blend = TRUE,
                                 placeableELFs = c("1", "2"), fuelInputELFs = c("13.1", "14.3"),
                                 fuelClassCol = "FuelClass"),
    "14\\.3.*rasterToMatchLargeELF")
  expect_identical(out$polygonID, "13.1")
  expect_warning(
    out <- selectSpreadParamRows(threeRows()[2:3, ], ownELF = "13.1", blend = TRUE,
                                 placeableELFs = c("13.1", "14.3"), fuelInputELFs = character(0),
                                 fuelClassCol = "FuelClass"),
    "14\\.3.*sppEquivs")
  expect_identical(out$polygonID, "13.1")
})

test_that("a missing own row stops with a message naming the ELF", {
  expect_error(selectSpreadParamRows(threeRows()[c(1, 3), ], ownELF = "13.1", blend = FALSE,
                                     placeableELFs = character(0), fuelInputELFs = character(0),
                                     fuelClassCol = "FuelClass"),
               "13\\.1")
})

test_that("prediction uses the simulated ELF's row when a neighbour's row comes first", {
  ## the user-visible symptom: on development this stopped with "the fitted model has an `other_agb` term"
  o <- toyObjects()
  o$.ELFind <- "13.1"
  o$studyAreaWithSpreadParams <- threeRows()
  for (blend in c(FALSE, TRUE)) {
    out <- suppressWarnings(toyPrepRun(o, params = list(blendNeighbourELFs = blend)))
    expect_identical(out$studyAreaWithSpreadParams$polygonID, "13.1", info = blend)
    sp <- covDF(out$fireSense_SpreadCovariates)
    ## 13.1's fuel roles, not 14.3's (dom_agb_class2) nor 4.2.2's
    expect_true(all(c("dom_agb_class1", "sec_agb_class2") %in% names(sp)), info = blend)
    expect_false("dom_agb_class2" %in% names(sp), info = blend)
  }
})

test_that("with ELF-labelled rasterToMatchLargeELF and per-ELF inputs, TRUE blends a usable neighbour", {
  sppB <- data.table::data.table(LandR = c("Pice_mar", "Pinu_ban", "Popu_tre"),
                                 FuelClass = c("class1", "class1", "class2"))
  o <- toyObjects()
  o$landcoverDT <- NULL
  o$.ELFind <- "13.1"
  o$studyAreaWithSpreadParams <- threeRows()
  ## named by polygonID, in ledger order, as fireSense_dataPrepFit makes them
  o$sppEquivs <- list("4.2.2" = toySppEquiv(), "13.1" = toySppEquiv(), "14.3" = sppB)
  o$nonForestedLCCGroupsList <- list("4.2.2" = list(wetland = 19L, grass = 16L),
                                     "13.1" = list(wetland = 19L, grass = 16L),
                                     "14.3" = list(wetgrs = c(16L, 19L)))
  o$missingLCCgroupList <- list("4.2.2" = "grass", "13.1" = "grass", "14.3" = "wetgrs")
  elf <- toyRast(rep(c(1, 2), each = 8))
  levels(elf) <- data.frame(value = 1:2, ELFind = c("13.1", "14.3"))
  o$rasterToMatchLargeELF <- elf
  expect_warning(out <- toyPrepRun(o), "4\\.2\\.2.*other_agb")
  expect_identical(out$studyAreaWithSpreadParams$polygonID, c("13.1", "14.3"))
  sp <- covDF(out$fireSense_SpreadCovariates)
  ## each ELF's own fitted roles and non-forest groups
  expect_true(all(c("dom_agb_class1", "sec_agb_class2", "dom_agb_class2", "sec_agb_class1",
                    "wetland", "grass", "wetgrs") %in% names(sp)))
})
