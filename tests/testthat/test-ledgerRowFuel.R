## fireSense_dataPrepFit makes sppEquivs, nonForestedLCCGroupsList and missingLCCgroupList from the ledger read
## BEFORE the fit. A fit-then-predict run of an ELF with no earlier row (ELF 4.2.1, GPSC) then has a ledger row
## for itself that those lists lack, and prediction stopped with "sppEquivs has no entry for ELF(s) 4.2.1".
## The fuel inputs are now taken from the ledger row's own sppEquiv / nonForestedLCCGroups / missingLCCgroup
## columns, as fireSense_spreadFit writes them; the lists are only the fallback for rows without them.

## a ledger row as fireSense_spreadFit writes it; a NULL fuel column is an empty entry
fuelLedgerRow <- function(id, termNames, sppEquiv = NULL, nonForestedLCCGroups = NULL, missingLCCgroup = NULL) {
  p <- matrix(1, nrow = 1, ncol = length(termNames), dimnames = list(NULL, termNames))
  data.frame(polygonID = id, params = I(list(as.data.frame(p))),
             covMinMax_spread = I(list(list(MDC = c(0, 1)))), sppEquiv = I(list(sppEquiv)),
             nonForestedLCCGroups = I(list(nonForestedLCCGroups)), missingLCCgroup = I(list(missingLCCgroup)))
}

ownTerms <- c("CMD", "youngAge", "dom_agb_class1", "sec_agb_class2")
nfOwn <- list(nfLCC_wetgrs = c(16L, 19L))
nfNeighbour <- list(nfLCC_wetland = 19L, nfLCC_grass = 16L)

## the run of ELF 13.1 whose lists were made before its own fit: neighbour 14.3 only
afterFitObjects <- function(ownRow) {
  o <- toyObjects()
  o$landcoverDT <- NULL
  o$.ELFind <- "13.1"
  o$studyAreaWithSpreadParams <- rbind(
    fuelLedgerRow("14.3", ownTerms, toySppEquiv(), nfNeighbour, "nfLCC_grass"), ownRow)
  o$sppEquivs <- list("14.3" = toySppEquiv())
  o$nonForestedLCCGroupsList <- list("14.3" = nfNeighbour)
  o$missingLCCgroupList <- list("14.3" = "nfLCC_grass")
  o
}

test_that("an ELF fitted in this run predicts with the fuel inputs of its own ledger row", {
  ## the user-visible symptom: on development this stopped with "sppEquivs has no entry for ELF(s) 13.1"
  o <- afterFitObjects(fuelLedgerRow("13.1", ownTerms, toySppEquiv(), nfOwn, "nfLCC_wetgrs"))
  out <- toyPrepRun(o)
  expect_identical(out$studyAreaWithSpreadParams$polygonID, "13.1")
  sp <- covDF(out$fireSense_SpreadCovariates)
  expect_true(all(c("dom_agb_class1", "sec_agb_class2", "nfLCC_wetgrs") %in% names(sp)))
  ## the row's non-forest groups, not the neighbour's from the list
  expect_false(any(c("nfLCC_wetland", "nfLCC_grass") %in% names(sp)))
})

test_that("rows without the fuel columns still use the lists", {
  o <- afterFitObjects(fuelLedgerRow("13.1", ownTerms))
  o$sppEquivs <- c(o$sppEquivs, list("13.1" = toySppEquiv()))
  o$nonForestedLCCGroupsList <- c(o$nonForestedLCCGroupsList, list("13.1" = nfOwn))
  o$missingLCCgroupList <- c(o$missingLCCgroupList, list("13.1" = "nfLCC_wetgrs"))
  sp <- covDF(toyPrepRun(o)$fireSense_SpreadCovariates)
  expect_true("nfLCC_wetgrs" %in% names(sp))
  expect_false("nfLCC_grass" %in% names(sp))
})

test_that("a row with neither its own columns nor list entries still stops, naming the ELF", {
  expect_error(toyPrepRun(afterFitObjects(fuelLedgerRow("13.1", ownTerms))), "sppEquivs has no entry for ELF\\(s\\) 13\\.1")
})
