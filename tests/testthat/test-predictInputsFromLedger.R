## A run without fireSense_dataPrepFit only predicts (a user who adds fireSense to a project to predict):
## nothing else reads the SpreadFit ledger, so `.inputObjects` does (predictInputsFromLedger()). Before
## 2026-11-02 such a run stopped in prepIgAndEscPredictData with "[subset] invalid name(s)": the climate
## variables were this module's 'MDC' default, not the fits' (CMD, CMDsm, ...), and the stored fits,
## the per-ELF species and fuel inputs and the lightning were missing (fireSense_dataPrepFit made them).

## two neighbour ELFs' rows, as fireSense_spreadFit writes them: 9.1 fitted with CMD, 9.2 with CMDsm (CMD_sm)
toyLedgerRows <- function() {
  groups <- list(nfLCC_grass = 16L, nfLCC_wetland = 19L)
  pars <- function(clim) {
    p <- as.data.frame(matrix(0, nrow = 1, ncol = 0))
    for (nm in c(fireSenseUtils::logisticParamNames[[1]], clim, "youngAge", names(groups))) p[[nm]] <- 0
    p
  }
  ledger <- data.frame(numIterations = c(1L, 1L))
  ledger[[fireSenseUtils::polygonIDTxt]] <- c("9.1", "9.2")
  ledger$params <- list(pars("CMD"), pars("CMDsm"))
  ledger$sppEquiv <- rep(list(data.table::data.table(LandR = c("Pice_mar", "Popu_tre"),
                                                     FuelClass = c("Pice_mar", "Popu_tre"))), 2)
  ledger$nonForestedLCCGroups <- list(groups, groups)
  ledger$missingLCCgroup <- c("nfLCC_grass", "nfLCC_grass")
  ledger
}

## a simList as `.inputObjects` would hand it over, with the ledger read mocked: no Drive. The toy
## objects supply `studyAreaWithSpreadParams = NULL`, so simInit() does not read it itself.
ledgerSim <- function(rows, objects = toyObjects(), times = list(start = 2021, end = 2023)) {
  objects$climateVariablesForFire <- NULL
  sim <- toyPrepSim(objects, times = times)
  sim$climateVariablesForFire <- NULL   # the toy run's .inputObjects default; not supplied by a user
  sim@current <- list(moduleName = moduleName, eventType = ".inputObjects", eventTime = 2021)  # as in .inputObjects
  rec <- new.env()
  testthat::local_mocked_bindings(
    readSpreadFitLedger = function(spreadFitFilename, cloudFolderID, domain, destinationPath, fitFileSuffix = "") {
      rec$suffix <- fitFileSuffix
      rows
    }, .package = "fireSenseUtils", .env = parent.frame())
  list(sim = sim, rec = rec)
}

test_that("the stored fits, their per-ELF inputs and climate variables come from the ledger", {
  x <- ledgerSim(toyLedgerRows())
  sim <- predictInputsFromLedger(x$sim)
  expect_identical(as.character(sim$studyAreaWithSpreadParams$polygonID), c("9.1", "9.2"))
  expect_setequal(names(sim$sppEquivs), c("9.1", "9.2"))
  expect_length(sim$nonForestedLCCGroupsList, 2L)
  expect_length(sim$missingLCCgroupList, 2L)
  ## spread: both fits' variables (prediction blends the ELFs); ignition: the fit defaults and those
  expect_setequal(sim$climateVariablesForFire$spread, c("CMD", "CMDsm"))
  expect_setequal(sim$climateVariablesForFire$ignition, c("CMD", "cumMDC", "CMDsm", "CMDsp"))
  ## canClimateData prepares them: historical climate for the simulated years; no canClimateData,
  ## so no projected years
  cv <- sim$climateVariables
  expect_setequal(names(cv), paste0("historical_", c("CMD", "cumMDC", "CMDsm", "CMDsp")))
  expect_identical(cv$historical_CMD$.dots$historical_years, 2021:2023)
  expect_identical(x$rec$suffix, "")   # the module default; .globals sets it for every module
})

test_that("climate variables a user supplies are kept", {
  objs <- toyObjects()
  x <- ledgerSim(toyLedgerRows(), objs)
  x$sim$climateVariablesForFire <- list(ignition = "CMD", spread = "CMD")
  sim <- predictInputsFromLedger(x$sim)
  expect_identical(sim$climateVariablesForFire, list(ignition = "CMD", spread = "CMD"))
})

test_that("years after the last historical climate year ask for no historical climate", {
  x <- ledgerSim(toyLedgerRows(), times = list(start = 2090, end = 2091))
  sim <- predictInputsFromLedger(x$sim)
  expect_false(any(grepl("^historical_", names(sim$climateVariables))))
})

test_that("a ledger without a fit for the study area stops, naming the ledger", {
  x <- ledgerSim(NULL)
  expect_error(predictInputsFromLedger(x$sim), "fitFileSuffix \"\" has no stored fit for this study area")
})

test_that(".inputObjects reads the ledger only when nothing supplies the stored fits", {
  read <- 0L
  testthat::local_mocked_bindings(readSpreadFitLedger = function(...) { read <<- read + 1L; toyLedgerRows() },
                                  .package = "fireSenseUtils")
  objs <- toyObjects()
  objs["studyAreaWithSpreadParams"] <- NULL                        # nothing supplies them
  objs["climateVariablesForFire"] <- NULL                          # the toy 'MDC' is not a ClimateNA name
  sim <- toyPrepSim(objs)
  expect_identical(read, 1L)
  expect_identical(as.character(sim$studyAreaWithSpreadParams$polygonID), c("9.1", "9.2"))
  objs$studyAreaWithSpreadParams <- toyLedgerRows()                # supplied (as by fireSense_dataPrepFit)
  toyPrepSim(objs)
  expect_identical(read, 1L)
})
