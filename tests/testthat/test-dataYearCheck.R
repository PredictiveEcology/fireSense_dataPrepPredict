## dataYear is the vegetation and land-cover year of Biomass_core, Biomass_borealDataPrep, Biomass_speciesData
## and burnSummaries (default 2020). This module's default was 2011, and nothing compared them.
## paramCheckOtherMods() compares the values every module holds, defaults included.

stubVegModule <- function(dataYear) {
  mp <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(mp, "vegStub"))
  writeLines(sprintf('
defineModule(sim, list(name = "vegStub", description = "", keywords = "",
  authors = person("A", "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(vegStub = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year", citation = list(), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(defineParameter("dataYear", "numeric", %s, NA, NA, "")),
  inputObjects = bindrows(), outputObjects = bindrows()))
doEvent.vegStub <- function(sim, eventTime, eventType, debug = FALSE) invisible(sim)
', dataYear), file.path(mp, "vegStub", "vegStub.R"))
  mp
}

vegSim <- function(stubYear, params = list()) {
  mp <- stubVegModule(stubYear)
  SpaDES.core::simInit(times = list(start = 2001, end = 2001, timeunit = "year"),
                       modules = c(moduleName, "vegStub"),
                       params = c(stats::setNames(list(list(igAggFactor = 2)), moduleName), params),
                       objects = toyObjects(),
                       paths = utils::modifyList(testPaths, list(modulePath = c(testPaths$modulePath, mp))))
}

test_that("the default dataYear is 2020, as the vegetation modules'", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_identical(md$parameters$default[[match("dataYear", md$parameters$paramName)]], 2020)
  expect_no_error(suppressMessages(vegSim(2020)))
})

test_that("a module with another dataYear, by default or set, is an error; .globals sets all", {
  expect_error(suppressMessages(vegSim(2011)), "multiple values for dataYear")
  expect_error(suppressMessages(vegSim(2020, list(vegStub = list(dataYear = 2015)))), "multiple values for dataYear")
  expect_no_error(suppressMessages(vegSim(2020, list(.globals = list(dataYear = 2025)))))
})
