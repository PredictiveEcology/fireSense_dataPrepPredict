## `.inputObjects()` supplies defaults. Two of its branches are broken in ways that make the
## module unusable without a workaround, so they are pinned here -- as recorded defects, not
## as correct behaviour -- to make a fix detectable.

test_that("KNOWN BUG: .inputObjects leaves rstLCC_RTM NULL, so Init() cannot run", {
  ## `.inputObjects` (module source l.509-510) branches on `suppliedElsewhere("rstLCCs", sim)`.
  ## That branch is the only one that does not try to download NTEMS landcover, but it does
  ## `sim$rstLCC_RTM <- tail(sim$rstLCCs, 1)[[1]]`, and `rstLCCs` is not a declared input
  ## (it is absent from `inputObjects`, l.89-147), so it is not in the simList when
  ## `.inputObjects` runs. `tail(NULL, 1)[[1]]` is NULL, so `rstLCC_RTM` is left NULL.
  ##
  ## INTENDED: taking the `rstLCCs` branch should produce a usable `rstLCC_RTM`.
  ## Stated and recorded as failing. This does not assert that NULL is correct.
  objs <- toyObjects()   # supplies rstLCCs, does not supply rstLCC_RTM
  sim <- SpaDES.core::simInit(
    times = list(start = 2001, end = 2001, timeunit = "year"),
    modules = moduleName,
    params = stats::setNames(list(list(igAggFactor = 2)), moduleName),
    objects = objs,
    paths = testPaths
  )
  expect_failure(expect_s4_class(sim$rstLCC_RTM, "SpatRaster"))
  ## recorded actual state
  expect_null(sim$rstLCC_RTM)

  ## and the consequence: `Init()` calls `.compareRas(rasterToMatch, rstLCC_RTM)` at l.241,
  ## which errors on NULL, so the `init` event cannot complete and the module is unusable as
  ## shipped. This is why `toyPrepSim()` restores `rstLCC_RTM` after `simInit()` for every
  ## other test in this suite.
  expect_error(SpaDES.core::spades(sim, events = "init", debug = FALSE),
               "subscript out of bounds")
})

test_that("KNOWN BUG: .inputObjects cannot build flammableRTM when it is not supplied", {
  ## `.inputObjects` l.546 calls `defineFlammable(rstLCC, ...)`. `rstLCC` is a local created
  ## only in the *other* branch (l.512), and `defineFlammable` is not imported by the module,
  ## so with `rstLCCs` supplied and `flammableRTM` absent this is a guaranteed error.
  ## The correct construction exists in `Init()` (~l.241-253). Recorded, not fixed here.
  objs <- toyObjects()
  objs$flammableRTM <- NULL
  objs$landcoverDT <- NULL
  expect_error(
    SpaDES.core::simInit(
      times = list(start = 2001, end = 2001, timeunit = "year"),
      modules = moduleName,
      params = stats::setNames(list(list(igAggFactor = 2)), moduleName),
      objects = objs,
      paths = testPaths
    ),
    "defineFlammable|could not find function|object 'rstLCC' not found"
  )
})

test_that(".inputObjects supplies the default climateVariablesForFire", {
  ## the one branch of `.inputObjects` that works as documented (l.502-507)
  objs <- toyObjects()
  objs$climateVariablesForFire <- NULL
  sim <- toyPrepSim(objs)
  expect_identical(sim$climateVariablesForFire, list(spread = "MDC", ignition = "MDC"))
})
