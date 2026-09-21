## `.inputObjects()` supplies defaults. Two of its branches used to be broken in ways that made
## the module unusable without a workaround; these tests pin the fixed behaviour.

test_that(".inputObjects takes rstLCC_RTM from a supplied rstLCCs, and init then runs", {
  ## `rstLCCs` used not to be a declared input, so it was absent from the simList when
  ## `.inputObjects` ran, `rstLCC_RTM` was left NULL and `Init()` errored in `.compareRas()`.
  objs <- toyObjects()   # supplies rstLCCs, does not supply rstLCC_RTM
  sim <- SpaDES.core::simInit(
    times = list(start = 2001, end = 2001, timeunit = "year"),
    modules = moduleName,
    params = stats::setNames(list(list(igAggFactor = 2)), moduleName),
    objects = objs,
    paths = testPaths
  )
  expect_s4_class(sim$rstLCC_RTM, "SpatRaster")
  expect_identical(as.vector(terra::values(sim$rstLCC_RTM)), as.vector(terra::values(toyLCC())))
  expect_no_error(SpaDES.core::spades(sim, events = "init", debug = FALSE))
})

test_that(".inputObjects builds flammableRTM from rstLCC_RTM when it is not supplied", {
  ## This used to call `defineFlammable(rstLCC, ...)` with a local that only exists in the
  ## other branch, which was a guaranteed error when `rstLCCs` was supplied.
  objs <- toyObjects()
  objs$flammableRTM <- NULL
  objs$landcoverDT <- NULL
  objs$rstLCCs <- list(terra::as.int(toyLCC())) # `defineFlammable()` wants integer landcover
  sim <- SpaDES.core::simInit(
    times = list(start = 2001, end = 2001, timeunit = "year"),
    modules = moduleName,
    params = stats::setNames(list(list(igAggFactor = 2)), moduleName),
    objects = objs,
    paths = testPaths
  )
  ## only cell 16 (class 20) is in `nonflammableLCC`
  expect_equal(as.vector(terra::values(sim$flammableRTM)), c(rep(1, 15), 0))
})

test_that(".inputObjects supplies the default climateVariablesForFire", {
  ## the one branch of `.inputObjects` that works as documented (l.502-507)
  objs <- toyObjects()
  objs$climateVariablesForFire <- NULL
  sim <- toyPrepSim(objs)
  expect_identical(sim$climateVariablesForFire, list(spread = "MDC", ignition = "MDC"))
})
