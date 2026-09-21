## .inputObjects() branched on `suppliedElsewhere("rstLCCs", sim)`, but `rstLCCs` was not a declared
## input, so a user-supplied `rstLCCs` was TRUE for the branch and NULL in the simList: rstLCC_RTM
## stayed NULL and Init() then died in .compareRas(). And in the OTHER branch `defineFlammable()`
## referenced a local `rstLCC` that only exists inside that branch.

test_that("a supplied rstLCCs is visible to .inputObjects() and sets rstLCC_RTM", {
  lcc <- toyRastP(toyLCCvalsP(), "lcc")
  objs <- toyInputsP(rstLCCs = list(year2001 = lcc))
  objs$rstLCC_RTM <- NULL   # must be derived from rstLCCs
  objs$flammableRTM <- NULL # exercises the defineFlammable() path too
  objs$landcoverDT <- NULL

  sim <- toySimInitP(objects = objs, start = 2001, end = 2001)
  expect_s4_class(sim$rstLCC_RTM, "SpatRaster")
  expect_equal(as.vector(terra::values(sim$rstLCC_RTM)), as.vector(terra::values(lcc)))
  ## 20 is in P(sim)$nonflammableLCC, everything else is flammable
  expect_equal(as.vector(terra::values(sim$flammableRTM)),
                   as.numeric(toyLCCvalsP() != 20L))
})

test_that("a supplied rstLCC_RTM is left alone and never triggers a download", {
  objs <- toyInputsP()
  objs$rstLCCs <- NULL
  sim <- toySimInitP(objects = objs, start = 2001, end = 2001)
  expect_equal(as.vector(terra::values(sim$rstLCC_RTM)), as.double(toyLCCvalsP()))
})
