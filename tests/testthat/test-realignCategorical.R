## Init() realigns rstLCC_RTM onto rasterToMatch when they differ. Landcover is categorical, so the
## postProcess() default (bilinear) invents classes that are in no landcover legend, and
## makeLandcoverDT() then returns an all-NA table of the right shape: a silent failure.

test_that("misaligned categorical landcover is realigned with nearest neighbour", {
  lcc <- toyRastP(toyLCCvalsP(), "lcc")
  ## same extent and CRS, shifted half a pixel, so .compareRas() is FALSE and the branch is taken
  misaligned <- terra::shift(lcc, dx = 125, dy = 125)

  sim <- toySimInitP(objects = toyInputsP(rstLCC_RTM = misaligned), start = 2001, end = 2001)
  sim$landcoverDT <- NULL # so that Init(), not .inputObjects(), builds it
  sim <- toyRunEventsP(sim, "init") # Init() builds landcoverDT from the realigned landcover

  dt <- sim$landcoverDT
  expect_s3_class(dt, "data.table")
  ## the real symptom: an all-NA landcoverDT apart from pixelID
  nonID <- setdiff(names(dt), "pixelID")
  expect_false(all(vapply(nonID, function(nm) all(is.na(dt[[nm]])), logical(1))))
})
