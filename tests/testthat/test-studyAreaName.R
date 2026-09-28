## `.studyAreaName` names the study area in the landcover file and cache tags. Its default, NA,
## must become a real name before use, never "NA" in a file name.

test_that("an unset .studyAreaName becomes a hash of the study area", {
  objs <- toyObjects()
  sim <- SpaDES.core::spades(toyPrepSim(objs), events = "init", debug = FALSE)
  nm <- SpaDES.core::P(sim, module = moduleName)$.studyAreaName
  expect_false(is.na(nm))
  expect_identical(nm, reproducible::studyAreaName(objs$studyArea))
})

test_that("without studyArea, an unset .studyAreaName stays NA", {
  objs <- toyObjects()
  objs$studyArea <- NULL
  sim <- SpaDES.core::spades(toyPrepSim(objs), events = "init", debug = FALSE)
  expect_true(is.na(SpaDES.core::P(sim, module = moduleName)$.studyAreaName))
})

test_that("a supplied .studyAreaName is kept", {
  sim <- SpaDES.core::spades(toyPrepSim(params = list(.studyAreaName = "myArea")),
                             events = "init", debug = FALSE)
  expect_identical(SpaDES.core::P(sim, module = moduleName)$.studyAreaName, "myArea")
})
