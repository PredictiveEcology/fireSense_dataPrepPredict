[1mdiff --git a/fireSense_dataPrepPredict.R b/fireSense_dataPrepPredict.R[m
[1mindex 17eaa8a..3e88cad 100644[m
[1m--- a/fireSense_dataPrepPredict.R[m
[1m+++ b/fireSense_dataPrepPredict.R[m
[36m@@ -288,7 +288,7 @@[m [mprepare_IgnitionAndEscapePredict <- function(sim) {[m
 [m
     #following changes to ignitionModel - prediction will now occur at same spatial scale,[m
     # location of predicted ignitions will be randomly drawn from finer scale[m
[31m-    browser()[m
[32m+[m
     covNames <- setdiff(names(climateCovariates, "pixelID"))[m
     covRas <- lapply(covNames, function(cov, rtm = sim$flammableRTM, covDT = ignitionCovariates) {[m
       covRas <- rast(rtm)[m
