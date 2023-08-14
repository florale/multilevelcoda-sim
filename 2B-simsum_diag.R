library(JWileymisc)
library(data.table)
library(extraoperators)
library(compositions)
library(multilevelcoda)
library(brms)
library(cmdstanr)

library(doFuture)
library(foreach)
library(doRNG)
library(parallel)

# source("simsum_in.R") # raw output from sim
simsum_sub <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_sub.RDS")
simsum_brmcoda <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda.RDS")

# diag var names ----------------
condvars <- c("N", "K", "J", "I", "D", "rint_sd", "res_sd", "run", 
              "ndt", "zero", "cond",
              "condition", "sigma_condition", "u0_condition")

rhatvars_d3 <- c("rhat_Intercept", 
                 "rhat_bilr1", "rhat_bilr2", 
                 "rhat_wilr1", "rhat_wilr2",
                 "rhat_u0", "rhat_sigma")
rhatvars_d4 <- c("rhat_Intercept", 
                 "rhat_bilr1", "rhat_bilr2", "rhat_bilr3", 
                 "rhat_wilr1", "rhat_wilr2","rhat_wilr3",
                 "rhat_u0", "rhat_sigma")
rhatvars_d5 <- c("rhat_Intercept", 
                 "rhat_bilr1", "rhat_bilr2", "rhat_bilr3", "rhat_bilr4",
                 "rhat_wilr1", "rhat_wilr2","rhat_wilr3", "rhat_wilr4",
                 "rhat_u0", "rhat_sigma")

essvars_d3 <- c("bess_Intercept", 
                "bess_bilr1", "bess_bilr2", 
                "bess_wilr1", "bess_wilr2",
                "bess_u0", "bess_sigma",
                "tess_Intercept", 
                "tess_bilr1", "tess_bilr2", 
                "tess_wilr1", "tess_wilr2",
                "tess_u0", "tess_sigma")

essvars_d4 <- c("bess_Intercept", 
                "bess_bilr1", "bess_bilr2", "bess_bilr3", 
                "bess_wilr1", "bess_wilr2","bess_wilr3",
                "bess_u0", "bess_sigma",
                "tess_Intercept", 
                "tess_bilr1", "tess_bilr2", "tess_bilr3", 
                "tess_wilr1", "tess_wilr2","tess_wilr3",
                "tess_u0", "tess_sigma")

essvars_d5 <- c("bess_Intercept", 
                "bess_bilr1", "bess_bilr2", "bess_bilr3", "bess_bilr4",
                "bess_wilr1", "bess_wilr2","bess_wilr3", "bess_wilr4",
                "bess_u0", "bess_sigma",
                "tess_Intercept", 
                "tess_bilr1", "tess_bilr2", "tess_bilr3", "tess_bilr4",
                "tess_wilr1", "tess_wilr2","tess_wilr3", "tess_wilr4",
                "tess_u0", "tess_sigma")

# brmcoda diag -------------------
simsum_brmcoda_d3 <- simsum_brmcoda[["simsum_brmcoda_d3"]]
simsum_brmcoda_d4 <- simsum_brmcoda[["simsum_brmcoda_d4"]]
simsum_brmcoda_d5 <- simsum_brmcoda[["simsum_brmcoda_d5"]]

colnames(simsum_brmcoda_d3)
colnames(simsum_brmcoda_d4)
colnames(simsum_brmcoda_d5)

estnames_brmcoda_d3 <- colnames(simsum_brmcoda_d3) %snin% c(condvars, rhatvars_d3, essvars_d3)
estnames_brmcoda_d4 <- colnames(simsum_brmcoda_d4) %snin% c(condvars, rhatvars_d4, essvars_d4)
estnames_brmcoda_d5 <- colnames(simsum_brmcoda_d5) %snin% c(condvars, rhatvars_d5, essvars_d5)

## ndt -----------------------
# ests of any runs with divergent transition to NA

simsum_brmcoda_d3 <- simsum_brmcoda_d3[ndt != 0 | zero != 0, (estnames_brmcoda_d3) := NA]
simsum_brmcoda_d4 <- simsum_brmcoda_d4[ndt != 0 | zero != 0, (estnames_brmcoda_d4) := NA]
simsum_brmcoda_d5 <- simsum_brmcoda_d5[ndt != 0 | zero != 0, (estnames_brmcoda_d5) := NA]

nrow(simsum_brmcoda_d3[ndt != 0]) #520
nrow( simsum_brmcoda_d4[ndt != 0]) #392
nrow(simsum_brmcoda_d5[ndt != 0]) #400

egltable(condvars, data = rbind(simsum_brmcoda_d3[ndt != 0, ..condvars],
                                simsum_brmcoda_d4[ndt != 0, ..condvars],
                                simsum_brmcoda_d5[ndt != 0, ..condvars]
))

## rhat -----------------------
apply(simsum_brmcoda_d3[, rhatvars_d3, with = FALSE], 2, function(x) sum(x > 1.01, na.rm = TRUE))
apply(simsum_brmcoda_d4[, rhatvars_d4, with = FALSE], 2, function(x) sum(x > 1.01, na.rm = TRUE))
apply(simsum_brmcoda_d5[, rhatvars_d5, with = FALSE], 2, function(x) sum(x > 1.01, na.rm = TRUE))

nrow(simsum_brmcoda_d3[simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3]])
nrow(simsum_brmcoda_d4[simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4]])
nrow(simsum_brmcoda_d5[simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5]])
# ests of any runs with R hat > 1.01 to NA

# simsum_brmcoda_d3 <- simsum_brmcoda_d3[simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3], (estnames_brmcoda_d3) := NA]
# simsum_brmcoda_d4 <- simsum_brmcoda_d4[simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4], (estnames_brmcoda_d4) := NA]
# simsum_brmcoda_d5 <- simsum_brmcoda_d5[simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5], (estnames_brmcoda_d5) := NA]

egltable(condvars, data = rbind(simsum_brmcoda_d3[simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3], ..condvars],
                                simsum_brmcoda_d4[simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4], ..condvars],
                                simsum_brmcoda_d5[simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5], ..condvars]
))

## ess -----------------------
# ests of any runs with ESS < 400 to NA
nrow(simsum_brmcoda_d3[simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3]])
nrow(simsum_brmcoda_d4[simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4]])
nrow(simsum_brmcoda_d5[simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5]])

# simsum_brmcoda_d3 <- simsum_brmcoda_d3[simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3], (estnames_brmcoda_d3) := NA]
# simsum_brmcoda_d4 <- simsum_brmcoda_d4[simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4], (estnames_brmcoda_d4) := NA]
# simsum_brmcoda_d5 <- simsum_brmcoda_d5[simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5], (estnames_brmcoda_d5) := NA]

egltable(condvars, data = rbind(simsum_brmcoda_d3[simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3], ..condvars],
                                simsum_brmcoda_d4[simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4], ..condvars],
                                simsum_brmcoda_d5[simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5], ..condvars]
))

egltable(condvars, data = rbind(simsum_brmcoda_d3[simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3] |
                                                  simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3], ..condvars],
                                simsum_brmcoda_d4[simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4] |
                                                  simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4], ..condvars],
                                simsum_brmcoda_d5[simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5] |
                                                  simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5], ..condvars]
))
nrow(rbind(simsum_brmcoda_d3[simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3] |
                               simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3], ..condvars],
           simsum_brmcoda_d4[simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4] |
                               simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4], ..condvars],
           simsum_brmcoda_d5[simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5] |
                               simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5], ..condvars]
           ))
# 27651
# sub diag -------------------
simsum_sub_d3 <- simsum_sub[["simsum_sub_d3"]]
simsum_sub_d4 <- simsum_sub[["simsum_sub_d4"]]
simsum_sub_d5 <- simsum_sub[["simsum_sub_d5"]]

colnames(simsum_sub_d3)
colnames(simsum_sub_d4)
colnames(simsum_sub_d5)

estnames_sub_d3 <- colnames(simsum_sub_d3) %snin% c(condvars, rhatvars_d3, essvars_d3,
                                                    "Delta", "From", "To", "Level", "Reference")
estnames_sub_d4 <- colnames(simsum_sub_d4) %snin% c(condvars, rhatvars_d4, essvars_d4,
                                                    "Delta", "From", "To", "Level", "Reference")
estnames_sub_d5 <- colnames(simsum_sub_d5) %snin% c(condvars, rhatvars_d5, essvars_d5,
                                                    "Delta", "From", "To", "Level", "Reference")
## ndt -----------------------
# ests of any runs with divergent transition to NA
simsum_sub_d3 <- simsum_sub_d3[ndt != 0 | zero != 0, (estnames_sub_d3) := NA]
simsum_sub_d4 <- simsum_sub_d4[ndt != 0 | zero != 0, (estnames_sub_d4) := NA]
simsum_sub_d5 <- simsum_sub_d5[ndt != 0 | zero != 0, (estnames_sub_d5) := NA]

nrow(simsum_sub_d4[is.na(b_Intercept)]) #9408
nrow(simsum_sub_d3[is.na(b_Intercept)]) #6240
nrow(simsum_sub_d5[is.na(b_Intercept)]) #16000

egltable(condvars, data = rbind(simsum_sub_d3[ndt != 0, ..condvars],
                                simsum_sub_d4[ndt != 0, ..condvars],
                                simsum_sub_d5[ndt != 0, ..condvars]
))

## rhat -----------------------

nrow(simsum_sub_d3[simsum_sub_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3]])
nrow(simsum_sub_d4[simsum_sub_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4]])
nrow(simsum_sub_d5[simsum_sub_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5]])

# simsum_sub_d3 <- simsum_sub_d3[simsum_sub_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3], (estnames_sub_d3) := NA]
# simsum_sub_d4 <- simsum_sub_d4[simsum_sub_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4], (estnames_sub_d4) := NA]
# simsum_sub_d5 <- simsum_sub_d5[simsum_sub_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5], (estnames_sub_d5) := NA]

egltable(condvars, data = rbind(simsum_sub_d3[simsum_sub_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3], ..condvars],
                                simsum_sub_d4[simsum_sub_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4], ..condvars],
                                simsum_sub_d5[simsum_sub_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5], ..condvars]
))


## ess -----------------------
# ests of any runs with ESS < 400 to NA

# simsum_sub_d3 <- simsum_sub_d3[simsum_sub_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3], (estnames_sub_d3) := NA]
# simsum_sub_d4 <- simsum_sub_d4[simsum_sub_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4], (estnames_sub_d4) := NA]
# simsum_sub_d5 <- simsum_sub_d5[simsum_sub_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5], (estnames_sub_d5) := NA]

egltable(condvars, data = rbind(simsum_sub_d3[simsum_sub_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3], ..condvars],
                                simsum_sub_d4[simsum_sub_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4], ..condvars],
                                simsum_sub_d5[simsum_sub_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5], ..condvars]
))

egltable(condvars, data = rbind(simsum_sub_d3[simsum_sub_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3] |
                                                simsum_sub_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3], ..condvars],
                                simsum_sub_d4[simsum_sub_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4] |
                                                simsum_sub_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4], ..condvars],
                                simsum_sub_d5[simsum_sub_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5] |
                                                simsum_sub_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5], ..condvars]
))
nrow(rbind(simsum_sub_d3[simsum_sub_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3] |
                           simsum_sub_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3], ..condvars],
           simsum_sub_d4[simsum_sub_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4] |
                           simsum_sub_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4], ..condvars],
           simsum_sub_d5[simsum_sub_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5] |
                           simsum_sub_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5], ..condvars]
))

## DESC -------------------------
## Descriptive of conditions of problematic runs
egltable(condvars, data = simsum_brmcoda_d3[is.na(b_Intercept)])
egltable(condvars, data = simsum_brmcoda_d4[is.na(b_Intercept)])
egltable(condvars, data = simsum_brmcoda_d5[is.na(b_Intercept)])

egltable("cond", data = simsum_brmcoda_d3[is.na(b_Intercept)])
egltable("cond", data = simsum_brmcoda_d4[is.na(b_Intercept)])
egltable("cond", data = simsum_brmcoda_d5[is.na(b_Intercept)])

nrow(simsum_brmcoda_d3[is.na(b_Intercept)])/nrow(simsum_brmcoda_d3)*100
nrow(simsum_brmcoda_d4[is.na(b_Intercept)])/nrow(simsum_brmcoda_d4)*100
nrow(simsum_brmcoda_d5[is.na(b_Intercept)])/nrow(simsum_brmcoda_d5)*100

nrow(simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & !is.na(b_Intercept)]) 
nrow(simsum_brmcoda_d4[cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & !is.na(b_Intercept)]) 
nrow(simsum_brmcoda_d5[cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & !is.na(b_Intercept)]) 

nrow(simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: REsmall_RESlarge" & !is.na(b_Intercept)])
nrow(simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & !is.na(b_Intercept)]) 
nrow(simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: REbase_RESlarge" & !is.na(b_Intercept)]) 
nrow(simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: REbase_RESsmall" & !is.na(b_Intercept)]) 
nrow(simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: base" & !is.na(b_Intercept)]) 

lapply(simsum_sub_d3[, essvars_d3, with = FALSE], quantile , probs = .05)
lapply(simsum_sub_d4[, essvars_d4, with = FALSE], min)
lapply(simsum_sub_d5[, essvars_d5, with = FALSE], min)

lapply(simsum_sub_d3[, essvars_d3, with = FALSE], max)
lapply(simsum_sub_d4[, essvars_d4, with = FALSE], max)
lapply(simsum_sub_d5[, essvars_d5, with = FALSE], max)

egltable("ICC", data = simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: base"])
egltable("ICC", data = simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: RElarge_RESsmall"])
egltable("ICC", data = simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: REsmall_RESlarge"])
egltable("ICC", data = simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: REbase_RESlarge"])
egltable("ICC", data = simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: REbase_RESsmall"])

# save diag stats ------------------
## D3 --------------------
diagvars_d3 <- c(essvars_d3, rhatvars_d3, "ndt")
out <- list()
simsum_diag_d3 <- lapply(diagvars_d3, function(x) {
  out[[x]] <- as.data.table((t(egltable(x, g = "cond", data = simsum_brmcoda_d3))), keep.rownames = TRUE)
  setnames(out[[x]], c("condition", "Value"))
  out[[x]][, c("Diag_Stat", "par") := tstrsplit(x, "_")]
  out[[x]][, D := 3]
  out[[x]][, c("condition", "MSD") := tstrsplit(condition, " M")][, MSD := NULL]
  out[[x]] <- out[[x]][-c(1, .N)]
})
names(simsum_diag_d3) <- diagvars_d3

## D4 --------------------
diagvars_d4 <- c(essvars_d4, rhatvars_d4, "ndt")
out <- list()
simsum_diag_d4 <- lapply(diagvars_d4, function(x) {
  out[[x]] <- as.data.table((t(egltable(x, g = "cond", data = simsum_brmcoda_d4))), keep.rownames = TRUE)
  setnames(out[[x]], c("condition", "Value"))
  out[[x]][, c("Diag_Stat", "par") := tstrsplit(x, "_")]
  out[[x]][, D := 4]
  out[[x]][, c("condition", "MSD") := tstrsplit(condition, " M")][, MSD := NULL]
  out[[x]] <- out[[x]][-c(1, .N)]
})
names(simsum_diag_d4) <- diagvars_d4

## D5--------------------
diagvars_d5 <- c(essvars_d5, rhatvars_d5, "ndt")
out <- list()
simsum_diag_d5 <- lapply(diagvars_d5, function(x) {
  out[[x]] <- as.data.table((t(egltable(x, g = "cond", data = simsum_brmcoda_d5))), keep.rownames = TRUE)
  setnames(out[[x]], c("condition", "Value"))
  out[[x]][, c("Diag_Stat", "par") := tstrsplit(x, "_")]
  out[[x]][, D := 5]
  out[[x]][, c("condition", "MSD") := tstrsplit(condition, " M")][, MSD := NULL]
  out[[x]] <- out[[x]][-c(1, .N)]
})
names(simsum_diag_d5) <- diagvars_d5

## save results by condition/parameter to present full results in shiny app -------------
simsum_diag <- rbind(rbindlist(simsum_diag_d3),
                     rbindlist(simsum_diag_d4),
                     rbindlist(simsum_diag_d5))

simsum_diag[, Estimand := NA]
simsum_diag[, Estimand := ifelse(par == "Intercept", "b_Intercept", Estimand)]
simsum_diag[, Estimand := ifelse(par == "bilr1", "b_bilr1", Estimand)]
simsum_diag[, Estimand := ifelse(par == "bilr2", "b_bilr2", Estimand)]
simsum_diag[, Estimand := ifelse(par == "wilr1", "b_wilr1", Estimand)]
simsum_diag[, Estimand := ifelse(par == "wilr2", "b_wilr2", Estimand)]
simsum_diag[, Estimand := ifelse(par == "bilr3", "b_bilr3", Estimand)]
simsum_diag[, Estimand := ifelse(par == "wilr3", "b_wilr3", Estimand)]
simsum_diag[, Estimand := ifelse(par == "bilr4", "b_bilr4", Estimand)]
simsum_diag[, Estimand := ifelse(par == "wilr4", "b_wilr4", Estimand)]
simsum_diag[, Estimand := ifelse(par == "u0", "sd_ID_Intercept", Estimand)]
simsum_diag[, Estimand := ifelse(par == "sigma", "sigma", Estimand)]
simsum_diag[, Estimand := ifelse(par == "ndt", NA, Estimand)]

# reshape ndt
simsum_diag_ndt <- reshape(simsum_diag[Diag_Stat == "ndt", -c("par")], idvar = c("condition", "D"), timevar = "Diag_Stat", direction = "wide")
setnames(simsum_diag_ndt, "Value.ndt", "ndt")

# reshape rest
simsum_diag_rest <- reshape(simsum_diag[Diag_Stat != "ndt", -c("par")], idvar = c("Estimand", "condition", "D"), timevar = "Diag_Stat", direction = "wide")
setnames(simsum_diag_rest, "Value.bess", "Bulk_ESS")
setnames(simsum_diag_rest, "Value.tess", "Tail_ESS")
setnames(simsum_diag_rest, "Value.rhat", "R_hat")

# merge
simsum_diag <- merge(simsum_diag_rest, simsum_diag_ndt[, -c("Estimand.ndt")], by = c("condition", "D"))

## save raw results to get descriptive stats ---------------------
brmcoda_diag <- rbind(
  melt(simsum_brmcoda_d3[, ..diagvars_d3]),
  melt(simsum_brmcoda_d4[, ..diagvars_d4]),
  melt(simsum_brmcoda_d5[, ..diagvars_d5])
)
brmcoda_diag[, c("Diag_Stat", "par") := tstrsplit(variable, "_")]
saveRDS(brmcoda_diag, "/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/brmcoda_diag.RDS")
saveRDS(simsum_diag, "/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_diag.RDS")
