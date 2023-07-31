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

# diag var names
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

## 3 parts -----------------------
estnames_brmcoda_d3 <- colnames(simsum_brmcoda_d3) %snin% c(condvars, rhatvars_d3, essvars_d3)

# ests of any runs with divergent transition to NA
simsum_brmcoda_d3 <- simsum_brmcoda_d3[ndt != 0 | zero != 0, (estnames_brmcoda_d3) := NA]
nrow(simsum_brmcoda_d3[is.na(b_Intercept)]) #520

# rhat d
apply(simsum_brmcoda_d3[, rhatvars_d3, with = FALSE], 2, function(x) sum(x > 1.01, na.rm = TRUE))

# ests of any runs with R hat > 1.01 to NA
simsum_brmcoda_d3 <- simsum_brmcoda_d3[simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3], (estnames_brmcoda_d3) := NA]
nrow(simsum_brmcoda_d3[is.na(b_Intercept)]) #6246

# ess
# ests of any runs with ESS < 400 to NA
simsum_brmcoda_d3 <- simsum_brmcoda_d3[simsum_brmcoda_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3], (estnames_brmcoda_d3) := NA]
nrow(simsum_brmcoda_d3[is.na(b_Intercept)]) #6890

## 4 parts -----------------------
estnames_brmcoda_d4 <- colnames(simsum_brmcoda_d4) %snin% c(condvars, rhatvars_d4, essvars_d4)

# ests of any runs with divergent transition to NA
simsum_brmcoda_d4 <- simsum_brmcoda_d4[ndt != 0 | zero != 0, (estnames_brmcoda_d4) := NA]
nrow(simsum_brmcoda_d4[is.na(b_Intercept)]) #392

# rhat
apply(simsum_brmcoda_d4[, rhatvars_d4, with = FALSE], 2, function(x) sum(x > 1.01, na.rm = TRUE))

# ests of any runs with R hat > 1.01 to NA
simsum_brmcoda_d4 <- simsum_brmcoda_d4[simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4], (estnames_brmcoda_d4) := NA]
nrow(simsum_brmcoda_d4[is.na(b_Intercept)]) #9794

# ess
# ests of any runs with ESS < 400 to NA
simsum_brmcoda_d4 <- simsum_brmcoda_d4[simsum_brmcoda_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4], (estnames_brmcoda_d4) := NA]
nrow(simsum_brmcoda_d4[is.na(b_Intercept)]) #10523

## 5 parts -----------------------
estnames_brmcoda_d5 <- colnames(simsum_brmcoda_d5) %snin% c(condvars, rhatvars_d5, essvars_d5)

# ests of any runs with divergent transition to NA
simsum_brmcoda_d5 <- simsum_brmcoda_d5[ndt != 0 | zero != 0, (estnames_brmcoda_d5) := NA]
nrow(simsum_brmcoda_d5[is.na(b_Intercept)]) #400

# rhat
apply(simsum_brmcoda_d5[, rhatvars_d5, with = FALSE], 2, function(x) sum(x > 1.01, na.rm = TRUE))

# ests of any runs with R hat > 1.01 to NA
simsum_brmcoda_d5 <- simsum_brmcoda_d5[simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5], (estnames_brmcoda_d5) := NA]
nrow(simsum_brmcoda_d5[is.na(b_Intercept)]) #10978

# ess
# ests of any runs with ESS < 400 to NA
simsum_brmcoda_d5 <- simsum_brmcoda_d5[simsum_brmcoda_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5], (estnames_brmcoda_d5) := NA]
nrow(simsum_brmcoda_d5[is.na(b_Intercept)]) #11550

# sub diag -------------------
simsum_sub_d3 <- simsum_sub[["simsum_sub_d3"]]
simsum_sub_d4 <- simsum_sub[["simsum_sub_d4"]]
simsum_sub_d5 <- simsum_sub[["simsum_sub_d5"]]

colnames(simsum_sub_d3)
colnames(simsum_sub_d4)
colnames(simsum_sub_d5)

## 3 parts -----------------------
estnames_sub_d3 <- colnames(simsum_sub_d3) %snin% c(condvars, rhatvars_d3, essvars_d3,
                                                    "Delta", "From", "To", "Level", "Reference")

# ests of any runs with divergent transition to NA
simsum_sub_d3 <- simsum_sub_d3[ndt != 0 | zero != 0, (estnames_sub_d3) := NA]
nrow(simsum_sub_d3[is.na(b_Intercept)]) #6240

# rhat
apply(simsum_sub_d3[, rhatvars_d3, with = FALSE], 2, function(x) sum(x > 1.01, na.rm = TRUE))

# ests of any runs with R hat > 1.01 to NA
simsum_sub_d3 <- simsum_sub_d3[simsum_sub_d3[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d3], (estnames_sub_d3) := NA]
nrow(simsum_sub_d3[is.na(b_Intercept)]) #74952

# ess
# ests of any runs with ESS < 400 to NA
simsum_sub_d3 <- simsum_sub_d3[simsum_sub_d3[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d3], (estnames_sub_d3) := NA]
nrow(simsum_sub_d3[is.na(b_Intercept)]) #82680

## 4 parts -----------------------
estnames_sub_d4 <- colnames(simsum_sub_d4) %snin% c(condvars, rhatvars_d4, essvars_d4,
                                                    "Delta", "From", "To", "Level", "Reference")

# ests of any runs with divergent transition to NA
simsum_sub_d4 <- simsum_sub_d4[ndt != 0 | zero != 0, (estnames_sub_d4) := NA]
nrow(simsum_sub_d4[is.na(b_Intercept)]) #9408

# rhat
apply(simsum_sub_d4[, rhatvars_d4, with = FALSE], 2, function(x) sum(x > 1.01, na.rm = TRUE))

# ests of any runs with R hat > 1.01 to NA
simsum_sub_d4 <- simsum_sub_d4[simsum_sub_d4[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d4], (estnames_sub_d4) := NA]
nrow(simsum_sub_d4[is.na(b_Intercept)]) #235056

# ess
# ests of any runs with ESS < 400 to NA
simsum_sub_d4 <- simsum_sub_d4[simsum_sub_d4[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d4], (estnames_sub_d4) := NA]
nrow(simsum_sub_d4[is.na(b_Intercept)]) #252552
## 5 parts -----------------------
estnames_sub_d5 <- colnames(simsum_sub_d5) %snin% c(condvars, rhatvars_d5, essvars_d5,
                                                    "Delta", "From", "To", "Level", "Reference")
# ests of any runs with divergent transition to NA
simsum_sub_d5 <- simsum_sub_d5[ndt != 0 | zero != 0, (estnames_sub_d5) := NA]
nrow(simsum_sub_d5[is.na(b_Intercept)]) #16000

# rhat
apply(simsum_sub_d5[, rhatvars_d5, with = FALSE], 2, function(x) sum(x > 1.01, na.rm = TRUE))

# ests of any runs with R hat > 1.01 to NA
simsum_sub_d5 <- simsum_sub_d5[simsum_sub_d5[, Reduce(`|`, lapply(.SD, `>`, 1.01)), .SDcols = rhatvars_d5], (estnames_sub_d5) := NA]
nrow(simsum_sub_d5[is.na(b_Intercept)]) #439120

# ess
# ests of any runs with ESS < 400 to NA
simsum_sub_d5 <- simsum_sub_d5[simsum_sub_d5[, Reduce(`|`, lapply(.SD, `<`, 400)), .SDcols = essvars_d5], (estnames_sub_d5) := NA]
nrow(simsum_sub_d5[is.na(b_Intercept)]) #462000

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

lapply(simsum_sub_d3[, essvars_d3, with = FALSE], min)
lapply(simsum_sub_d4[, essvars_d4, with = FALSE], min)
lapply(simsum_sub_d5[, essvars_d5, with = FALSE], min)

lapply(simsum_sub_d3[, essvars_d3, with = FALSE], max)
lapply(simsum_sub_d4[, essvars_d4, with = FALSE], max)
lapply(simsum_sub_d5[, essvars_d5, with = FALSE], max)

simsum_sub_d3[, ICC := (rint_sd^2)/(rint_sd^2 + res_sd^2)]
simsum_brmcoda_d3[, ICC := (rint_sd^2)/(rint_sd^2 + res_sd^2)]

egltable("ICC", data = simsum_brmcoda_d3[cond == "J: 1200, I: 14, sigma: RElarge_RESsmall"])

