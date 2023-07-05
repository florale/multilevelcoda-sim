library(data.table)
library(extraoperators)
library(compositions)
library(multilevelcoda)
library(brms)
library(cmdstanr)
library(insight)
library(MASS)

library(doFuture)
library(foreach)
library(parallel)
library(doRNG)
library(future)

## input ---------
input <- readRDS("input.RDS")
meanscovs <- input$meanscovs
prefit5 <- input$prefit5
prefit4 <- input$prefit4
prefit3 <- input$prefit3

source("input.R") # groundtruth, conditions and functions

## set different for each script -------
set.seed(17) 
sampled_cond <- cond[320001:340000] 

## model -------------------
registerDoFuture()
# plan(list(tweak(multisession, workers = 20L),
#           tweak(sequential)))
plan(multisession, workers = 20L)

starttime <- proc.time()
out17 <- vector("list", length = nrow(sampled_cond))
sim_model <- list()

out17 <- foreach(i = seq_len(nrow(sampled_cond)),
               .combine = c, .export = ls(globalenv())) %dorng% {
                 
                 N             <- sampled_cond[i, N]
                 K             <- sampled_cond[i, K]
                 rint_sd       <- sampled_cond[i, rint_sd]
                 res_sd        <- sampled_cond[i, res_sd]
                 run           <- sampled_cond[i, run]
                 n_parts       <- sampled_cond[i, n_parts]
                 sbp_n         <- sampled_cond[i, sbp]
                 prefit_n      <- sampled_cond[i, prefit]
                 groundtruth_n <- sampled_cond[i, groundtruth]
                 parts         <- sampled_cond[i, parts]
                 
                 # inputs
                 sbp           <- meanscovs[[paste(sbp_n)]]
                 prefit        <- get(prefit_n)
                 groundtruth   <- get(groundtruth_n)
                 parts         <- as.vector(strsplit(parts, " ")[[1]])
                 
                 # sim data ----------------------------------------------
                 simd <- with(meanscovs, rbind(
                   simulateData(
                     bm = BMeans,
                     wm = WMeans,
                     bcov = BCov,
                     wcov = WCov,
                     n = N,
                     k = K,
                     psi = psi)
                 ))
                 
                 simd[, Sleep := TST + WAKE]
                 simd[, PA := MVPA + LPA]

                 # ILR ---------------------------------------------------
                 cilr <- compilr(
                   data = simd,
                   sbp = sbp,
                   parts = parts,
                   idvar = "ID")
                 
                 tmp <- cbind(cilr$data,
                              cilr$BetweenILR,
                              cilr$WithinILR,
                              cilr$TotalILR)
                 
                 # random effects ----------------------------------------
                 redat <- data.table(ID = unique(tmp$ID),
                                     rint = rnorm(
                                       n = length(unique(tmp$ID)),
                                       mean = 0,
                                       sd = rint_sd))
                 
                 tmp <- merge(tmp, redat, by = "ID")
                 
                 # outcome -----------------------------------------------
                 if (n_parts == 3) {
                   tmp[, sleepy :=  rnorm(
                     n = nrow(simd),
                     mean = groundtruth$b_Intercept  + rint +
                       (groundtruth$b_bilr1 * bilr1) +
                       (groundtruth$b_bilr2 * bilr2) +
                       (groundtruth$b_wilr1 * wilr1) +
                       (groundtruth$b_wilr2 * wilr2),
                     sd = res_sd)]
                 }
                 
                 if (n_parts == 4) {
                   tmp[, sleepy :=  rnorm(
                     n = nrow(simd),
                     mean = groundtruth$b_Intercept  + rint +
                       (groundtruth$b_bilr1 * bilr1) +
                       (groundtruth$b_bilr2 * bilr2) +
                       (groundtruth$b_bilr3 * bilr3) +
                       (groundtruth$b_wilr1 * wilr1) +
                       (groundtruth$b_wilr2 * wilr2) +
                       (groundtruth$b_wilr3 * wilr3),
                     sd = res_sd)]
                 }
                 
                 if (n_parts == 5) {
                   tmp[, sleepy :=  rnorm(
                     n = nrow(simd),
                     mean = groundtruth$b_Intercept  + rint +
                       (groundtruth$b_bilr1 * bilr1) +
                       (groundtruth$b_bilr2 * bilr2) +
                       (groundtruth$b_bilr3 * bilr3) +
                       (groundtruth$b_bilr4 * bilr4) +
                       (groundtruth$b_wilr1 * wilr1) +
                       (groundtruth$b_wilr2 * wilr2) +
                       (groundtruth$b_wilr3 * wilr3) +
                       (groundtruth$b_wilr4 * wilr4),
                     sd = res_sd)]
                 }
                 
                 simd$sleepy <- tmp$sleepy

                 # model -------------------------------------------------
                 list(append(
                   simmodel(
                     database = simd,
                     sbpbase = sbp,
                     parts = parts,
                     prefit = prefit),
                   list(N = N,
                        K = K,
                        rint_sd = rint_sd,
                        res_sd = res_sd,
                        run = run,
                        n_parts = n_parts,
                        parts = parts)))
               } 

endtime <- proc.time()
endtime - starttime ## time to complete
saveRDS(out17, "out17.RDS")