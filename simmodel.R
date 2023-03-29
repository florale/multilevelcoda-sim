library(data.table)
library(extraoperators)
library(compositions)
library(multilevelcoda)
library(brms)
library(cmdstanr)
library(insight)

library(doFuture)
library(foreach)
library(parallel)
library(MASS)

## input ---------
meanscovs <- readRDS("meanscovs.RDS")
groundtruth <- readRDS("groundtruth.RDS")
source("input.R") # conditions and functions

## set different for each script -------
set.seed(1) 
sampled_cond <- cond[1:1] 

## model -------------------
out <- vector("list", length = nrow(sampled_cond))

# plan(cluster, workers = availableWorkers())
registerDoFuture()
plan(multisession, workers = 4L)

starttime <- proc.time()
for (i in seq_len(nrow(sampled_cond))) {
  
  N <- sampled_cond[i, N]
  K <- sampled_cond[i, K]
  rint_sd <- sampled_cond[i, rint_sd]
  res_sd <- sampled_cond[i, res_sd]
  run <- sampled_cond[i, run]
  
  simd <- with(meanscovs, rbind(
    simulateData(
      bm = BMeans, wm = WMeans,
      bcov = BCov, wcov = WCov,
      n = N, k = K, psi = psi)))

    # ILR ---------------------------------------------------------------------
  cilr <- compilr(
    data = simd,
    sbp = meanscovs$sbp,
    parts = c("TST", "WAKE", "MVPA", "LPA", "SB"), idvar = "ID")

  tmp <- cbind(cilr$data, cilr$BetweenILR, cilr$WithinILR, 
               cilr$TotalILR)

  # random effects ----------------------------------------------------------
  redat <- data.table(ID = unique(tmp$ID),
                      rint = rnorm(n = length(unique(tmp$ID)),
                                   mean = 0, sd = rint_sd))
  
  tmp <- merge(tmp, redat, by = "ID")

  # outcome - simulated based on ml regression  -----------------------------
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
    sd = res_sd
  )]

  simd$sleepy <- tmp$sleepy

  if (i == 1) {
    out[[i]] <- simmodel(database = simd, sbpbase = meanscovs$sbp)
    } else {
      prefit <- out[[1]]$Result$brmsfit
      out[[i]] <- simmodel(database = simd, sbpbase = meanscovs$sbp, prefit = prefit)
      }
}
endtime <- proc.time()
endtime - starttime ## time to complete
saveRDS(out, "out.RDS", compress = "xz")

prefit <- out[[1]]$Result$brmsfit
saveRDS(prefit, "prefit.RDS", compress = "xz")
