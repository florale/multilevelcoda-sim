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
source("input.R") # groundtruth, conditions and functions

### prefit 3 class classification --------------
registerDoFuture()
plan(multisession, workers = 4L)
out3 <- vector("list", length = nrow(sampled_cond))
sampled_cond <- cond[1]
starttime <- proc.time()

for (i in seq_len(nrow(sampled_cond))) {
  
  N <- sampled_cond[i, N]
  K <- sampled_cond[i, K]
  rint_sd <- sampled_cond[i, rint_sd]
  res_sd <- sampled_cond[i, res_sd]
  run <- sampled_cond[i, run]
  
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
    sbp = meanscovs$sbp3,
    parts = c("Sleep", "PA", "SB"),
    idvar = "ID"
  )
  
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
  
  # outcome - simulated based on ml regression  -----------
  tmp[, sleepy :=  rnorm(
    n = nrow(simd),
    mean = groundtruth3$b_Intercept  + rint +
      (groundtruth3$b_bilr1 * bilr1) +
      (groundtruth3$b_bilr2 * bilr2) +
      (groundtruth3$b_wilr1 * wilr1) +
      (groundtruth3$b_wilr2 * wilr2),
    sd = res_sd)]
  
  simd$sleepy <- tmp$sleepy
  
  if (i == 1) {
    out3[[i]] <- simmodel3(database = simd, sbpbase = meanscovs$sbp3)
  } else {
    prefit <- out3[[1]]$Result$brmsfit
    out3[[i]] <- simmodel3(database = simd, sbpbase = meanscovs$sbp3, prefit = prefit)
  }
}
prefit3 <- out3[[1]]$Result$brmsfit
saveRDS(prefit3, "prefit3.RDS", compress = "xz")

### prefit 4 class classification --------------
registerDoFuture()
plan(multisession, workers = 4L)
out4 <- vector("list", length = nrow(sampled_cond))
sampled_cond <- cond[1]
starttime <- proc.time()

for (i in seq_len(nrow(sampled_cond))) {
  
  N <- sampled_cond[i, N]
  K <- sampled_cond[i, K]
  rint_sd <- sampled_cond[i, rint_sd]
  res_sd <- sampled_cond[i, res_sd]
  run <- sampled_cond[i, run]
  
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
    sbp = meanscovs$sbp4,
    parts = c("Sleep", "MVPA", "LPA", "SB"), idvar = "ID")
  
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
      (groundtruth$b_wilr1 * wilr1) +
      (groundtruth$b_wilr2 * wilr2) +
      (groundtruth$b_wilr3 * wilr3),
    sd = res_sd
  )]
  
  simd$sleepy <- tmp$sleepy
  
  if (i == 1) {
    out4[[i]] <- simmodel4(database = simd, sbpbase = meanscovs$sbp4)
  } else {
    prefit <- out4[[1]]$Result$brmsfit
    out4[[i]] <- simmodel4(database = simd, sbpbase = meanscovs$sbp4, prefit = prefit)
  }}

prefit4 <- out4[[1]]$Result$brmsfit
saveRDS(prefit4, "prefit4.RDS", compress = "xz")

### prefit 4 class classification --------------
registerDoFuture()
plan(multisession, workers = 4L)
out5 <- vector("list", length = nrow(sampled_cond))
sampled_cond <- cond[1]
starttime <- proc.time()

for (i in seq_len(nrow(sampled_cond))) {
  
  N <- sampled_cond[i, N]
  K <- sampled_cond[i, K]
  rint_sd <- sampled_cond[i, rint_sd]
  res_sd <- sampled_cond[i, res_sd]
  run <- sampled_cond[i, run]
  
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
    out5[[i]] <- simmodel(database = simd, sbpbase = meanscovs$sbp)
  } else {
    prefit <- out5[[1]]$Result$brmsfit
    out5[[i]] <- simmodel(database = simd, sbpbase = meanscovs$sbp, prefit = prefit)
  }
}

prefit5 <- out5[[1]]$Result$brmsfit
saveRDS(prefit5, "prefit5.RDS", compress = "xz")
