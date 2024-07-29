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
source("1c_simmodel_input.R") # groundtruth, conditions and functions
set.seed(2023)
sampled_cond <- cond[1]

## SIM MODEL
simmodel5 <- function(database, sbpbase, prefit = NULL) {
  psub <- basesub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
  parts <- colnames(psub)
  
  cilr <-
    compilr(database, sbpbase, parts, total = 1440, idvar = "ID")
  
  model <- list()
  # model --------
    m <-
      brmcoda(
        cilr,
        sleepy ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 + (1 | ID),
        cores = 4,
        chains = 4,
        iter = 3000,
        warmup = 500,
        backend = "cmdstanr"
      )
    
    submodel <- substitution(
      m,
      delta = c(10, 20, 30),
      level = c("between", "within"),
      ref = "grandmean"
    )
    
    model <- list(
      ModelSummary = summary(m$Model),
      Substitution = submodel,
      ndt = sum(subset( # number of divergent transitions
        nuts_params(m$Model), Parameter == "divergent__")$Value),
      brmsfit = m$Model
    )
  
  list(
    CompILR = cilr,
    Result = model
  )
}

simmodel4 <- function(database, sbpbase, prefit = NULL) {
  psub <- basesub(c("Sleep", "MVPA", "LPA", "SB"))
  parts <- colnames(psub)
  
  cilr <-
    compilr(database, sbpbase, parts, total = 1440, idvar = "ID")
  
  model <- list()
  # model --------
    m <-
      brmcoda(
        cilr,
        sleepy ~ bilr1 + bilr2 + bilr3 + wilr1 + wilr2 + wilr3 + (1 | ID),
        cores = 4,
        chains = 4,
        iter = 3000,
        warmup = 500,
        backend = "cmdstanr"
      )
    
    submodel <- substitution(
      m,
      delta = c(10, 20, 30),
      level = c("between", "within"),
      ref = "grandmean"
    )
    
    model <- list(
      ModelSummary = summary(m$Model),
      Substitution = submodel,
      ndt = sum(subset( # number of divergent transitions
        nuts_params(m$Model), Parameter == "divergent__")$Value),
      brmsfit = m$Model
    )
  
  list(
    CompILR = cilr,
    Result = model
  )
}

simmodel3 <- function(database, sbpbase, prefit = NULL) {
  psub <- basesub(c("Sleep", "PA", "SB"))
  parts <- colnames(psub)
  
  cilr <-
    compilr(database, sbpbase, parts, total = 1440, idvar = "ID")
  
  model <- list()
  # model --------
    m <-
      brmcoda(
        cilr,
        sleepy ~ bilr1 + bilr2 + wilr1 + wilr2 + (1 | ID),
        cores = 4,
        chains = 4,
        iter = 3000,
        warmup = 500,
        backend = "cmdstanr"
      )
    
    submodel <- substitution(
      m,
      delta = c(10, 20, 30),
      level = c("between", "within"),
      ref = "grandmean"
    )
    
    model <- list(
      ModelSummary = summary(m$Model),
      Substitution = submodel,
      ndt = sum(subset( # number of divergent transitions
        nuts_params(m$Model), Parameter == "divergent__")$Value),
      brmsfit = m$Model
    )
  
  list(
    CompILR = cilr,
    Result = model
  )
}

### prefit 3 class classification --------------
registerDoFuture()
plan(multisession, workers = 4L)
starttime <- proc.time()
out3 <- vector("list", length = nrow(sampled_cond))

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
    mean = groundtruth4$b_Intercept  + rint +
      (groundtruth4$b_bilr1 * bilr1) +
      (groundtruth4$b_bilr2 * bilr2) +
      (groundtruth4$b_bilr3 * bilr3) +
      (groundtruth4$b_wilr1 * wilr1) +
      (groundtruth4$b_wilr2 * wilr2) +
      (groundtruth4$b_wilr3 * wilr3),
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
    sbp = meanscovs$sbp5,
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
    mean = groundtruth5$b_Intercept  + rint +
      (groundtruth5$b_bilr1 * bilr1) +
      (groundtruth5$b_bilr2 * bilr2) +
      (groundtruth5$b_bilr3 * bilr3) +
      (groundtruth5$b_bilr4 * bilr4) +
      (groundtruth5$b_wilr1 * wilr1) +
      (groundtruth5$b_wilr2 * wilr2) +
      (groundtruth5$b_wilr3 * wilr3) +
      (groundtruth5$b_wilr4 * wilr4),
    sd = res_sd
  )]
  
  simd$sleepy <- tmp$sleepy
  
  if (i == 1) {
    out5[[i]] <- simmodel5(database = simd, sbpbase = meanscovs$sbp5)
  } else {
    prefit <- out5[[1]]$Result$brmsfit
    out5[[i]] <- simmodel5(database = simd, sbpbase = meanscovs$sbp5, prefit = prefit)
  }
}

prefit5 <- out5[[1]]$Result$brmsfit
saveRDS(prefit5, "prefit5.RDS", compress = "xz")

input <- list(
  meanscovs = meanscovs,
  prefit5 = prefit5,
  prefit4 = prefit4,
  prefit3 = prefit3
)
saveRDS(input, "input.RDS", compress = "xz")
