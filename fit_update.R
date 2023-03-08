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

sbp <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)
psi <- gsi.buildilrBase(t(sbp))

## ground truth ---------

meanscovs <- readRDS("meanscovs.RDS")
groundtruth <- readRDS("groundtruth.RDS")

## functions ---------

simulateData <- function(bm, wm, bcov, wcov, n, k, psi) {
  simd.b <- rnorm.acomp(n = n, bm, bcov)
  simd.b <- as.data.table(simd.b)
  setnames(simd.b, c("BTST", "BWAKE", "BMVPA", "BLPA", "BSB"))
  
  simd.w <- rnorm.acomp(n = n * k, wm, wcov)
  simd.w <- clo(simd.w, total = 5)
  simd.w <- as.data.table(simd.w)
  setnames(simd.w, c("WTST", "WWAKE", "WMVPA", "WLPA", "WSB"))
  
  simd.b[, ID := seq_len(n)]
  simd.w[, ID := rep(seq_len(n), each = k)]
  
  simd.all <- merge(simd.b, simd.w, by = "ID")
  simd.all[, TST := WTST * BTST]
  simd.all[, WAKE := WWAKE * BWAKE]
  simd.all[, MVPA := WMVPA * BMVPA]
  simd.all[, LPA := WLPA * BLPA]
  simd.all[, SB := WSB * BSB]
  
  simd.t <- simd.all[, .(TST, WAKE, MVPA, LPA, SB)]
  simd.t <- clo(simd.t, total = 1440)
  simd.t <- as.data.table(simd.t)
  setnames(simd.t, c("TST", "WAKE", "MVPA", "LPA", "SB"))
  
  simd.t[, ID := rep(seq_len(n), each = k)]
  
  return(simd.t)
}

## conditions (1000 runs each condition) --------
cond <- as.data.table(expand.grid(N = c(30, 50, 360, 1200),
                                  K = c(3, 5, 7, 14)))
cond <- cond[rep(seq_len(.N), 1000)]
sampled_cond <- cond[1:2]

## model -------------------
set.seed(1) # set different for each script

N <- sampled_cond[1, N]
K <- sampled_cond[1, K]

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
                                 mean = 0, sd = 1),
                    rints = rnorm(n = length(unique(tmp$ID)),
                                  mean = 0, sd = sqrt(0.5)),
                    rintl = rnorm(n = length(unique(tmp$ID)),
                                  mean = 0, sd = sqrt(1.5)))

tmp <- merge(tmp, redat, by = "ID")

# outcome - simulated based on ml regression  -----------------------------
tmp[, sleepy :=  rnorm(n = nrow(simd),
                       mean = 2.20  + rint +
                         (-0.20 * bilr1) + (-0.01 * bilr2) + (-0.02 * bilr3) + (0.05 * bilr4) +
                         (+0.15 * wilr1) + (0.25 * wilr2) + (0.01 * wilr3) + (-0.15 * wilr4),
                       sd = 1)]
  
simd$sleepy <- tmp$sleepy
psub <- basesub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
parts <- colnames(psub)

cilr <- compilr(simd, sbp, parts, total = 1440, idvar = "ID")

mod <- brmcoda(cilr,
               sleepy ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 + (1 | ID),
               cores = 4, chains = 4, iter = 3000, warmup = 500, backend = "cmdstanr")
fit <- mod$Model

### 2nd run
N <- sampled_cond[2, N]
K <- sampled_cond[2, K]

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
                                 mean = 0, sd = 1),
                    rints = rnorm(n = length(unique(tmp$ID)),
                                  mean = 0, sd = sqrt(0.5)),
                    rintl = rnorm(n = length(unique(tmp$ID)),
                                  mean = 0, sd = sqrt(1.5)))

tmp <- merge(tmp, redat, by = "ID")

# outcome - simulated based on ml regression  -----------------------------
tmp[, sleepy :=  rnorm(n = nrow(simd),
                       mean = 2.20  + rint +
                         (-0.20 * bilr1) + (-0.01 * bilr2) + (-0.02 * bilr3) + (0.05 * bilr4) +
                         (+0.15 * wilr1) + (0.25 * wilr2) + (0.01 * wilr3) + (-0.15 * wilr4),
                       sd = 1)]

simd$sleepy <- tmp$sleepy

psub <- basesub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
parts <- colnames(psub)

cilr <- compilr(simd, sbp, parts, total = 1440, idvar = "ID")

dat <- cbind(cilr$data, cilr$BetweenILR, cilr$WithinILR)
fit_update <- update(fit, newdata = dat, recompile = FALSE)
mod_new <- brmcoda(cilr,
                   sleepy ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 + (1 | ID),
                   cores = 4, chains = 4, iter = 3000, warmup = 500, backend = "cmdstanr", fit = fit)
fit_fit <- mod_new$Model
## both fit and update dont recompile

# check
summary(fit)
summary(fit_fit) # using fit
summary(fit_update) # using update