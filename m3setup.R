library(MASS)
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

library(ggplot2)
library(ggsci)

## synd <- readRDS("syntheticpopulation.RDS")
## synd <- setDT(synd)

## sbp <- matrix(c(
##   1, 1, -1,-1, -1,
##   1, -1, 0, 0, 0,
##   0, 0, 1, -1, -1,
##   0, 0, 0, 1, -1), ncol=5, byrow=TRUE)

simulateData <- function(bm, wm, bcov, wcov, n, k, psi) {
  simd.b <- mvrnorm(n = n, mu = bm, Sigma = bcov)
  simd.b <- as.data.table(simd.b)

  inv.b <- ilrInv(simd.b[, .(V1, V2, V3, V4)], V = psi)
  inv.b <- clo(inv.b, total = 1440)

  simd.b <- cbind(inv.b, simd.b[, .(BSTRESS, Age = exp(Age))])
  simd.b[BSTRESS < 0, BSTRESS := 0]
  simd.b[Age < 18, Age := 18]
  setnames(simd.b, c("BTST", "BWAKE", "BMVPA", "BLPA", "BSB", "BSTRESS", "Age"))

  simd.w <- mvrnorm(n = n * k, mu = wm, Sigma = wcov)
  simd.w <- as.data.table(simd.w)

  inv.w <- ilrInv(simd.w[, .(V1, V2, V3, V4)], V = psi)
  inv.w <- clo(inv.w, total = 5) # 5 because 1 for each means no change
  simd.w <- cbind(inv.w, simd.w[, .(WSTRESS)])
  setnames(simd.w, c("WTST", "WWAKE", "WMVPA", "WLPA", "WSB", "WSTRESS"))

  simd.b[, ID := seq_len(n)]
  simd.w[, ID := rep(seq_len(n), each = k)]

  simd.all <- merge(simd.b, simd.w, by = "ID")
  simd.all[, TST := WTST * BTST]
  simd.all[, WAKE := WWAKE * BWAKE]
  simd.all[, MVPA := WMVPA * BMVPA]
  simd.all[, LPA := WLPA * BLPA]
  simd.all[, SB := WSB * BSB]
  simd.all[, STRESS := WSTRESS + BSTRESS]
  return(simd.all)
}


simmodel <- function(database, sbpbase) {
  
  psub <- possub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
  parts <- colnames(psub)
  
  cilr <- compilr(database, sbpbase, parts, total = 1440, idvar = "ID")
  
  model <- brmcoda(cilr,
                   depression ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
                     (1 + wilr2 | ID), cores = 4, chains = 4, iter = 2000, warmup = 1000,
                   backend = "cmdstanr")
  
  summodel <- summary(model$Model)
  ndt <- sum(subset(nuts_params(model$Model), Parameter == "divergent__")$Value)
  
  bsubm <- bsub(model, substitute = psub, minute = 30)
  wsubm <- wsub(model, substitute = psub, minute = 30)
  
  out <- list(
    CompILR = cilr,
    Result = summodel,
    BetweenResult = bsubm,
    WithinResult = wsubm,
    N = N,
    K = K,
    ndt = ndt
  )
}

# simulated data
obs <- data.table(K = c(3:28))
obs[, Kwt := dbeta((K - min(K))/(max(K) - min(K)),
                   1, 2)]
obs[, Kwt := Kwt/sum(Kwt)]

ppl <- data.table(N = seq(10, 1000, by = 2))
ppl[, Nwt := dbeta((N - min(N))/(max(N) - min(N)),
                   1, 2)]
ppl[, Nwt := Nwt/sum(Nwt)]

d <- expand.grid(
  K = obs$K,
  N = ppl$N
)
d <- merge(d, obs, by = "K")
d <- merge(d, ppl, by = "N")
d <- setDT(d)
d[, wt := Kwt*Nwt]


meanscovs <- readRDS("meanscovs.RDS")
