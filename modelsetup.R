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

# function for sim ilr

# simulateData <- function(bm, wm, bcov, wcov, n, k, psi) {
#   simd.b <- mvrnorm(n = n, mu = bm, Sigma = bcov)
#   simd.b <- as.data.table(simd.b)
# 
#   inv.b <- ilrInv(simd.b[, .(V1, V2, V3, V4)], V = psi)
#   simd.b <- as.data.table(inv.b)
#   setnames(simd.b, c("BTST", "BWAKE", "BMVPA", "BLPA", "BSB"))
# 
#   simd.w <- mvrnorm(n = n * k, mu = wm, Sigma = wcov)
#   simd.w <- as.data.table(simd.w)
# 
#   inv.w <- ilrInv(simd.w[, .(V1, V2, V3, V4)], V = psi)
#   inv.w <- clo(inv.w, total = 5) # 5 because 1 for each means no change
#   simd.w <- as.data.table(inv.w)
#   setnames(simd.w, c("WTST", "WWAKE", "WMVPA", "WLPA", "WSB"))
# 
#   simd.b[, ID := seq_len(n)]
#   simd.w[, ID := rep(seq_len(n), each = k)]
# 
#   simd.all <- merge(simd.b, simd.w, by = "ID")
#   simd.all[, TST := WTST * BTST]
#   simd.all[, WAKE := WWAKE * BWAKE]
#   simd.all[, MVPA := WMVPA * BMVPA]
#   simd.all[, LPA := WLPA * BLPA]
#   simd.all[, SB := WSB * BSB]
#   
#   simd.t <- simd.all[, .(TST, WAKE, MVPA, LPA, SB)]
#   simd.t <- clo(simd.t, total = 1440)
#   simd.t <- as.data.table(simd.t)
#   setnames(simd.t, c("TST", "WAKE", "MVPA", "LPA", "SB"))
#   
#   simd.t[, ID := rep(seq_len(n), each = k)]
#   
#   return(simd.t)
# }

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

simmodel <- function(database, sbpbase, prefit = NULL) {
  
  psub <- basesub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
  parts <- colnames(psub)
  
  cilr <- compilr(database, sbpbase, parts, total = 1440, idvar = "ID")

  # prior <- c(
  #   set_prior("normal(50, 5)", class = "Intercept"),
  #   set_prior("normal(0,  4)", class = "b"),
  #   set_prior("normal(0, 10)", class = "sigma"),
  #   set_prior("normal(0, 10)", class = "sd", coef = "Intercept", group = "ID"),
  #   set_prior("lkj(1)", class = "cor"))

  ## priordat <- data.table(x = seq(from = -100, to = 100), by = .01)
  ## ggplot(priordat, aes(x = x)) + ## fixed effects intercept
  ##   geom_line(aes(y = dnorm(x, mean = 50, sd = 5)), colour = "black") +
  ##   coord_cartesian(xlim = c(30, 70), expand = FALSE) + theme_minimal()
  ## ggplot(priordat, aes(x = x)) + ## fixed effects coefficients
  ##   geom_line(aes(y = dnorm(x, mean = 0, sd = 4)), colour = "black") +
  ##   coord_cartesian(xlim = c(-10, 10), expand = FALSE) + theme_minimal()
  ## ggplot(priordat, aes(x = x)) + ## random effect SDs & residual SD
  ##   geom_line(aes(y = dnorm(x, mean = 0, sd = 10)), colour = "black") +
  ##   geom_line(aes(y = dnorm(x, mean = 0, sd = 4)), colour = "blue") +
  ##   coord_cartesian(xlim = c(0, 10), expand = FALSE) + theme_minimal()

  if (isTRUE(is.null(prefit))) {
    model_se <- brmcoda(cilr,
                     sleepy_se ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
                       (1 | ID), cores = 4, chains = 4, iter = 3000, warmup = 500, backend = "cmdstanr")
    model_me <- brmcoda(cilr,
                     sleepy_me ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
                       (1 | ID), cores = 4, chains = 4, iter = 3000, warmup = 500, backend = "cmdstanr")
    model_le <- brmcoda(cilr,
                     sleepy_le ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
                       (1 | ID), cores = 4, chains = 4, iter = 3000, warmup = 500, backend = "cmdstanr")
  } else {
    model <- brmcoda(cilr,
                     sleepy ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
                       (1 | ID), cores = 4, chains = 4, iter = 3000, warmup = 500, backend = "cmdstanr", fit = prefit)
  }

  summodel <- summary(model$Model)
  ndt <- sum(subset(nuts_params(model$Model), Parameter == "divergent__")$Value)
  
  subm <- substitution(model, 
                       delta = 1:30,
                       level = c("between", "within"), 
                       type = "conditional")
  
  out <- list(
    CompILR = cilr,
    MainModel = summodel,
    SubstitutionModel = subm,
    N = N,
    K = K,
    ndt = ndt # number of divergent transitions
  )

  if (isTRUE(is.null(prefit))) {
    out$brmsfit <- model$Model
  }
  return(out)
}

## # simulated data
## obs <- data.table(K = c(3:28))
## obs[, Kwt := dbeta((K - min(K))/(max(K) - min(K)),
##                    1, 2)]
## obs[, Kwt := Kwt/sum(Kwt)]
## ppl <- data.table(N = seq(10, 1000, by = 2))
## ppl[, Nwt := dbeta((N - min(N))/(max(N) - min(N)),
##                    1, 2)]
## ppl[, Nwt := Nwt/sum(Nwt)]
## d <- expand.grid(
##   K = obs$K,
##   N = ppl$N
## )
## d <- merge(d, obs, by = "K")
## d <- merge(d, ppl, by = "N")
## d <- setDT(d)
## d[, wt := Kwt * Nwt]

## conditions (1000 runs each condition)
cond <- as.data.table(expand.grid(N = c(30, 50, 360, 1200),
                                  K = c(3, 5, 7, 14)))
cond <- cond[rep(seq_len(.N), 1000)]

meanscovs <- readRDS("meanscovs.RDS")
groundtruth <- readRDS("groundtruth.RDS")
