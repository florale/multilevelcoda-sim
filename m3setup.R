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

synd <- readRDS("syntheticpopulation.RDS")
synd <- setDT(synd)

sbp <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol=5, byrow=TRUE)

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

ppl <- data.table(N = c(10:1000))
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
