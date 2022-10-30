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

simmodel <- function(database, sbpbase, N, K) {
  
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

obs <- data.table(K = 3:28)
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
d <- as.data.table(d)
d[, wt := Kwt*Nwt]

set.seed(1) 
sampledd <- d[sample(seq_len(.N), size = 10, replace = TRUE, prob = wt)]

# plan(cluster, workers = availableWorkers())

registerDoFuture()
plan(multisession, workers = 5L)
# options(doFuture.debug = TRUE)

system.time(testout <- foreach (i = seq_len(nrow(sampledd)),
                                .combine = c) %dorng% {
                                  
                                  N = sampledd[i]$N
                                  K = sampledd[i]$K
                                  
                                  useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
                                  dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), K, replace = FALSE)], by = ID]
                                  
                                  list(simmodel(dat, sbp, N, K))
                            }) 

saveRDS(testout, "testout.RDS")
sessionInfo()
testout <- readRDS("testout.RDS")
