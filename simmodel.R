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

## conditions (1000 runs each condition) --------
# 4*4*5
cond <- as.data.table(expand.grid(N = c(30, 50, 360, 1200),
                                  K = c(3, 5, 7, 14),
                                  rint_sd = c(1, sqrt(.5), sqrt(1.5)),
                                  res_sd = c(1, sqrt(.5), sqrt(2)),
                                  run = 1:1000))
cond <- cond[
    (rint_sd == 1 & res_sd == 1) |
    (rint_sd == sqrt(.5) & res_sd == sqrt(1.5)) |
    (rint_sd == sqrt(1.5) & res_sd == sqrt(.5)) |
    (rint_sd == 1 & res_sd == sqrt(2)) |
    (rint_sd == 1 & res_sd == sqrt(.5))] 

sampled_cond <- cond[1:2]

cond <- cond[
  (rint_sd == 1 & res_sd == 1) |
    (rint_sd == sqrt(.5) & res_sd == sqrt(1.5)) |
    (rint_sd == sqrt(1.5) & res_sd == sqrt(.5)) |
    (rint_sd == 1 & res_sd == sqrt(2)) |
    (rint_sd == 1 & res_sd == sqrt(.5))]

cond[, cond := 1:.N, .(run)]

cond[, condition := NA]
cond[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
cond[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
cond[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
cond[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(2), "REbase_RESlarge",  condition)]
cond[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]

## functions ---------
simulateData <- function(bm, wm, bcov, wcov, n, k, psi) {
  simd.b <- mvrnorm(n = n, mu = bm, Sigma = bcov)
  simd.b <- as.data.table(simd.b)

  inv.b <- ilrInv(simd.b[, .(V1, V2, V3, V4)], V = psi)
  simd.b <- as.data.table(inv.b)
  setnames(simd.b, c("BTST", "BWAKE", "BMVPA", "BLPA", "BSB"))

  simd.w <- mvrnorm(n = n * k, mu = wm, Sigma = wcov)
  simd.w <- as.data.table(simd.w)

  inv.w <- ilrInv(simd.w[, .(V1, V2, V3, V4)], V = psi)
  inv.w <- clo(inv.w, total = 5) # 5 because 1 for each means no change
  simd.w <- as.data.table(inv.w)
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
  
  model <- list()
  # model --------
    if (isTRUE(is.null(prefit))) {
      m <- brmcoda(cilr, sleepy ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 + (1 | ID),
                   cores = 4, chains = 4, iter = 3000, warmup = 500, backend = "cmdstanr")
      
      submodel <- substitution(m, delta = c(10, 20, 30, 60),
                               level = c("between", "within"), 
                               type = "conditional")
      
      model <- list(
        ModelSummary = summary(m$Model),
        Substitution = submodel,
        ndt = sum(subset(nuts_params(m$Model), Parameter == "divergent__")$Value), # number of divergent transitions
        brmsfit = m$Model)
      
      } else {
        dat <- cbind(cilr$data, cilr$BetweenILR, cilr$WithinILR)
        fit <- update(prefit, newdata = dat, recompile = FALSE)
        m <- structure(
          list(CompIlr = cilr,
               Model = fit),
          class = "brmcoda")
        
        submodel <- substitution(m, delta = c(10, 20, 30, 60),
                                 level = c("between", "within"),
                                 type = "conditional")
        
        model <- list(
          ModelSummary = summary(m$Model),
          Substitution = submodel,
          ndt = sum(subset(nuts_params(m$Model), Parameter == "divergent__")$Value))
  }

  out <- list(
    CompILR = cilr,
    Result = model,
    N = N,
    K = K,
    rint_sd = rint_sd,
    res_sd = res_sd,
    run = run
  )
  return(out)
}

## model -------------------
set.seed(1) # set different for each script
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
  tmp[, sleepy :=  rnorm(n = nrow(simd),
                         mean = groundtruth$b0  + rint +
                           (groundtruth$b_bilr1 * bilr1) + 
                           (groundtruth$b_bilr2 * bilr2) + 
                           (groundtruth$b_bilr3 * bilr3) + 
                           (groundtruth$b_bilr4 * bilr4) +
                           (groundtruth$b_wilr1 * wilr1) + 
                           (groundtruth$b_wilr2 * wilr2) + 
                           (groundtruth$b_wilr3 * wilr3) + 
                           (groundtruth$b_wilr4 * wilr4),
                         sd = res_sd)]

  simd$sleepy <- tmp$sleepy

  outcome <- c(grep("sleepy", names(simd), value = T))
  
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
