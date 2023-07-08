library(compositions)

# sbp --------------------
sbp5 <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)

sbp4 <- matrix(c(
  1, -1, -1,-1,
  0, 1, -1, -1,
  0, 0, 1, -1), ncol = 4, byrow = TRUE)

sbp3 <- matrix(c(
  1, -1,-1, 
  0, 1, -1),ncol = 3, byrow = TRUE)

## groundtruth----------------------
groundtruth5 <- data.table(
  b_Intercept  = 2.30,
  b_bilr1      = 0.15,
  b_bilr2      = -0.01,
  b_bilr3      = 0.15,
  b_bilr4      = 0.05,
  b_wilr1      = -0.60,
  b_wilr2      = -0.45,
  b_wilr3      = -0.30,
  b_wilr4      = -0.20,
  
  u0           = 1,
  u0_small     = sqrt(.5),
  u0_large     = sqrt(1.5),
  
  sigma        = 1,
  sigma_small  = sqrt(.5),
  sigma_large  = sqrt(1.5)
)

groundtruth4 <- data.table(
  b_Intercept  = 2.10,
  b_bilr1      = 0.15,
  b_bilr2      = 0.15,
  b_bilr3      = 0.02,
  b_wilr1      = -0.75,
  b_wilr2      = -0.30,
  b_wilr3      = -0.20,
  
  u0           = 1,
  u0_small     = sqrt(.5),
  u0_large     = sqrt(1.5),
  
  sigma        = 1,
  sigma_small  = sqrt(.5),
  sigma_large  = sqrt(1.5)
)

groundtruth3 <- data.table(
  b_Intercept  = 2.10,
  b_bilr1      = 0.15,
  b_bilr2      = 0.10,
  b_wilr1      = -0.80,
  b_wilr2      = -0.25,
  
  u0           = 1,
  u0_small     = sqrt(.5),
  u0_large     = sqrt(1.5),
  
  sigma        = 1,
  sigma_small  = sqrt(.5),
  sigma_large  = sqrt(1.5)
)

## conditions --------
cond <- 
  expand.grid(N = c(30, 50, 360, 1200),
              K = c(3, 5, 7, 14),
              rint_sd = c(1, sqrt(.5), sqrt(1.5)),
              res_sd = c(1, sqrt(.5), sqrt(1.5)),
              n_parts = c(3, 4, 5),
              run = 1:2500)
cond <- as.data.table(cond)
cond <- cond[
  (rint_sd == 1 & res_sd == 1) |
    (rint_sd == sqrt(.5) & res_sd == sqrt(1.5)) |
    (rint_sd == sqrt(1.5) & res_sd == sqrt(.5)) |
    (rint_sd == 1 & res_sd == sqrt(1.5)) |
    (rint_sd == 1 & res_sd == sqrt(.5))] 

cond[, condition := NA]
cond[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
cond[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
cond[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
cond[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(1.5), "REbase_RESlarge",  condition)]
cond[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]

cond[, sbp := NA]
cond[, sbp := ifelse(n_parts == 3, "sbp3", sbp)]
cond[, sbp := ifelse(n_parts == 4, "sbp4", sbp)]
cond[, sbp := ifelse(n_parts == 5, "sbp5", sbp)]

cond[, prefit := NA]
cond[, prefit := ifelse(n_parts == 3, "prefit3", prefit)]
cond[, prefit := ifelse(n_parts == 4, "prefit4", prefit)]
cond[, prefit := ifelse(n_parts == 5, "prefit5", prefit)]

cond[, parts := NA]
cond[, parts := ifelse(n_parts == 3, "Sleep PA SB", parts)]
cond[, parts := ifelse(n_parts == 4, "Sleep MVPA LPA SB", parts)]
cond[, parts := ifelse(n_parts == 5, "TST WAKE MVPA LPA SB", parts)]

cond[, groundtruth := NA]
cond[, groundtruth := ifelse(n_parts == 3, "groundtruth3", groundtruth)]
cond[, groundtruth := ifelse(n_parts == 4, "groundtruth4", groundtruth)]
cond[, groundtruth := ifelse(n_parts == 5, "groundtruth5", groundtruth)]

## functions ---------
## SIM DATA USING ILR
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

## SIM DATA USING RNORM.ACOMP
simulateData.acomp <- function(bm, wm, bcov, wcov, n, k, psi) {
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

## SIM MODEL
simmodel <- function(database, parts, sbpbase, prefit = prefit) {
  model <- list()
  psub <- basesub(parts)
  cilr <-
    compilr(database, sbpbase, parts, total = 1440, idvar = "ID")
  
  # model --------    
  dat <- cbind(cilr$data, cilr$BetweenILR, cilr$WithinILR)
  fit <- update(prefit, 
                newdata = dat, 
                cores = 4,
                backend = "cmdstanr")
  
  m <- structure(list(CompILR = cilr,
                      Model = fit),
                 class = "brmcoda")
  
  submodel <- substitution(
    m,
    delta = c(30),
    level = c("between", "within"),
    ref = "grandmean"
  )
  
  model <- list(
    ModelSummary = summary(m$Model),
    Substitution = submodel,
    ndt = sum(subset(
      nuts_params(m$Model), Parameter == "divergent__")$Value))
  
  list(
    CompILR = cilr,
    Result = model
  )
}