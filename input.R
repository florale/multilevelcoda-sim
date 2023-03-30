library(compositions)

# sbp --------------------
sbp <- matrix(c(
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
groundtruth <- data.table(
  b_Intercept  = 2.20,
  b_bilr1      = -0.20,
  b_bilr2      = -0.01,
  b_bilr3      = -0.02,
  b_bilr4      = 0.05,
  b_wilr1      = +0.15,
  b_wilr2      = 0.25,
  b_wilr3      = 0.01,
  b_wilr4      = -0.15,
  
  u0           = 1,
  u0_small     = sqrt(.5),
  u0_large     = sqrt(1.5),
  
  sigma        = 1,
  sigma_small  = sqrt(.5),
  sigma_large  = sqrt(2)
)

groundtruth4 <- data.table(
  b_Intercept  = 2.50,
  b_bilr1      = -0.30,
  b_bilr2      = -0.02,
  b_bilr3      = 0.01,
  b_wilr1      = +0.20,
  b_wilr2      = 0.00,
  b_wilr3      = -0.15,
  
  u0           = 1,
  u0_small     = sqrt(.5),
  u0_large     = sqrt(1.5),
  
  sigma        = 1,
  sigma_small  = sqrt(.5),
  sigma_large  = sqrt(2)
)

groundtruth3 <- data.table(
  b_Intercept  = 2.40,
  b_bilr1      = -0.30,
  b_bilr2      = -0.10,
  b_wilr1      = +0.25,
  b_wilr2      = -0.20,
  
  u0           = 1,
  u0_small     = sqrt(.5),
  u0_large     = sqrt(1.5),
  
  sigma        = 1,
  sigma_small  = sqrt(.5),
  sigma_large  = sqrt(2)
)

## conditions --------
# 4*4*5
cond <- as.data.table(
  expand.grid(N = c(30, 50, 360, 1200),
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

cond[, condition := NA]
cond[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
cond[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
cond[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
cond[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(2), "REbase_RESlarge",  condition)]
cond[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]

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
simmodel <- function(database, sbpbase, prefit = NULL) {
  psub <- basesub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
  parts <- colnames(psub)
  
  cilr <-
    compilr(database, sbpbase, parts, total = 1440, idvar = "ID")
  
  model <- list()
  # model --------
  if (isTRUE(is.null(prefit))) {
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
      delta = c(10, 20, 30, 60),
      level = c("between", "within"),
      type = "conditional"
    )
    
    model <- list(
      ModelSummary = summary(m$Model),
      Substitution = submodel,
      ndt = sum(subset( # number of divergent transitions
        nuts_params(m$Model), Parameter == "divergent__")$Value),
      brmsfit = m$Model
    )
    
  } else {
    dat <- cbind(cilr$data, cilr$BetweenILR, cilr$WithinILR)
    fit <- update(prefit, newdata = dat, backend = "cmdstanr")
    m <- structure(list(CompIlr = cilr,
                        Model = fit),
                   class = "brmcoda")
    
    submodel <- substitution(
      m,
      delta = c(10, 20, 30, 60),
      level = c("between", "within"),
      type = "conditional"
    )
    
    model <- list(
      ModelSummary = summary(m$Model),
      Substitution = submodel,
      ndt = sum(subset(
        nuts_params(m$Model), Parameter == "divergent__")$Value)
    )
  }
  
  list(
    CompILR = cilr,
    Result = model
  )
}
