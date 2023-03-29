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
      ndt = sum(subset(
        nuts_params(m$Model), Parameter == "divergent__"
      )$Value),
      # number of divergent transitions
      brmsfit = m$Model
    )
    
  } else {
    dat <- cbind(cilr$data, cilr$BetweenILR, cilr$WithinILR)
    fit <- update(prefit, newdata = dat, recompile = FALSE)
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
        nuts_params(m$Model), Parameter == "divergent__"
      )$Value)
    )
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
