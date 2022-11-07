source("m3setup.R")

registerDoFuture()
plan(multisession, workers = 4L)

set.seed(1) # set different for each script

sampledd <- d[sample(seq_len(.N), size = 5, replace = TRUE, prob = wt)]

## ggplot(sampledd, aes(x = N, y = K)) + 
##   geom_density_2d_filled()

out1 <- list()
system.time(
  for (i in seq_len(nrow(sampledd))) {
    N = sampledd[i]$N
    K = sampledd[i]$K

    synd <- with(meanscovs, rbind(
      cbind(simulateData(
        bm = BMeans.F, wm = WMeans.F,
        bcov = BCov.F, wcov = WCov.F,
        n = N / 2, k = K, psi = psi),
        Female = 1),
      cbind(simulateData(
        bm = BMeans.M, wm = WMeans.M,
        bcov = BCov.M, wcov = WCov.M,
        n = N / 2, k = K, psi = psi),
        Female = 0)))
    synd[, ID := paste0(ID, "_", Female)]

    # ILR ------------------------
    cilr <- compilr(
      data = synd[, .(TST, WAKE, MVPA, LPA, SB, ID)],
      sbp = meanscovs$sbp,
      parts = c("TST", "WAKE", "MVPA", "LPA", "SB"), idvar = "ID")

    tmp <- cbind(cilr$data, cilr$BetweenILR, cilr$WithinILR, 
                 cilr$TotalILR)

    # random effects -----------------------------------------------------------------------------------
    redat <- mvrnorm(n = length(unique(tmp$ID)),
                     mu = c(0, 0), diag(2) * (c(8, 1)^2))
    redat <- data.table(ID = unique(tmp$ID),
                        rint = redat[, 1], rslope = redat[, 2])

    tmp <- merge(tmp, redat, by = "ID")

    # outcome ------------------------------------------------------------------------------------------
    tmp[, depression := rnorm(
      n = nrow(synd),
      mean = (50 + rint) +
        (4 * bilr1) + (0 * bilr2) + (-4 * bilr3) + (0 * bilr4) +
        (+2 * wilr1) + ((-5 + rslope) * wilr2) + (0 * wilr3) + (-2 * wilr4),
      sd = 5)]

    synd$depression <- tmp$depression

    out1[[i]] <- simmodel(synd, sbpbase = meanscovs$sbp)
  })
saveRDS(out1, "out1.RDS")
