source("modelsetup.R")

registerDoFuture()
plan(multisession, workers = 4L)

set.seed(1) # set different for each script

## sampledd <- d[sample(seq_len(.N), size = 5, replace = TRUE, prob = wt)]
## ggplot(sampledd, aes(x = N, y = K)) +
##   geom_density_2d_filled()

sampledd <- d[1:10] ## use the first 20 runs, for running on server, you might do 1:1000 in on model, 1001:2000 in a second, and so on

out1 <- vector("list", length = nrow(sampledd))

starttime <- proc.time()
for (i in seq_len(nrow(sampledd))) {
  N <- sampledd[i, N]
  K <- sampledd[i, K]
  mrint <- sampledd[i, mrint]
  sdrint <- sampledd[i, sdrint]

  simd <- with(meanscovs, rbind(
    simulateData(
      bm = BMeans, wm = WMeans,
      bcov = BCov, wcov = WCov,
      n = N, k = K, psi = psi)))

  # ILR ------------------------
  cilr <- compilr(
    data = simd[, .(TST, WAKE, MVPA, LPA, SB, ID)],
    sbp = meanscovs$sbp,
    parts = c("TST", "WAKE", "MVPA", "LPA", "SB"), idvar = "ID")

  tmp <- cbind(cilr$data, cilr$BetweenILR, cilr$WithinILR, 
               cilr$TotalILR)

  # random effects ---------------------------------------------------------------------------------
  # redat <- mvrnorm(n = length(unique(tmp$ID)),
  #                  mu = c(0, 0), diag(2) * (c(8, 1)^2))
  # redat <- data.table(ID = unique(tmp$ID),
  #                     rint = redat[, 1], rslope = redat[, 2])
  
  redat <- rnorm(n = length(unique(tmp$ID)), 
                 mean = mrint, sd = sdrint)
  redat <- data.table(ID = unique(tmp$ID),
                      rint = redat)
  
  tmp <- merge(tmp, redat, by = "ID")

  # outcome - simulated using ml regression  -------------------------------------------------------
  tmp[, sleepy := rnorm(
    n = nrow(simd),
    mean = (2.20 + rint) +
      (-0.20 * bilr1) + (-0.01 * bilr2) + (-0.02 * bilr3) + (0.05 * bilr4) +
      (+0.15 * wilr1) + (0.25 * wilr2) + (0.00 * wilr3) + (-0.15 * wilr4),
    sd = 0.8)]

  simd$sleepy <- tmp$sleepy

  if (i == 1) {
    out1[[i]] <- simmodel(simd, sbpbase = meanscovs$sbp)
  } else {
    out1[[i]] <- simmodel(simd, sbpbase = meanscovs$sbp, prefit = out1[[1]]$brmsfit)
  }
}
endtime <- proc.time()
endtime - starttime ## time to complete
saveRDS(out1, "out1.RDS", compress = "xz")


# check
cilr <- compilr(simd, sbp = sbp, 
                parts = c("TST", "WAKE", "MVPA", "LPA", "SB"), idvar = "ID")
simm <- brmcoda(cilr,
                sleepy ~ bilr1 + bilr2 + bilr3 + bilr4 +
                  wilr1 + wilr2 + wilr3 + wilr4 + (1 | ID), 
                cores = 8, seed = 123)
summary(simm$Model)


tilr <- ilr(acomp(simd[, .(TST, WAKE, MVPA, LPA, SB)]), V = psi)
bilr <- ilr(acomp(simd[, .(BTST, BWAKE, BMVPA, BLPA, BSB)]), V = psi)
wilr <- ilr(acomp(simd[, .(WTST, WWAKE, WMVPA, WLPA, WSB)]), V = psi)

colnames(tilr) <- c("ilr1", "ilr2", "ilr3", "ilr4")
colnames(bilr) <- c("bilr1", "bilr2", "bilr3", "bilr4")
colnames(wilr) <- c("wilr1", "wilr2", "wilr3", "wilr4")

simd <- cbind(
  tilr,
  bilr,
  wilr,
  simd[, .(sleepy, ID)])

simm <- brm(sleepy ~ bilr1 + bilr2 + bilr3 + bilr4 +
              wilr1 + wilr2 + wilr3 + wilr4 + (1 | ID), data = simd,
            cores = 8, seed = 123)
summary(simm)
