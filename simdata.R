#### Calculate Means & Covariance Matrix for multilevel compositional data simulation ####
library(data.table)
library(compositions)
library(multilevelTools)
library(MASS)

# sbp & psi
sbp <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol = 5, byrow = TRUE)
psi <- gsi.buildilrBase(t(sbp))

# read data ----------------------------------------------------------------------------------------
if (Sys.info()[["sysname"]] %in% "Windows") {
  loc.base <- "g:"
} else if (Sys.info()[["sysname"]] %in% "Darwin") {
  loc.base <- "/Volumes/GoogleDrive"
}

d <- as.data.table(readRDS("/Users/florale/Library/CloudStorage/GoogleDrive-flora.le@monash.edu/Shared drives/EMA_Studies/ema_studies.RDS"))

setnames(d, "TSTacti", "TST")
setnames(d, "MVPAPERCacti", "MVPA")
setnames(d, "LIGHTPERCacti", "LPA")
setnames(d, "SEDPERCacti", "SB")

d <- d[!is.na(TST) & !is.na(MVPA) & !is.na(SOLRAWacti)] ## exclude surveys without sleep/acti data

d[, TST := TST * 60]
d[, WAKE := SOLRAWacti + WASORAWacti]
d[, timeawake := (24 * 60) - TST - WAKE]
d[, SB   := (SB   / 100) * timeawake]
d[, LPA  := (LPA  / 100) * timeawake]
d[, MVPA := (MVPA / 100) * timeawake]
d[, totalhours := TST + WAKE + SB + LPA + MVPA]

d[, c("BMVPA", "WMVPA") := meanDeviations(MVPA), by = UID]
d[, c("BLPA", "WLPA") := meanDeviations(LPA), by = UID]
d[, c("BSB", "WSB") := meanDeviations(SB), by = UID]
d[, c("BWAKE", "WWAKE") := meanDeviations(WAKE), by = UID]
d[, c("BTST", "WTST") := meanDeviations(TST), by = UID]
d[, c("BSTRESS", "WSTRESS") := meanDeviations(STRESS), by = UID]

## make within variable ratio of total to between
d[, WMVPA := MVPA / BMVPA]
d[, WLPA := LPA / BLPA]
d[, WSB := SB / BSB]
d[, WWAKE := WAKE / BWAKE]
d[, WTST := TST / BTST]
d[, total := TST + WAKE + MVPA + LPA + SB]

## hist(d[!duplicated(UID)]$Age)
## table(d[!duplicated(UID)]$Female)
## between sim comp  -------------------------------------------------------------------------------
bd <- d[, .(BTST, BWAKE, BMVPA, BLPA, BSB)]

# sim ilr
bd_ilr <- ilr(acomp(bd[, .(BTST, BWAKE, BMVPA, BLPA, BSB)]), V = psi)
means.b <- colMeans(bd_ilr, na.rm = TRUE)
cov.b <- cov(bd_ilr, use = "complete.obs")

# # sim comp
# bd <- acomp(bd)
# (m_bd <- mean.acomp(bd))
# (v_bd <- var.acomp(bd))

## within sim comp  --------------------------------------------------------------------------------
wd <- d[, .(WTST, WWAKE, WMVPA, WLPA, WSB)]

# sim ilr
wd_ilr <- ilr(acomp(wd[, . (WTST, WWAKE, WMVPA, WLPA, WSB)]), V = psi)
means.w <- colMeans(wd_ilr, na.rm = TRUE)
cov.w <- cov(as.matrix(wd_ilr), use = "complete.obs")

# # sim comp
# td <- acomp(d[, .(TST, WAKE, MVPA, LPA, SB)])
# wd <- td - bd
# # wd <- acomp(wd)
# (m_wd <- mean.acomp(wd))
# (v_wd <- var.acomp(wd))

# save and load data --------
meanscovs <- list(
  BMeans = means.b,
  BCov = cov.b,
  WMeans = means.w,
  WCov = cov.w,
  compvars = c("TST", "WAKE", "MVPA", "LPA", "SB"),
  sbp = sbp,
  psi = psi)

# meanscovs <- list(
#   BMeans = m_bd,
#   BCov = v_bd,
#   WMeans = m_wd,
#   WCov = v_wd,
#   compvars = c("TST", "WAKE", "MVPA", "LPA", "SB"),
#   sbp = sbp,
#   psi = psi)

saveRDS(meanscovs, file = "meanscovs.RDS")

meanscovs <- readRDS("meanscovs.RDS")

## groundtruth 
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
saveRDS(groundtruth, file = "groundtruth.RDS")
# # check ------------------
# library(data.table)
# library(compositions)
# library(multilevelTools)
# library(MASS)
# 
# set.seed(1234)
# test <- with(meanscovs, rbind(
#   simulateData(
#     bm = BMeans, wm = WMeans,
#     bcov = BCov, wcov = WCov,
#     n = 2000, k = 50, psi = psi)))
#   
# plot(
#   quantile(d$Age, probs = seq(.01, .99, .01), na.rm = TRUE),
#   quantile(test$Age, probs = seq(.01, .99, .01)))
# abline(a = 0, b = 1)
# 
# plot(
#   quantile(d$TST, probs = seq(.01, .99, .01)),
#   quantile(test$TST, probs = seq(.01, .99, .01)))
# abline(a = 0, b = 1)
# 
# plot(
#   quantile(d$MVPA, probs = seq(.01, .99, .01)),
#   quantile(test$MVPA, probs = seq(.01, .99, .01)))
# abline(a = 0, b = 1)
# 
# plot(
#   quantile(d$WAKE, probs = seq(.01, .99, .01)),
#   quantile(test$WAKE, probs = seq(.01, .99, .01)))
# abline(a = 0, b = 1)
# 
# 
# # check compositional normality
# qqnorm(acomp(test[, .(BTST, BWAKE, BMVPA, BLPA, BSB)]))
# qqnorm(acomp(test[, .(WTST, WWAKE, WMVPA, WLPA, WSB)]))
# qqnorm(acomp(test[, .(TST, WAKE, MVPA, LPA, SB)]))
# 