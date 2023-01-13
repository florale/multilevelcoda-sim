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

d <- as.data.table(readRDS(file.path(loc.base, "Shared drives/EMA_Studies/ema_studies.RDS")))

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

## hist(d[!duplicated(UID)]$Age)
## table(d[!duplicated(UID)]$Female)

# between-person dataset for simulation
bd <- d[!duplicated(UID), .(BTST, BWAKE, BMVPA, BLPA, BSB, BSTRESS, Age = log(Age), Female)]
summary(bd)

## between female  ---------------------------------------------------------------------------------
bd.f <- bd[Female == 1, .(BTST, BWAKE, BMVPA, BLPA, BSB, BSTRESS, Age)]

bd.f <- cbind(  ## convert to composition
  ilr(acomp(bd.f[, .(BTST, BWAKE, BMVPA, BLPA, BSB)]), V = psi),
  bd.f[, .(BSTRESS, Age)])

# female simulated dataset
means.f <- colMeans(bd.f, na.rm = TRUE)
cov.f <- cov(bd.f, use = "complete.obs")
cov.f[cbind(c(1, 2), c(2, 1))] <- -.045
cov.f[cbind(c(3, 4), c(4, 3))] <- .07
all(eigen(cov.f)$values > 0) ## check matrix still positive definite using eigen values
round(cov2cor(cov.f), 2)

## between male  -----------------------------------------------------------------------------------
bd.m <- bd[Female == 0, .(BTST, BWAKE, BMVPA, BLPA, BSB, BSTRESS, Age)]

bd.m <- cbind(  ## convert to composition
  ilr(acomp(bd.m[, . (BTST, BWAKE, BMVPA, BLPA, BSB)]), V = psi),
  bd.m[, .(BSTRESS, Age)])

means.m <- colMeans(bd.m, na.rm = TRUE)
cov.m <- cov(bd.m, use = "complete.obs")
cov.m[cbind(c(1, 2), c(2, 1))] <- -.045
cov.m[cbind(c(3, 4), c(4, 3))] <- .09
all(eigen(cov.m)$values > 0) ## check matrix still positive definite using eigen values
round(cov2cor(cov.m), 2)

## within female  ----------------------------------------------------------------------------------
wd.f <- d[Female == 1, .(WTST, WWAKE, WMVPA, WLPA, WSB, WSTRESS)]

wd.f <- cbind( ## convert to composition
  ilr(acomp(wd.f[, . (WTST, WWAKE, WMVPA, WLPA, WSB)]), V = psi),
  wd.f[, .(WSTRESS)])

means.wf <- colMeans(wd.f, na.rm = TRUE)
cov.wf <- cov(wd.f, use = "complete.obs")

## within male -------------------------------------------------------------------------------------
wd.m <- d[Female == 0, .(WTST, WWAKE, WMVPA, WLPA, WSB, WSTRESS)]

wd.m <- cbind( ## convert to composition
  ilr(acomp(wd.m[, . (WTST, WWAKE, WMVPA, WLPA, WSB)]), V = psi),
  wd.m[, .(WSTRESS)])

means.wm <- colMeans(wd.m, na.rm = TRUE)
cov.wm <- cov(wd.m, use = "complete.obs")

meanscovs <- list(
  BMeans.F = means.f,
  BCov.F = cov.f,
  BMeans.M = means.m,
  BCov.M = cov.m,
  WMeans.F = means.wf,
  WCov.F = cov.wf,
  WMeans.M = means.wm,
  WCov.M = cov.wm,
  compvars = c("TST", "WAKE", "MVPA", "LPA", "SB"),
  sbp = sbp,
  psi = psi)

# save and load data --------
saveRDS(meanscovs, file = "meanscovs.RDS")

meanscovs <- readRDS("meanscovs.RDS")

# check ------------------
library(data.table)
library(compositions)
library(multilevelTools)
library(MASS)

set.seed(1234)
test <- with(meanscovs, rbind(
  cbind(simulateData(
    bm = BMeans.F, wm = WMeans.F,
    bcov = BCov.F, wcov = WCov.F,
    n = 2000, k = 50, psi = psi),
    Female = 1),
  cbind(simulateData(
    bm = BMeans.M, wm = WMeans.M,
    bcov = BCov.M, wcov = WCov.M,
    n = 2000, k = 50, psi = psi),
    Female = 0)))
test[, ID := paste0(ID,"_", Female)]
  
plot(
  quantile(d$Age, probs = seq(.01, .99, .01), na.rm = TRUE),
  quantile(test$Age, probs = seq(.01, .99, .01)))
abline(a = 0, b = 1)

plot(
  quantile(d$TST, probs = seq(.01, .99, .01)),
  quantile(test$TST, probs = seq(.01, .99, .01)))
abline(a = 0, b = 1)

plot(
  quantile(d$MVPA, probs = seq(.01, .99, .01)),
  quantile(test$MVPA, probs = seq(.01, .99, .01)))
abline(a = 0, b = 1)

plot(
  quantile(d$WAKE, probs = seq(.01, .99, .01)),
  quantile(test$WAKE, probs = seq(.01, .99, .01)))
abline(a = 0, b = 1)


# check compositional normality
qqnorm(acomp(test[, .(BTST, BWAKE, BMVPA, BLPA, BSB)]))
qqnorm(acomp(test[, .(WTST, WWAKE, WMVPA, WLPA, WSB)]))
qqnorm(acomp(test[, .(TST, WAKE, MVPA, LPA, SB)]))
