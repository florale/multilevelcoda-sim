## Simulated Dataset for multilevelcoda
library(MASS)
library(data.table)
library(multilevelTools)
library(extraoperators)
#library(tidyverse)
library(compositions)
library(multilevelcoda)
library(brms)
library(cmdstanr)
library(multilevelcoda)
library(lme4)
library(doFuture)
library(foreach)
library(doRNG)
library(parallel)

library(ggplot2)
library(ggsci)

# sbp
sbp <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol=5, byrow=TRUE)

k <- 200 ## number of days
N <- 20000 ## number of people
set.seed(1234)

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
d[, BSTRESS := mean(STRESS, na.rm = TRUE), by = UID]

hist(d[!duplicated(UID)]$Age)
table(d[!duplicated(UID)]$Female)

# between-person dataset for simulation
bd <- d[!duplicated(UID), .(UID, BTST, BWAKE, BMVPA, BLPA, BSB, BSTRESS, Age = log(Age), Female)]
summary(bd)

## between female  ---------------------------------------------------------------------------------
bd.f <- bd[Female == 1, .(BTST, BWAKE, BMVPA, BLPA, BSB, BSTRESS, Age)]

# composition
b <- bd.f[, .(BTST, BWAKE, BMVPA, BLPA, BSB)]
bcomp <- acomp(b)
psi <- gsi.buildilrBase(t(sbp))
bilr <- ilr(bcomp, V = psi)

bd.f <- cbind(bilr, bd.f[, .(BSTRESS, Age)])

# female simulated dataset
means.f <- colMeans(bd.f, na.rm = TRUE)
cov.f <- cov(bd.f, use = "complete.obs")
cov.f[cbind(c(1, 2), c(2, 1))] <- -.045
cov.f[cbind(c(3, 4), c(4, 3))] <- .07
all(eigen(cov.f)$values > 0) ## check matrix still positive definite using eigen values
round(cov2cor(cov.f), 2)
simd.f <- mvrnorm(n = N / 2, mu = means.f, Sigma = cov.f, empirical = TRUE)
simd.f <- as.data.table(simd.f)
## plot(simd.f)

invb <- ilrInv(simd.f[, .(V1, V2, V3, V4)], V = psi)
invb <- clo(invb, total = 1440)

simd.f <- cbind(invb, simd.f[, .(BSTRESS, Age = exp(Age))])
summary(simd.f)
simd.f[BSTRESS < 0, BSTRESS := 0]
simd.f[Age < 18, Age := 18]

simd.f$Female <- 1
simd.f$ID <- seq_len(nrow(simd.f))
names(simd.f) <- c("BTST", "BWAKE", "BMVPA", "BLPA", "BSB", "BSTRESS", "Age", "Female", "ID")

## between male  -----------------------------------------------------------------------------------
bd.m <- bd[Female == 0, .(BTST, BWAKE, BMVPA, BLPA, BSB, BSTRESS, Age)]

b <- bd.m[, . (BTST, BWAKE, BMVPA, BLPA, BSB)]
bcomp <- acomp(b)
psi <- gsi.buildilrBase(t(sbp))
bilr <- ilr(bcomp, V=psi)

bd.m <- cbind(bilr, bd.m[, .(BSTRESS, Age)])

means.m <- colMeans(bd.m, na.rm = TRUE)
cov.m <- cov(bd.m, use = "complete.obs")
cov.m[cbind(c(1, 2), c(2, 1))] <- -.045
cov.m[cbind(c(3, 4), c(4, 3))] <- .09
all(eigen(cov.m)$values > 0) ## check matrix still positive definite using eigen values
round(cov2cor(cov.m), 2)

simd.m <- mvrnorm(n = N / 2, mu = means.m, Sigma = cov.m)
simd.m <- as.data.table(simd.m)
## plot(simd.m)

invb <- ilrInv(simd.m[, .(V1, V2, V3, V4)], V = psi)
invb <- clo(invb, total = 1440)

simd.m <- cbind(invb, simd.m[, .(BSTRESS, Age = exp(Age))])
summary(simd.m)
simd.m[BSTRESS < 0, BSTRESS := 0]
simd.m[Age < 18, Age := 18]

summary(simd.m)

simd.m$Female <- 0
simd.m$ID <- seq_len(nrow(simd.m))
names(simd.m) <- c("BTST", "BWAKE", "BMVPA", "BLPA", "BSB", "BSTRESS", "Age", "Female", "ID")

## within female  ----------------------------------------------------------------------------------
wd.f <- d[Female == 1, .(TST, WAKE, MVPA, LPA, SB, BTST, BWAKE, BMVPA, BLPA, BSB, STRESS, BSTRESS)]
wd.f[, WSTRESS := STRESS - BSTRESS]
wd.f[, WTST := TST/BTST]
wd.f[, WWAKE := WAKE/BWAKE]
wd.f[, WMVPA := MVPA/BMVPA]
wd.f[, WLPA := LPA/BLPA]
wd.f[, WSB := SB/BSB]

# female within composition
w <- wd.f[, . (WTST, WWAKE, WMVPA, WLPA, WSB)]
wcomp <- acomp(w)
psi <- gsi.buildilrBase(t(sbp))
wilr <- ilr(wcomp, V = psi)

wd.f <- cbind(wilr, wd.f[, .(WSTRESS)])
## plot(simdw.f)

simdw.f <- mvrnorm(n = N / 2 * k,
                    mu = colMeans(wd.f, na.rm = TRUE),
                    Sigma = cov(wd.f, use = "complete.obs"))
simdw.f <- as.data.table(simdw.f)

invb <- ilrInv(simdw.f[, .(V1, V2, V3, V4)], V = psi)
invb <- clo(invb, total = 5) # 5 because 1 for each means no change
simdw.f <- cbind(invb, simdw.f[, .(WSTRESS)])
names(simdw.f) <- c("WTST", "WWAKE", "WMVPA", "WLPA", "WSB", "WSTRESS")

simdw.f$ID <- rep(seq_len(N / 2), each = k)
simd.fall <- merge(simdw.f, simd.f, by = "ID")

## within male -------------------------------------------------------------------------------------
wd.m <- d[Female == 0, .(TST, WAKE, MVPA, LPA, SB, BTST, BWAKE, BMVPA, BLPA, BSB, STRESS, BSTRESS)]

wd.m[, WSTRESS := STRESS - BSTRESS]
wd.m[, WTST := TST/BTST]
wd.m[, WWAKE := WAKE/BWAKE]
wd.m[, WMVPA := MVPA/BMVPA]
wd.m[, WLPA := LPA/BLPA]
wd.m[, WSB := SB/BSB]

w <- wd.m[, . (WTST, WWAKE, WMVPA, WLPA, WSB)]
wcomp <- acomp(w)
psi <- gsi.buildilrBase(t(sbp))
wilr <- ilr(wcomp, V=psi)

wd.m <- cbind(wilr, wd.m[, .(WSTRESS)])
## plot(wd.m)

simdw.m <- mvrnorm(n = N / 2 * k, 
                   mu = colMeans(wd.m, na.rm = TRUE),
                   Sigma = cov(wd.m, use = "complete.obs"))
simdw.m <- as.data.table(simdw.m)

invb <- ilrInv(simdw.m[, .(V1, V2, V3, V4)], V = psi)
invb <- clo(invb, total = 5)
simdw.m <- cbind(invb, simdw.m[, .(WSTRESS)])
names(simdw.m) <- c("WTST", "WWAKE", "WMVPA", "WLPA", "WSB", "WSTRESS")

simdw.m$ID <- rep(seq_len(N / 2), each = k)
simd.mall <- merge(simdw.m, simd.m, by = "ID")
simd.mall$ID <- simd.mall$ID + max(simd.fall$ID)

# full dataset ------------------------------------------------------------------------------------\
simd.all <- rbind(simd.mall, simd.fall)

simd.all[, TST := WTST*BTST]
simd.all[, WAKE := WWAKE*BWAKE]
simd.all[, MVPA := WMVPA*BMVPA]
simd.all[, LPA := WLPA*BLPA]
simd.all[, SB := WSB*BSB]
simd.all[, STRESS := WSTRESS + BSTRESS]

t <- simd.all[, .(TST, WAKE, MVPA, LPA, SB)]
t <- clo(t, total = 1440)

simd.all <- cbind(simd.all[, .(ID, Age, Female, STRESS)], t)
simd.all[STRESS < 0, STRESS := 0][STRESS > 10, STRESS := 10]

summary(simd.all)
synd <- simd.all

# ILR ------------------------
cilr <- compilr(data = synd[, .(TST, WAKE, MVPA, LPA, SB, ID)],
                sbp = sbp, parts = c("TST", "WAKE", "MVPA", "LPA", "SB"), idvar = "ID")

tmp <- cbind(cilr$data, cilr$BetweenILR, cilr$WithinILR, 
             cilr$TotalILR)

# random effects -----------------------------------------------------------------------------------
redat <- mvrnorm(n = length(unique(tmp$ID)),
                 mu = c(0, 0), diag(2) * (c(8, 1)^2),
                 empirical = TRUE)
redat <- data.table(ID = unique(tmp$ID),
                    rint = redat[, 1], rslope = redat[, 2])

tmp <- merge(tmp, redat, by = "ID")

## add some checks on correlations
cormat <- cor(tmp[, .(bilr1, bilr2, bilr3, bilr4, wilr1, wilr2, wilr3, wilr4, rint, rslope)])

## view correlations
round(cormat, 2)

diag(cormat) <- 0 ## set diagonal to 0 to ignore these

## expect all correlations to have absolute values < .90
all(abs(cormat) < .9)

## expect all correlations with random effects to have very small correlations < .05
all(abs(cormat[, c("rint", "rslope")]) < .05)

# outcome ------------------------------------------------------------------------------------------

## tmp[, depression := rnorm(n = nrow(synd),
##                           mean = (50 + rint) +
##                             (0 * bilr1) + (0 * bilr2) + (0 * bilr3) + (0 * bilr4) +
##                             (+0.5 * wilr1) + ((-1 + rslope) * wilr2) + (-0.5 * wilr3) + (-1 * wilr4),
##                           sd = 5)]

tmp[, depression := rnorm(n = nrow(synd),
                          mean = (50 + rint) +
                            (4 * bilr1) + (0 * bilr2) + (-4 * bilr3) + (0 * bilr4) +                            
                            (+2 * wilr1) + ((-5 + rslope) * wilr2) + (0 * wilr3) + (-2 * wilr4),
                          sd = 5)]

## tmp[, depressionnore := rnorm(n = nrow(synd),
##                               mean = (50 + rint) +
##                                 (-0.5 * bilr1) + (1 * bilr2) + (0.5 * bilr3) + (1 * bilr4) +
##                                 (+0.5 * wilr1) + ((-1) * wilr2) + (-0.5 * wilr3) + (-1 * wilr4),
##                               sd = 5)]

synd$depression <- tmp$depression

# saveRDS(synd, file = "syntheticpopulation.RDS")
# synd <- readRDS("syntheticpopulation.RDS")
