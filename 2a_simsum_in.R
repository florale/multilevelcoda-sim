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

library(rsimsum) # https://cran.r-project.org/web/packages/rsimsum/vignettes/A-introduction.html
library(heatmaply)
library(ggthemes)
library(bayesplot)

library(hrbrthemes)
library(wesanderson)
library(ggplot2)
library(ggsci)
library(ggpubr)

data.table::setDTthreads(10)

# brmcoda out --------------------------
simsum_brmcoda_1 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_1.RDS")
simsum_brmcoda_2 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_2.RDS")
simsum_brmcoda_3 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_3.RDS")
simsum_brmcoda_4 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_4.RDS")
simsum_brmcoda_5 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_5.RDS")
simsum_brmcoda_6 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_6.RDS")
simsum_brmcoda_7 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_7.RDS")
simsum_brmcoda_8 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_8.RDS")
simsum_brmcoda_9 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_9.RDS")
simsum_brmcoda_10 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_10.RDS")
simsum_brmcoda_11 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_11.RDS")
simsum_brmcoda_12 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_12.RDS")
simsum_brmcoda_13 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_13.RDS")
simsum_brmcoda_14 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_14.RDS")
simsum_brmcoda_15 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_15.RDS")
simsum_brmcoda_16 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_16.RDS")
simsum_brmcoda_17 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_17.RDS")
simsum_brmcoda_18 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_18.RDS")
simsum_brmcoda_19 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_19.RDS")
simsum_brmcoda_20 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_20.RDS")
simsum_brmcoda_21 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_21.RDS")
simsum_brmcoda_22 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_22.RDS")
simsum_brmcoda_23 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_23.RDS")
simsum_brmcoda_24 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda_24.RDS")

## separate by parts and clean -----------------
simsum_brmcoda_d3 <- list()
simsum_brmcoda_d4 <- list()
simsum_brmcoda_d5 <- list()

for (j in c(1:24)) {
  simsum_brmcoda_d3[[j]] <- get(paste0("simsum_brmcoda_", j))[["out3"]]
  simsum_brmcoda_d4[[j]] <- get(paste0("simsum_brmcoda_", j))[["out4"]]
  simsum_brmcoda_d5[[j]] <- get(paste0("simsum_brmcoda_", j))[["out5"]]
}
simsum_brmcoda <- list(simsum_brmcoda_d3,
                       simsum_brmcoda_d4,
                       simsum_brmcoda_d5)

# add sim conditions
simsum_brmcoda <- lapply(simsum_brmcoda, function(simsum_brmcoda_d) {
  simsum_brmcoda_d <- as.data.table(do.call(rbind, simsum_brmcoda_d))
  
  simsum_brmcoda_d[, N := factor(N, levels = c("30", "50", "360", "1200"))]
  simsum_brmcoda_d[, K := factor(K, levels = c("3", "5", "7", "14"))]
  simsum_brmcoda_d[, D := factor(n_parts, levels = c("3", "4", "5"))]
  simsum_brmcoda_d[, n_parts := NULL]
  simsum_brmcoda_d[, I := K]
  simsum_brmcoda_d[, J := N]
  
  simsum_brmcoda_d[, condition := NA]
  simsum_brmcoda_d[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
  simsum_brmcoda_d[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
  simsum_brmcoda_d[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
  simsum_brmcoda_d[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(1.5), "REbase_RESlarge",  condition)]
  simsum_brmcoda_d[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]
  simsum_brmcoda_d[, condition := factor(condition, levels = c(
    "base",
    "REsmall_RESlarge",
    "RElarge_RESsmall",
    "REbase_RESlarge",
    "REbase_RESsmall"
  ))]
  
  simsum_brmcoda_d[, sigma_condition := NA]
  simsum_brmcoda_d[, sigma_condition := ifelse(res_sd == 1, "base",  sigma_condition)]
  simsum_brmcoda_d[, sigma_condition := ifelse(res_sd == sqrt(1.5), "large",  sigma_condition)]
  simsum_brmcoda_d[, sigma_condition := ifelse(res_sd == sqrt(.5), "small",  sigma_condition)]
  simsum_brmcoda_d[, sigma_condition := factor(sigma_condition, levels = c(
    "base",
    "small",
    "large"
  ))]
  
  simsum_brmcoda_d[, u0_condition := NA]
  simsum_brmcoda_d[, u0_condition := ifelse(rint_sd == 1, "base",  u0_condition)]
  simsum_brmcoda_d[, u0_condition := ifelse(rint_sd == sqrt(1.5), "large",  u0_condition)]
  simsum_brmcoda_d[, u0_condition := ifelse(rint_sd == sqrt(.5), "small",  u0_condition)]
  simsum_brmcoda_d[, u0_condition := factor(u0_condition, levels = c(
    "base",
    "small",
    "large"
  ))]
  
  simsum_brmcoda_d[, cond := paste0("J: ", J, ", ", "I: ", I, ", ", "sigma: ", condition)]
  
})
names(simsum_brmcoda) <- c("simsum_brmcoda_d3",
                           "simsum_brmcoda_d4",
                           "simsum_brmcoda_d5")

rm(simsum_brmcoda_1, simsum_brmcoda_2, simsum_brmcoda_3, simsum_brmcoda_4, 
   simsum_brmcoda_5, simsum_brmcoda_6, simsum_brmcoda_7, simsum_brmcoda_8,
   simsum_brmcoda_9, simsum_brmcoda_10, simsum_brmcoda_11, simsum_brmcoda_12,
   simsum_brmcoda_13, simsum_brmcoda_14, simsum_brmcoda_15, simsum_brmcoda_16,
   simsum_brmcoda_17, simsum_brmcoda_18, simsum_brmcoda_19, simsum_brmcoda_20,
   simsum_brmcoda_21, simsum_brmcoda_22, simsum_brmcoda_23, simsum_brmcoda_24)

# substitution out ----------------------------------
# simsum_sub <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_.RDS")
simsum_sub_1 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_1.RDS")
simsum_sub_2 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_2.RDS")
simsum_sub_3 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_3.RDS")
simsum_sub_4 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_4.RDS")
simsum_sub_5 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_5.RDS")
simsum_sub_6 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_6.RDS")
simsum_sub_7 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_7.RDS")
simsum_sub_8 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_8.RDS")
simsum_sub_9 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_9.RDS")
simsum_sub_10 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_10.RDS")
simsum_sub_11 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_11.RDS")
simsum_sub_12 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_12.RDS")
simsum_sub_13 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_13.RDS")
simsum_sub_14 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_14.RDS")
simsum_sub_15 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_15.RDS")
simsum_sub_16 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_16.RDS")
simsum_sub_17 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_17.RDS")
simsum_sub_18 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_18.RDS")
simsum_sub_19 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_19.RDS")
simsum_sub_20 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_20.RDS")
simsum_sub_21 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_21.RDS")
simsum_sub_22 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_22.RDS")
simsum_sub_23 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_23.RDS")
simsum_sub_24 <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub_24.RDS")

## separate by parts and clean -----------------
simsum_sub_d3 <- list()
simsum_sub_d4 <- list()
simsum_sub_d5 <- list()

for (j in c(1:24)) {
  simsum_sub_d3[[j]] <- get(paste0("simsum_sub_", j))[[1]]
  simsum_sub_d4[[j]] <- get(paste0("simsum_sub_", j))[[2]]
  simsum_sub_d5[[j]] <- get(paste0("simsum_sub_", j))[[3]]
}
simsum_sub <- list(simsum_sub_d3,
                   simsum_sub_d4,
                   simsum_sub_d5)

# add sim conditions
simsum_sub <- lapply(simsum_sub, function(simsum_sub_d) {
  simsum_sub_d <- as.data.table(do.call(rbind, simsum_sub_d))
  
  simsum_sub_d[, N := factor(N, levels = c("30", "50", "360", "1200"))]
  simsum_sub_d[, K := factor(K, levels = c("3", "5", "7", "14"))]
  simsum_sub_d[, D := factor(n_parts, levels = c("3", "4", "5"))]
  simsum_sub_d[, Level := factor(Level, levels = c("between", "within"))]
  simsum_sub_d[, n_parts := NULL]
  simsum_sub_d[, I := K]
  simsum_sub_d[, J := N]
  
  simsum_sub_d[, condition := NA]
  simsum_sub_d[, condition := ifelse(rint_sd == 1 & res_sd == 1, "base",  condition)]
  simsum_sub_d[, condition := ifelse(rint_sd == sqrt(.5) & res_sd == sqrt(1.5), "REsmall_RESlarge",  condition)]
  simsum_sub_d[, condition := ifelse(rint_sd == sqrt(1.5) & res_sd == sqrt(.5), "RElarge_RESsmall",  condition)]
  simsum_sub_d[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(1.5), "REbase_RESlarge",  condition)]
  simsum_sub_d[, condition := ifelse(rint_sd == 1 & res_sd == sqrt(.5), "REbase_RESsmall",  condition)]
  simsum_sub_d[, condition := factor(condition, levels = c(
    "base",
    "REsmall_RESlarge",
    "RElarge_RESsmall",
    "REbase_RESlarge",
    "REbase_RESsmall"
  ))]
  
  simsum_sub_d[, cond := paste0("J: ", J, ", ", "I: ", I, ", ", "sigma: ", condition)]
  
})
names(simsum_sub) <- c("simsum_sub_d3",
                       "simsum_sub_d4",
                       "simsum_sub_d5")

rm(simsum_sub_1, simsum_sub_2, simsum_sub_3, simsum_sub_4, simsum_sub_5, simsum_sub_6,
   simsum_sub_7, simsum_sub_8, simsum_sub_9, simsum_sub_10, simsum_sub_11, simsum_sub_12,
   simsum_sub_13, simsum_sub_14, simsum_sub_15, simsum_sub_16, simsum_sub_17, simsum_sub_18,
   simsum_sub_19, simsum_sub_20, simsum_sub_21, simsum_sub_22, simsum_sub_23, simsum_sub_24)

# merge diag stats to simsum_sub and save -----------------------
## 3 parts
simsum_sub[["simsum_sub_d3"]] <- merge(simsum_sub[["simsum_sub_d3"]],
                                       simsum_brmcoda[["simsum_brmcoda_d3"]],
                                       by = intersect(colnames(simsum_sub[["simsum_sub_d3"]]), 
                                                      colnames(simsum_brmcoda[["simsum_brmcoda_d3"]]))
)

simsum_sub[["simsum_sub_d4"]] <- merge(simsum_sub[["simsum_sub_d4"]],
                                       simsum_brmcoda[["simsum_brmcoda_d4"]],
                                       by = intersect(colnames(simsum_sub[["simsum_sub_d4"]]), 
                                                      colnames(simsum_brmcoda[["simsum_brmcoda_d4"]]))
)

simsum_sub[["simsum_sub_d5"]] <- merge(simsum_sub[["simsum_sub_d5"]],
                                       simsum_brmcoda[["simsum_brmcoda_d5"]],
                                       by = intersect(colnames(simsum_sub[["simsum_sub_d5"]]), 
                                                      colnames(simsum_brmcoda[["simsum_brmcoda_d5"]]))
)

# save --------------
# saveRDS(simsum_sub, "/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_sub.RDS")
# saveRDS(simsum_brmcoda, "/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/simsum_brmcoda.RDS")
