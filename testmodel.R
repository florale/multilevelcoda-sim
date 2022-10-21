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

library(ggplot2)
library(ggsci)

synd <- readRDS("syntheticpopulation.RDS")
synd <- setDT(synd)

sbp <- matrix(c(
  1, 1, -1,-1, -1,
  1, -1, 0, 0, 0,
  0, 0, 1, -1, -1,
  0, 0, 0, 1, -1), ncol=5, byrow=TRUE)

simmodel <- function(database, sbpbase) {
  
  psub <- possub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
  parts <- colnames(psub)
  
  cilr <- compilr(database, sbpbase, parts, total = 1440, idvar = "ID")
  
  model <- brmcoda(cilr,
                   depression ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
                     (1 + wilr2 | ID), cores = 4, chains = 4, iter = 1500, warmup = 500,
                   backend = "cmdstanr")
  
  modelout <- data.table(
    bilr1 = summary(model$Model)$fixed[2, 1],
    bilr1_CILow = summary(model$Model)$fixed[2, 3],
    bilr1_CIHigh = summary(model$Model)$fixed[2, 4],
    
    bilr2 = summary(model$Model)$fixed[3, 1],
    bilr2_CILow = summary(model$Model)$fixed[3, 3],
    bilr2_CIHigh = summary(model$Model)$fixed[3, 4],
    
    bilr3 = summary(model$Model)$fixed[4, 1],
    bilr3_CILow = summary(model$Model)$fixed[4, 3],
    bilr3_CIHigh = summary(model$Model)$fixed[4, 4],
    
    bilr4 = summary(model$Model)$fixed[5, 1],
    bilr4_CILow = summary(model$Model)$fixed[5, 3],
    bilr4_CIHigh = summary(model$Model)$fixed[5, 4],
    
    wilr1 = summary(model$Model)$fixed[6, 1],
    wilr1_CILow = summary(model$Model)$fixed[6, 3],
    wilr1_CIHigh = summary(model$Model)$fixed[6, 4],
    
    wilr2 = summary(model$Model)$fixed[7, 1],
    wilr2_CILow = summary(model$Model)$fixed[7, 3],
    wilr2_CIHigh = summary(model$Model)$fixed[7, 4],
    
    wilr3 = summary(model$Model)$fixed[8, 1],
    wilr3_CILow = summary(model$Model)$fixed[8, 3],
    wilr3_CIHigh = summary(model$Model)$fixed[8, 4],
    
    wilr4 = summary(model$Model)$fixed[9, 1],
    wilr4_CILow = summary(model$Model)$fixed[9, 3],
    wilr4_CIHigh = summary(model$Model)$fixed[9, 4],
    
    Rhat = summary(model$Model)$fixed[, 5]
  )
  
  bsubm <- bsub(model, substitute = psub, minute = 30)
  wsubm <- wsub(model, substitute = psub, minute = 30)
  
  out <- list(
    CompILR = cilr,
    Result = modelout,
    BetweenResult = bsubm,
    WithinResult = wsubm,
    N = N,
    k = k
  )
}
# max = 14000 - N = 1000, k = 14
# min N = 20, k = 3
# 200 obs

out1 <- list()
system.time(
  for (N in 10:10*(1:20)) {
    for (k in 3:14) {
      useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
      dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
      
      mod <- simmodel(dat, sbpbase = sbp)
      out1[[N]] <- list(mod)
    }
  })
saveRDS(out1, "out1.RDS")

out2 <- list()
system.time(
  for (N in 10:10*(21:40)) {
    for (k in 3:14) {
      useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
      dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
      
      mod <- simmodel(dat, sbpbase = sbp)
      out2[[N]] <- list(mod)
    }
  })
saveRDS(out2, "out2.RDS")

out3 <- list()
system.time(
  for (N in 10:10*(41:60)) {
    for (k in 3:14) {
      useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
      dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]

      mod <- simmodel(dat, sbpbase = sbp)
      out3[[N]] <- list(mod)
    }
  })
saveRDS(out3, "out3.RDS")

out4 <- list()
system.time(
  for (N in 10:10*(61:70)) {
    for (k in 3:14) {
      useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
      dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]

      mod <- simmodel(dat, sbpbase = sbp)
      out4[[N]] <- list(mod)
    }
  })
saveRDS(out4, "out4.RDS")

out5 <- list()
system.time(
  for (N in 10:10*(71:80)) {
    for (k in 3:14) {
      useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
      dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]

      mod <- simmodel(dat, sbpbase = sbp)
      out5[[N]] <- list(mod)
    }
  })
saveRDS(out5, "out5.RDS")


out6 <- list()
system.time(
  for (N in 10:10*(81:90)) {
    for (k in 3:14) {
      useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
      dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
      
      mod <- simmodel(dat, sbpbase = sbp)
      out6[[N]] <- list(mod)
    }
  })
saveRDS(out6, "out6.RDS")

out7 <- list()
system.time(
  for (N in 10:10*(91:100)) {
    for (k in 3:14) {
      useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
      dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
      
      mod <- simmodel(dat, sbpbase = sbp)
      out7[[N]] <- list(mod)
    }
  })
saveRDS(out7, "out7.RDS")

# user  system elapsed 
# 37.035   1.449  36.356 

# # 24 models - max 100 obs
# system.time(
#   for (N in c(10, 20, 50, 100)) {
#     for (k in 5:10) {
#       useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
#       dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
#       
#       mod <- simmodel(dat, sbp = sbp)
#       out[[N]] <- list(mod)
#     }
#   })
# 
# # user  system elapsed 
# # 999.735  35.059 898.519 
# 
# # 14000 obs
# system.time(
#   for (N in 1000 ) {
#     for (k in 14) {
#       useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
#       dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
#       
#       mod <- simmodel(dat, sbp = sbp)
#       out[[N]] <- list(mod)
#     }
#   })
# 
# # user  system elapsed 
# # 428.006   4.414 163.029 

