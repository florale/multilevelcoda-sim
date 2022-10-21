source("m3setup.R")

# out1 <- list()
# system.time(
#   for (N in 10:10*(1:20)) {
#     for (k in 3:14) {
#       useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
#       dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
#       
#       mod <- simmodel(dat, sbpbase = sbp)
#       out1[[N]] <- list(mod)
#     }
#   })
# saveRDS(out1, "out1.RDS")

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

# out3 <- list()
# system.time(
#   for (N in 10:10*(41:60)) {
#     for (k in 3:14) {
#       useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
#       dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
#       
#       mod <- simmodel(dat, sbpbase = sbp)
#       out3[[N]] <- list(mod)
#     }
#   })
# saveRDS(out3, "out3.RDS")
# 
# out4 <- list()
# system.time(
#   for (N in 10:10*(61:70)) {
#     for (k in 3:14) {
#       useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
#       dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
#       
#       mod <- simmodel(dat, sbpbase = sbp)
#       out4[[N]] <- list(mod)
#     }
#   })
# saveRDS(out4, "out4.RDS")
# 
# out5 <- list()
# system.time(
#   for (N in 10:10*(71:80)) {
#     for (k in 3:14) {
#       useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
#       dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
#       
#       mod <- simmodel(dat, sbpbase = sbp)
#       out5[[N]] <- list(mod)
#     }
#   })
# saveRDS(out5, "out5.RDS")
# 
# 
# out6 <- list()
# system.time(
#   for (N in 10:10*(81:90)) {
#     for (k in 3:14) {
#       useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
#       dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
#       
#       mod <- simmodel(dat, sbpbase = sbp)
#       out6[[N]] <- list(mod)
#     }
#   })
# saveRDS(out6, "out6.RDS")
# 
# out7 <- list()
# system.time(
#   for (N in 10:10*(91:100)) {
#     for (k in 3:14) {
#       useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
#       dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
#       
#       mod <- simmodel(dat, sbpbase = sbp)
#       out7[[N]] <- list(mod)
#     }
#   })
# saveRDS(out7, "out7.RDS")
