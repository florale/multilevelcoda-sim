source("m3setup.R")

set.seed(2) # set different for each script

sampledd <- d[sample(seq_len(.N), size = 2000, replace = TRUE, prob = wt)]

ggplot(sampledd, aes(x = N, y = K)) + 
  geom_density_2d_filled()

out2 <- list()
system.time(
  for (i in seq_len(nrow(sampledd))) {
    
    N = sampledd[i]$N
    K = sampledd[i]$K
    
    useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
    dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), K, replace = FALSE)], by = ID]
    
    out2[[i]] <- simmodel(dat, sbpbase = sbp)
  })
saveRDS(out2, "out2.RDS")