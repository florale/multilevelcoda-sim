library(JWileymisc)
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
library(patchwork)

brmcoda_tab <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/brmcoda_tab.RDS")
brmcoda_dat <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/brmcoda_dat.RDS")

sub_tab <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/sub_tab.RDS")
sub_dat <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/sub_dat.RDS")

brmcoda_dat_d4 <- brmcoda_dat[["brmcoda_d4"]]
brmcoda_dat_d4[, Estimand := droplevels(Estimand)]

sub_dat_d4 <- sub_dat[["sub_d4"]]

brmcoda_tab_d4 <- brmcoda_tab[["brmcoda_d4"]]
sub_tab_d4 <- sub_tab[["sub_d4"]]

# brmcoda_d4 <- brmcoda_dat[["brmcoda_d4"]]
# sub_d4 <- sub_dat[["sub_d4"]]

#check on target
egltable("OnTarget", g = "Estimand", brmcoda_tab_d4[Stat == "bias"])
egltable("OnTarget", brmcoda_tab_d4[Stat == "bias"])

egltable("OnTarget", g = "Estimand", brmcoda_tab_d4[Stat == "cover"])
egltable("OnTarget", g = "Estimand", brmcoda_tab_d4[Stat == "becover"])
est <- brmcoda_dat_d4[stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimates

# test
p <- .par_plot(brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3])
p
d <- rbind(brmcoda_dat_d4,
           data.table(Estimand = "Estimand",
                      Estimates = "Estimates",
                      Stat = "bias",
                      condition = "base",
                      N = 30,
                      K = 3,
                      OnTarget = "Y"), fill = TRUE)

t <- ggplot(d[Stat == "bias" & condition == "base" & N == 30 & K == 3], aes(y = Estimand)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimand), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimand == "Estimand", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimates), 
            vjust = 0.5, colour = "black",
            fontface = ifelse(d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$OnTarget == "Y", "bold", "plain")) + 
  xlim(-0.3, -0.2) + theme_void()

layout <- c(
  area(t = 0, l = 0, b = 10, r = 3),
  area(t = 1, l = 2, b = 10, r = 6)
)
plot(layout)

t + p + plot_layout(design = layout)
# t | p + plot_layout(design = layout)

# execute
comm_col <- 
  ggplot(d[Stat == "bias" & condition == "base" & N == 30 & K == 3], aes(y = Estimand)) +
  geom_text(aes(x = -0.35, label = d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimand),
            vjust = -1.5, colour = "black",
            fontface = ifelse(d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimand == "Estimand", "bold", "plain")) +
  xlim(-0.4, -0.3) + theme_void()
comm_col

out = list()
for (n in levels(brmcoda_dat_d4[Stat == "bias" & condition == "base"]$N)) {
  for (k in levels(brmcoda_dat_d4[Stat == "bias" & condition == "base"]$K)) {
    
    layout <- c(
      area(t = 0, l = 0, b = 20, r = 2),
      area(t = 2, l = 3, b = 20, r = 4)
    )
    
    tmp <- brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == n & K == k]
    p <- .par_plot(tmp)
    
    tmp1 <- rbind(tmp,
                  data.table(Estimand = "Estimand",
                             Estimates = "Estimates",
                             Stat = "bias",
                             condition = "base",
                             N = n,
                             K = k,
                             OnTarget = "Y"), fill = TRUE)
    
    t <- ggplot(tmp1, aes(y = Estimand)) +
      geom_text(aes(x = -0.25, label = tmp1$Estimates),
                vjust = 0,
                # colour = "black",
                fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
      xlim(-0.55, 0) + theme_void()
    
    pt <- t + p + plot_layout(design = layout)
    out[[n]][[k]] <- list(pt)
  }
}
out <- do.call(c, do.call(c, out))


# layout for all
# layout <- c(
#   area(t = 0, l = 0, b = 10, r = 2),
#   area(t = 0, l = 3, b = 10, r = 5),
#   area(t = 0, l = 6, b = 10, r = 9)
#   # area(t = 0, l = 16, b = 10, r = 21),
#   # area(t = 0, l = 21, b = 10, r = 26)
# )
# plot(layout)

p1 <- out[[1]] | out[[2]] | out[[3]] | out[[4]]
p2 <- out[[5]] | out[[6]] | out[[7]] | out[[8]]
p3 <- out[[9]] | out[[10]] | out[[1]] | out[[12]]
p4 <- out[[13]] | out[[14]] | out[[15]] | out[[16]]

layout <- c(
  area(t = 0, l = 0, b = 20, r = 2),
  area(t = 2, l = 3, b = 20, r = 50)
)
plot(layout)
comm_col | p1 + plot_layout(ncol = 4)

p1 / p2 / p3 / p4
ggarrange(p1, p2, p3, p4)
