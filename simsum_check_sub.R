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

source("input.R") # groundtruth, conditions and functions
source("functions.R") # functions for plots
source("simsum_sub_out.R")

ssub <-
  simsum_sub_d4[condition == "RElarge_RESsmall" &
                  D == 4 &
                  From %in% c("Sleep", "SB") &
                  To %in% c("Sleep", "SB") & Level == "within" &
                  N == 1200 & K == 14]
ssub[, CI_width := abs(CI_low - CI_high)]

# cal cover and becover manually
ssub[To == "Sleep", cover := (substutitution_gt_d4[To == "Sleep" & From == "SB"]$diff_delta_y_w >= CI_low) & 
          (substutitution_gt_d4[To == "Sleep" & From == "SB"]$diff_delta_y_w <= CI_high)]
ssub[To == "SB", cover := (substutitution_gt_d4[To == "SB" & From == "Sleep"]$diff_delta_y_w >= CI_low) & 
       (substutitution_gt_d4[To == "SB" & From == "Sleep"]$diff_delta_y_w <= CI_high)]
ssub[, mean(cover), by = To] # results same as rsimsum

ssub[, becover := (Mean >= CI_low) & (Mean <= CI_high)]
ssub[, mean(becover), by = To]

# cal becover using rsimsum formula - ie whether CI covers the mean of the estimates over the runs
# becover <- 1 / 2000 * sum(mean(ssub[To == "SB"][["Mean"]], na.rm = TRUE) >= ssub[To == "SB"][["CI_low"]] & mean(ssub[To == "SB"][["Mean"]], na.rm = TRUE) <= ssub[To == "SB"][["CI_high"]], na.rm = TRUE)

ssub[To == "SB", becover2 := (mean(ssub[To == "SB"][["Mean"]], na.rm = TRUE) >= CI_low) & 
       (mean(ssub[To == "SB"][["Mean"]], na.rm = TRUE) <= CI_high)]
ssub[To == "Sleep", becover2 := (mean(ssub[To == "Sleep"][["Mean"]], na.rm = TRUE) >= CI_low) & 
       (mean(ssub[To == "Sleep"][["Mean"]], na.rm = TRUE) <= CI_high)]
ssub[, mean(becover2), by = To]
# poor because results from substitution are rounded

# now round the mean to check
ssub[To == "SB", becover3 := 
       (round(mean(ssub[To == "SB"][["Mean"]], na.rm = TRUE), 2) >= CI_low) & 
       (round(mean(ssub[To == "SB"][["Mean"]], na.rm = TRUE), 2) <= CI_high)]
ssub[To == "Sleep", becover3 := 
       (round(mean(ssub[To == "Sleep"][["Mean"]], na.rm = TRUE), 2) >= CI_low) & 
       (round(mean(ssub[To == "Sleep"][["Mean"]], na.rm = TRUE), 2) <= CI_high)]
ssub[, mean(becover3), by = To]

ggplot(ssub[1:1000][To == "Sleep"], aes(y = run, x = Mean, xmin = CI_low, xmax = CI_high, colour = run)) +
  geom_point() +
  geom_errorbar() +
  geom_vline(xintercept = -0.05946445, color = "red")
ggplot(ssub[1:1000][To == "SB"], aes(y = run, x = Mean, xmin = CI_low, xmax = CI_high, colour = run)) +
  geom_point() +
  geom_errorbar() +
  geom_vline(xintercept = 0.05946445, color = "red")
