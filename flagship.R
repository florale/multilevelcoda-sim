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
extrafont::font_import()

brmcoda_tab <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/brmcoda_tab.RDS")
brmcoda_dat <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/brmcoda_dat.RDS")

sub_tab <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/sub_tab.RDS")
sub_dat <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/sub_dat.RDS")

brmcoda_dat_d4 <- brmcoda_dat[["brmcoda_d4"]]
brmcoda_dat_d4[, Estimand := droplevels(Estimand)]
brmcoda_dat_d4[, EstimandF := droplevels(EstimandF)]

sub_dat_d4 <- sub_dat[["sub_d4"]]

brmcoda_tab_d4 <- brmcoda_tab[["brmcoda_d4"]]
sub_tab_d4 <- sub_tab[["sub_d4"]]

## PLOTS ------------
## common column
comm_col <- 
  ggplot(brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3], aes(y = Estimand)) +
  geom_text(aes(x = 0, label = brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimand, 
                family = "Arial Narrow"),
            vjust = 0.5, 
            colour = "black",
            fontface = ifelse(brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimand == "Estimand", "bold", "plain"),
            hjust = 0) +
  xlim(-0.2, -0.0)  +
  geom_text(aes(label = "Estimand", y = 10, x = 0, 
                family = "Arial Narrow"), 
            fontface = "bold",
            color = "black", vjust = "inward", hjust = 0) + 
  scale_x_discrete(drop = FALSE) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) +
  geom_segment(aes(x = -Inf, y = 9.4, xend = Inf, yend = 9.4), color = "black", linewidth = 0.5) 
comm_col 

# brmcoda bias plots ---------------------
# test plot
p01 <- .par_plot(brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3])
# annotate("text", x = 9.75, y =0, label = "NK")
p01
p02 <- ggplot(brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3], aes(y = Estimand)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimand), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimand == "Estimand", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3]$`Est [95% CI], MCSE`, 
                # colour = d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimates,
                family = "Arial Narrow"), 
            vjust = 0,
            colour = ifelse(brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3]$OnTarget == "Y", "black", "grey")) +
  geom_text(aes(label = "Est [95% CI], MCSE", y = 10, x = -0.25, 
                family = "Arial Narrow"), 
            color = "black", vjust = "inward", hjust = "inward") + 
  xlim(-0.3, -0.2) +
  scale_x_discrete(drop = FALSE) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) 
p02

layout <- c(
  area(t = 0, l = 0, b = 11, r = 3),
  area(t = 0, l = 4, b = 11, r = 7)
)
plot(layout)
p01 + p02 + plot_layout(design = layout)

# execute 
out = list()
for (n in levels(brmcoda_dat_d4[Stat == "bias" & condition == "base"]$N)) {
  for (k in levels(brmcoda_dat_d4[Stat == "bias" & condition == "base"]$K)) {
    
    layout <- layout
    
    tmp <- brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == n & K == k]
    p01 <- .par_plot(tmp)
    
    p02 <- ggplot(tmp, aes(y = Estimand)) +
      geom_text(aes(x = -0.25, label = tmp$`Est [95% CI], MCSE`, family = "Arial Narrow"),
                vjust = 0.5,
                # colour = "black",
                # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
                colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")) + 
      # colour = d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimates,
      geom_text(aes(label = "Est [95% CI], MCSE", y = 10, x = -0.25, 
                    family = "Arial Narrow"), 
                color = "black",
                fontface = "bold",
                vjust = "inward", hjust = "inward") + 
      xlim(-0.3, -0.2) +
      scale_x_discrete(drop = FALSE) +
      hrbrthemes::theme_ipsum() + theme_void() +
      theme(
        axis.ticks        = element_blank(),
        panel.background  = element_blank(),
        panel.border      = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        plot.background   = element_rect(fill = "transparent", colour = NA),
        axis.title.y      = element_blank(),
        axis.title.x      = element_blank(),
        axis.text.x       = element_blank(),
        axis.text.y       = element_blank()
      ) 
    
    out[[n]][[k]] <- list(list(p01, p02))
  }
}
# patch 1st layer
p <- lapply(do.call(c, do.call(c, out)), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 9.4, yend = Inf, xend = 9.4), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 9.4, xend = Inf, yend = 9.4), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check - looks good
comm_col | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- comm_col | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- comm_col | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- comm_col | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- comm_col | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

png("brmcoda_bias.png", width = 14, height = 13, units = 'in', res = 1000)
ggarrange(p1, p2, p3, p4,
               nrow = 4)
dev.off()

# brmcoda cover plots ---------------------
# test plot
p01 <- .par_plot(brmcoda_dat_d4[Stat == "cover" & condition == "base" & N == 30 & K == 3])
# annotate("text", x = 9.75, y =0, label = "NK")
p01

p02 <- ggplot(brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3], aes(y = Estimand)) +
  # geom_text(aes(x = -0.5, label = d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimand), 
  #           vjust = -1.5, colour = "black",
  #           fontface = ifelse(d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimand == "Estimand", "bold", "plain")) +
  geom_text(aes(x = -0.25, label = brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3]$`Est [95% CI], MCSE`, 
                # colour = d[Stat == "bias" & condition == "base" & N == 30 & K == 3]$Estimates,
                family = "Arial Narrow"), 
            vjust = 0,
            colour = ifelse(brmcoda_dat_d4[Stat == "bias" & condition == "base" & N == 30 & K == 3]$OnTarget == "Y", "black", "grey")) +
  geom_text(aes(label = "Est [95% CI], MCSE", y = 10, x = -0.25, 
                family = "Arial Narrow"), 
            color = "black", vjust = "inward", hjust = "inward") + 
  xlim(-0.3, -0.2) +
  scale_x_discrete(drop = FALSE) +
  hrbrthemes::theme_ipsum() + theme_void() +
  theme(
    axis.ticks        = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    axis.title.y      = element_blank(),
    axis.title.x      = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank()
  ) 
p02

layout <- c(
  area(t = 0, l = 0, b = 11, r = 3),
  area(t = 0, l = 4, b = 11, r = 7)
)
plot(layout)
p01 + p02 + plot_layout(design = layout)

# execute
out = list()
for (n in levels(brmcoda_dat_d4[Stat == "cover" & condition == "base"]$N)) {
  for (k in levels(brmcoda_dat_d4[Stat == "cover" & condition == "base"]$K)) {
    
    layout <- layout
    
    tmp <- brmcoda_dat_d4[Stat == "cover" & condition == "base" & N == n & K == k]
    p01 <- .par_plot(tmp)
    
    p02 <- ggplot(tmp, aes(y = Estimand)) +
      geom_text(aes(x = -0.25, label = tmp$`Est [95% CI], MCSE`, family = "Arial Narrow"),
                vjust = 0.5,
                # colour = "black",
                # fontface = ifelse(tmp1$OnTarget == "Y", "bold", "plain")) +
                colour = ifelse(tmp$OnTarget == "Y", "black", "#A3A3A3")) + 
      # colour = d[Stat == "cover" & condition == "base" & N == 30 & K == 3]$Estimates,
      geom_text(aes(label = "Est [95% CI], MCSE", y = 10, x = -0.25, 
                    family = "Arial Narrow"), 
                color = "black",
                fontface = "bold",
                vjust = "inward", hjust = "inward") + 
      xlim(-0.3, -0.2) +
      scale_x_discrete(drop = FALSE) +
      hrbrthemes::theme_ipsum() + theme_void() +
      theme(
        axis.ticks        = element_blank(),
        panel.background  = element_blank(),
        panel.border      = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        plot.background   = element_rect(fill = "transparent", colour = NA),
        axis.title.y      = element_blank(),
        axis.title.x      = element_blank(),
        axis.text.x       = element_blank(),
        axis.text.y       = element_blank()
      ) 
    
    out[[n]][[k]] <- list(list(p01, p02))
  }
}
# patch 1st layer
p <- lapply(do.call(c, do.call(c, out)), function(i){
  p <- 
    i[[1]] + geom_segment(aes(y = -Inf, x = 9.4, yend = Inf, xend = 9.4), color = "black", linewidth = 0.5) +
    i[[2]] + geom_segment(aes(x = -Inf, y = 9.4, xend = Inf, yend = 9.4), color = "black", linewidth = 0.5) +
    plot_layout(design = layout)
})

# check - looks good
comm_col | p[[1]] | p[[2]] | p[[3]] | p[[4]]

# patch all
p1 <- comm_col | p[[1]] | p[[2]] | p[[3]] | p[[4]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p2 <- comm_col | p[[5]] | p[[6]] | p[[7]] | p[[8]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p3 <- comm_col | p[[9]] | p[[10]] | p[[1]] | p[[12]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p4 <- comm_col | p[[13]] | p[[14]] | p[[15]] | p[[16]] + theme(plot.margin = unit(c(1,1,0,0), "lines"))
p1 / p2 / p3 / p4

png("brmcoda_cover.png", width = 14, height = 13, units = 'in', res = 1000)
ggarrange(p1, p2, p3, p4,
          nrow = 4)
dev.off()