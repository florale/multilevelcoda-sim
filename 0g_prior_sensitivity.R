library(MASS)
library(knitr)
library(xtable)
library(papaja)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
#library(sjPlot)
library(brms)
library(gridExtra)
library(cmdstanr)
library(priorsense)
library(posterior)
library(ggh4x)
library(elementalist)
library(bayesplot)
library(patchwork)

# theme_set(theme_apa())

outputdir <- "/Users/florale/Library/CloudStorage/OneDrive-Personal/monash/projects/multilevelcoda/multilevelcoda-sim-proj/multilevelcoda-sim-results/"

# c_light <- "#DCBCBC"; c_light_highlight <- "#C79999"
# c_mid   <- "#B97C7C"; c_mid_highlight   <- "#A25050"
# c_dark  <- "#8F2727"; c_dark_highlight  <- "#7C0000"

color_scheme_get("blue")

c_light <- "#d1e1ec"; c_light_highlight <- "#ADC7DA"
c_mid   <- "#456691"; c_mid_highlight   <- "#8AAFCA"
c_dark  <- "#2A3E59"; c_dark_highlight  <- "#011f4b" #03396c

## prior likelihood posterior ----

FigBay <- function(corr) {
  sigma <- matrix(c(1,corr,corr,1), nrow=2)
  m <- c(0, 0)
  data.grid <- expand.grid(s.1 = seq(-3, 3, length.out=200), s.2 = seq(-3, 3, length.out=200))
  q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = m, sigma = sigma))
  FigBay3c <- ggplot(q.samp, aes(x=s.1, y=s.2)) + 
    geom_raster(aes(fill = prob)) +
    scale_fill_gradient(low="white",high=c_mid,guide=FALSE)+
    labs(x="",y="")+
    coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)+
    papaja::theme_apa() +
    # hrbrthemes::theme_ipsum() +
    theme(axis.line=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = c_dark, size = 4, fill=NA, linewidth = 1),
          axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.margin       = unit(c(0,0,0,0), "lines")
    )
  return(FigBay3c)
}

sigma <- matrix(c(1,0.9,0.9,1), nrow=2)
m <- c(0, 0)
data.grid <- expand.grid(s.1 = seq(-3, 3, length.out=200), s.2 = seq(-3, 3, length.out=200))
q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = m, sigma = sigma))
maxQ <- max(q.samp$prob)
mINQ <- min(q.samp$prob)
s1 <- unique(q.samp$s.1)
q.samp$prob2 <- NA
for (i in 1:length(s1)) { # i <- 1
  idxQ <- which(q.samp$s.1==s1[i])
  q.samp$prob2[idxQ] <- dnorm(q.samp$s.2[idxQ],s1[i],1)
}
q.samp$prob <- q.samp$prob2
FigBay2bc3b <- ggplot(q.samp, aes(x=s.1, y=s.2)) + 
  geom_raster(aes(fill = prob)) +
  scale_fill_gradient(low="white",high=c_mid,guide=FALSE)+
  labs(x="",y="")+
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), ratio = 1)+
  papaja::theme_apa() +
  # hrbrthemes::theme_ipsum() +
  theme(axis.line=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = c_dark, size = 4, fill=NA, linewidth = 1),
        # panel.background = element_rect_round(radius = unit(1, "cm"), colour = c_dark, fill=NA, linewidth = 1),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.margin       = unit(c(0,0,0,0), "lines")
        
  )

# FigBay1a2a  <- FigBay(1) + labs(title="Prior") + theme(title=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
# FigBay3c    <- FigBay(0.5) + labs(title="Posterior") + theme(title=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
# 
# FigBay1bc3a <- FigBay(0) + theme(title=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
# FigBay1b    <- FigBay1bc3a + labs(title="Likelihood") + theme(title=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
# FigBay1c    <- FigBay1bc3a + labs(title="Posterior") + theme(title=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
# FigBay3a    <- FigBay1bc3a + labs(title="Prior") + theme(title=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
# 
# FigBay2b3b  <- FigBay2bc3b + labs(title="Likelihood") + theme(title=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
# FigBay2c    <- FigBay2bc3b + labs(title="Posterior") + theme(title=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))

FigBay1a2a  <- FigBay(1) + xlab( "Prior") + theme(axis.title.x =element_text(family = "Arial Narrow", size = 13, margin = unit(c(0,0,0,0), "lines")))
FigBay3c    <- FigBay(0.5) + xlab("Posterior") + theme(axis.title.x=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))

FigBay1bc3a <- FigBay(0) + theme(element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
FigBay1b    <- FigBay1bc3a + xlab("Likelihood") + theme(axis.title.x=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
FigBay1c    <- FigBay1bc3a + xlab("Posterior") + theme(axis.title.x=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
FigBay3a    <- FigBay1bc3a + xlab("Prior") + theme(axis.title.x=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))

FigBay2b3b  <- FigBay2bc3b + xlab("Likelihood") + theme(axis.title.x=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))
FigBay2c    <- FigBay2bc3b + xlab("Posterior") + theme(axis.title.x=element_text(family = "Arial Narrow", size = 12, margin = unit(c(0,0,0,0), "lines")))

panel_a <- FigBay1a2a + FigBay1b + FigBay1c
panel_b <- FigBay1a2a + FigBay2b3b + FigBay2c
panel_c <- FigBay3a + FigBay2b3b + FigBay3c

grDevices::cairo_pdf(
  file = paste0(outputdir, "prior_likelihood_posterior", ".pdf"),
  width = 6,
  height = 6,
)
ggpubr::ggarrange(panel_a, panel_b, panel_c,
                  nrow = 3,
                  labels = c("A", "B", "C"),
                  vjust = 1.75,
                  # hjust = -4,
                  font.label = list(size = 14, color = "black", family = "Arial Narrow"))

dev.off()

# # prior check real data study -----------------------
brmcoda_gt <- readRDS("brmcoda_gt.RDS")
fit <- brmcoda_gt$m5$Model

summary(fit)

# m5
prior_summary(brmcoda_gt$m5$Model)

v <- c("b_Intercept", 
       "b_bilr1", "b_bilr2", "b_bilr3", "b_bilr4",
       "b_wilr1", "b_wilr2", "b_wilr3", "b_wilr4",
       "sd_UID__Intercept",
       "sigma")

make_stancode(SLEEPYNextDay ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 + (1 | UID), data = model.frame(brmcoda_gt$m5$Model))

# fit a model manually via rstan
scode <- '
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  array[M_1] vector[N_1] z_1;  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  real log_prior = 0;  // prior contributions to the log posterior
  r_1_1 = (sd_1[1] * (z_1[1]));
  log_prior += student_t_lpdf(Intercept | 3, 1.7, 2.5);
  log_prior += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  log_prior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
    }
    target += normal_id_glm_lpdf(Y | Xc, mu, b, sigma);
  }
  // priors including constants
  target += log_prior;
  target += std_normal_lpdf(z_1[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  
  // Manually added to exclude random intercept prior
  real lprior;
  lprior = student_t_lpdf(Intercept | 3, 1.7, 2.5) +
  student_t_lpdf(sigma | 3, 0, 2.5)- 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
'
sdata <- make_standata(SLEEPYNextDay ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 + (1 | UID), data = model.frame(brmcoda_gt$m5$Model))
stanfit <- rstan::stan(model_code = scode, data = sdata,
                       cores = 4,
                       chains = 4,
                       iter = 3000,
                       warmup = 500,
                       seed = 123)

# feed the Stan model back into brms
fit_lprior <- brm(SLEEPYNextDay ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4  + (1 | UID), data = model.frame(brmcoda_gt$m5$Model), empty = TRUE)
fit_lprior$fit <- stanfit
fit_lprior <- rename_pars(fit_lprior)
summary(fit_lprior)
prior_summary(fit_lprior)

# check log prior
log_prior_draws(fit_lprior)
log_prior_draws(fit)

# sensitivity
powerscale_sensitivity(fit_lprior, 
                       variable = v,
                       lower_alpha = 0.5,
                       upper_alpha = 2
)
powerscale_sensitivity(fit, 
                       variable = v,
                       lower_alpha = 0.5,
                       upper_alpha = 2
)

powerscale_sequence(fit_lprior, 
                    variable = v,
                    moment_match = FALSE)

grDevices::cairo_pdf(
  file = paste0(outputdir, "prior_powerscale", ".pdf"),
  width = 18,
  height = 8,
)
(plot_priorsense <- powerscale_plot_dens(
  fit_lprior,
  lower_alpha = .5, length = 3,
  variable = v,
  colors = c(c_mid_highlight, c_mid, c_dark)) +
    scale_colour_gradient(low=c_mid_highlight,high=c_dark,guide=FALSE)
)
dev.off()


(plot_fit_lprior <- powerscale_plot_dens(
  fit_lprior,
  lower_alpha = .5, length = 3,
  variable = v,
  colors = c(c_mid_highlight, c_mid, c_dark)) +
    facetted_pos_scales(
      x = list(
        scale_x_continuous(n.breaks = 3),
        scale_x_continuous(breaks = c(-0.5, 0, 0.5)),
        scale_x_continuous(breaks = c(-0.5, 0, 0.5)),
        scale_x_continuous(breaks = c(-0.5, 0, 0.5)),
        scale_x_continuous(breaks = c(1.5, 2.5, 3.5), limits = c(1.5,3.5)),
        scale_x_continuous(breaks = c(-0.8, -0.6, -0.4)),
        scale_x_continuous(breaks = c(-0.7, -0.5, -0.3)),
        scale_x_continuous(breaks = c(-0.5, -0.3, -0.1), limits = c(-0.5,-0.1)),
        scale_x_continuous(breaks = c(-0.6, -0.3, 0)),
        scale_x_continuous(breaks = c(0.6, 0.7, 0.8)),
        scale_x_continuous(breaks = c(0.7, 0.75, 0.8), limits = c(0.7,0.8)),
        scale_x_continuous(n.breaks = 3),
        scale_x_continuous(breaks = c(-0.5, 0, 0.5)),
        scale_x_continuous(breaks = c(-0.5, 0, 0.5)),
        scale_x_continuous(breaks = c(-0.5, 0, 0.5)),
        scale_x_continuous(breaks = c(1.5, 2.5, 3.5), limits = c(1.5,3.5)),
        scale_x_continuous(breaks = c(-0.8, -0.6, -0.4)),
        scale_x_continuous(breaks = c(-0.7, -0.5, -0.3)),
        scale_x_continuous(breaks = c(-0.5, -0.3, -0.1), limits = c(-0.5,-0.1)),
        scale_x_continuous(breaks = c(-0.6, -0.3, 0)),
        scale_x_continuous(breaks = c(0.6, 0.7, 0.8)),
        scale_x_continuous(breaks = c(0.7, 0.75, 0.8), limits = c(0.7,0.8))
      )) +
    scale_x_continuous(n.breaks = 3) +
    hrbrthemes::theme_ipsum() +
    theme(
      plot.background     = element_rect(fill = "transparent", colour = NA),
      panel.background    = element_rect(fill = "transparent", colour = "black", linewidth = 1),
      panel.grid.major    = element_blank(),
      panel.grid.minor    = element_blank(),
      legend.position     = "top",
      axis.text.y         = element_blank(),
      axis.text.x         = element_text(size = 16),
      strip.text          = element_text(hjust = 0.5, face = "bold", size = 20),
      legend.text         = element_text(size = 16),
      legend.title         = element_text(size = 16),
      plot.margin         = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
      panel.spacing       = unit(0.5, "lines"),
      plot.title          = element_text(size = 22),
      plot.subtitle       = element_text(size = 16)
    )
)

grDevices::cairo_pdf(
  file = paste0(outputdir, "prior_powerscale", ".pdf"),
  width = 18,
  height = 8,
)
plot_fit_lprior
dev.off()


