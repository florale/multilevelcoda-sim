
# ---- libs ----



library("dplyr")

library("tidyr")
library("ggplot2")

library("mvtnorm")
library("foreach")

library("brms")
library("compositions")


# library("compositions")
# library("coda") # glmm posterioir ests

library("mixAK")



# ---- seed ----

# for reproducibility of random number generation:
set.seed(1234)



# ---- constants ----

D <- 3
p <- 1

n_1 <- 150
n_2 <- 250
n <- n_1 + n_2


k <- c(rep(1L, n_1), rep(2L, n_2))
# class(k)
id_dat <- 
  data.frame(k = k) %>% 
  mutate(id = sprintf("%03.0f", 1:n()))


nt <- 7
ts <- 1:nt




mu_alpha <- c(0.25, -0.25, 0.25)
alpha_mat <- matrix(mu_alpha, nrow = D)



# ---- grp_level ----


# group level effects

mu_1 <- 
  c(
    -0.4476885, -2.5361812, 1.0936246
  )

sigma_1 <- 
  matrix(
    c(
       0.02675576,  0.02545242, -0.01707387,
       0.02545242,  0.19441209, -0.07749162,
      -0.01707387, -0.07749162,  0.04328517
    ),
  nrow = 3, 
  byrow = TRUE
)

mu_2 <- 
  c( 
    -0.06415428, -2.07053704, 0.92007509
  )

sigma_2 <- 
  matrix(
    c(
       0.01301868,  0.02130291, -0.00297424,
       0.02130291,  0.17447517, -0.05103463,
      -0.00297424, -0.05103463,  0.02304435
    ),
    nrow = 3, 
    byrow = TRUE
  )

(n_1 * mu_1 + n_2 * mu_2) / n

b0j_1 <- 
  mvrnorm(
    n = n_1, 
    mu = mu_1,
    Sigma = sigma_1
  )

b0j_2 <- 
  mvrnorm(
    n = n_2, 
    mu = mu_2,
    Sigma = sigma_2
  )

grp_re <- rbind(b0j_1, b0j_2)
colnames(grp_re) <- paste0("b0", 1:ncol(grp_re))
grp_re <- as.data.frame(grp_re)


# ---- population_level ----

populn_dat <-
  inner_join(
    cbind(id_dat, grp_re),
    expand.grid(id = id_dat[["id"]], time = ts),
    "id"
  ) %>%
  as_tibble(.) %>%
  mutate(x01 = as.integer(time >= 6))


sigma_e <- 2 * (sigma_1 + sigma_2)


# ---- create_outcome ----

x <- populn_dat %>% select(starts_with("x0")) %>% as.matrix()

X <- foreach(j = 1:D, .combine = cbind) %do% { x }

Y <-
  populn_dat %>% select(starts_with("b0")) +
  X %*% alpha_mat +
  rmvnorm(n = nrow(populn_dat), mean = rep(0, D), sigma = sigma_e)

colnames(Y) <- gsub("^b0", "y", colnames(Y))
Y <- as_tibble(Y)


populn_dat <-
  populn_dat %>%
  bind_cols(
    .,
    Y
  )

saveRDS(populn_dat, "randgenexamples/data/populn_dat_ex2.rds")

populn_dat



# ---- plot ----

populn_dat %>%
  pivot_longer(
    .,
    cols = starts_with("y")
  ) %>%
  ggplot(., aes(x = name, y = value, col = factor(k))) +
  geom_line(aes(group = id), alpha = 0.25) +
  geom_jitter(alpha = 0.25, height = 0, width = 0.1) +
  facet_wrap(~time, ncol = 4, labeller = label_both) +
  theme_bw()


ggsave("randgenexamples/fig/populn_dat_ex2.pdf", width = 10, height = 7)


tu_comps <-
  populn_dat %>%
  select(starts_with("y")) %>%
  as.data.frame(.) %>%
  ilrInv(.)%>%
  as.data.frame(.) 

colnames(tu_comps) <- paste0("time_", c("sed", "lpa", "mvpa", "sleep"))
head(tu_comps)

bind_cols(populn_dat, tu_comps) %>%
  pivot_longer(
    .,
    cols = starts_with("time_")
  ) %>%
  ggplot(., aes(x = name, y = value, col = factor(k))) +
  geom_line(aes(group = id), alpha = 0.1) +
  geom_jitter(alpha = 0.25, height = 0, width = 0.1) +
  facet_wrap(~time, ncol = 4, labeller = label_both) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

ggsave("randgenexamples/fig/populn_dat_ex2_timeuse.pdf", width = 10, height = 7)

