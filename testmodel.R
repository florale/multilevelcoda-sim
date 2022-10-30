# # main model
# m <- lmer(depression ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
#             (1 + wilr2 | ID),
#           data = tmp, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
# 
# summary(m)
# 
# # lmer substitution model --------------------------------------------------------------------------
# ID <- 1
# psub <- possub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
# min <- 60
# 
# # mean composition
# b <- cilr$BetweenComp
# mcomp <- mean(b, robust = TRUE)
# mcomp <- clo(mcomp, total = cilr$total)
# mcomp <- as.data.table(t(mcomp))
# 
# # model for no change
# bilr <- ilr(mcomp, V = cilr$psi)
# bilr <- as.data.table(t(bilr))
# wilr <- as.data.table(matrix(0, nrow = nrow(bilr), ncol = ncol(bilr)))
# colnames(wilr) <- paste0("wilr", seq_len(ncol(wilr)))
# colnames(bilr) <- paste0("bilr", seq_len(ncol(bilr)))
# 
# dsame <- cbind(bilr, wilr)
# ysame <- predict(m, newdata = dsame, re.form = NA)
# 
# ## between-person substitution model ---------------------------------------------------------------
# bout <- foreach(i = colnames(psub), .combine = c) %dopar% {
#   
#   posub <- as.data.table(psub)
#   posub <- posub[(get(i) != 0)]
#   posub <- posub[order(-rank(get(i)))]
#   
#   # substitution variable names
#   subvar <- colnames(posub) %snin% eval(i)
#   iv <- i
#   
#   kout <- vector("list", length = nrow(posub))
#   jout <- vector("list", length = min)
#   
#   for (j in seq_len(min)) { # time level
#     sub <- posub * j
#     for (k in seq_len(nrow(sub))) {
#       newcomp <- mcomp + sub[k, ]
#       names(newcomp) <- cilr$parts
#       MinSubstituted <- sub[k, get(i)]
#       kout[[k]] <- cbind(mcomp, newcomp, MinSubstituted)
#     }
#     jout[[j]] <- do.call(rbind, kout)
#   }
#   newd <- setDT(do.call(rbind, jout))
#   
#   # useful information for the final results
#   newd[, Substitute := rep(subvar, length.out = nrow(newd))]
#   newd$Predictor <- iv
#   newd$MinSubstituted <- as.numeric(newd$MinSubstituted)
#   
#   # remove impossible reallocation that result in negative values 
#   cols <- colnames(newd) %snin% c("MinSubstituted", "Substitute", "Predictor")
#   newd <- newd[rowSums(newd[, ..cols] < 0) == 0]
#   
#   # compositions and ilrs for predictions
#   bcomp <- acomp(newd[, colnames(cilr$BetweenComp), with = FALSE])
#   tcomp <- acomp(newd[, cilr$parts, with = FALSE])
#   bilr <- ilr(bcomp, V = cilr$psi)
#   tilr <- ilr(tcomp, V = cilr$psi)
#   
#   wilr <- matrix(0, nrow = nrow(tilr), ncol = ncol(tilr))
#   wilr <- as.data.table(wilr)
#   
#   colnames(tilr) <- paste0("bilr", seq_len(ncol(tilr)))
#   colnames(wilr) <- paste0("wilr", seq_len(ncol(wilr)))
#   
#   # prediction
#     subd <- cbind(newd, tilr, wilr, ID)
#     ysub <- predict(m, newdata = subd, re.form = NA)
#   
#   # difference
#     ydiff <- ysub - ysame
#     ydiff <- as.data.table(ydiff)
#     setnames(ydiff, "ydiff", "Mean")
#     
#     # final results for entire composition
#     ydiff <- cbind(ydiff, newd)
#     
#     out <- list(ydiff)
#     names(out) <- i
#     out
#   }
# 
# ## within-person substitution model ----------------------------------------------------------------
# wout <- foreach(i = colnames(psub), .combine = c) %dopar% {
#   
#   posub <- as.data.table(psub)
#   posub <- posub[(get(i) != 0)]
#   posub <- posub[order(-rank(get(i)))]
#   
#   # substitution variable names
#   subvar <- colnames(posub) %snin% eval(i)
#   iv <- i
#   
#   kout <- vector("list", length = nrow(posub))
#   jout <- vector("list", length = min)
#   
#   for (j in seq_len(min)) { # time level
#     sub <- posub * j
#     for (k in seq_len(nrow(sub))) {
#       newcomp <- mcomp + sub[k, ]
#       names(newcomp) <- cilr$parts
#       MinSubstituted <- sub[k, get(i)]
#       kout[[k]] <- cbind(mcomp, newcomp, MinSubstituted)
#     }
#     jout[[j]] <- do.call(rbind, kout)
#   }
#   newd <- setDT(do.call(rbind, jout))
#   
#   # useful information for the final results
#   newd[, Substitute := rep(subvar, length.out = nrow(newd))]
#   newd$Predictor <- iv
#   newd$MinSubstituted <- as.numeric(newd$MinSubstituted)
#   
#   # remove impossible reallocation that result in negative values 
#   cols <- colnames(newd) %snin% c("MinSubstituted", "Substitute", "Predictor")
#   newd <- newd[rowSums(newd[, ..cols] < 0) == 0]
#   
#   # compositions and ilrs for predictions
#   bcomp <- acomp(newd[, colnames(cilr$BetweenComp), with = FALSE])
#   tcomp <- acomp(newd[, cilr$parts, with = FALSE])
#   wcomp <- tcomp - bcomp 
#   
#   bilr <- ilr(bcomp, V = cilr$psi)
#   wilr <- ilr(wcomp, V = cilr$psi)
# 
#   colnames(bilr) <- paste0("bilr", seq_len(ncol(bilr)))
#   colnames(wilr) <- paste0("wilr", seq_len(ncol(wilr)))
#   
#   # prediction
#   subd <- cbind(newd, bilr, wilr, ID)
#   ysub <- predict(m, newdata = subd, re.form = NA)
#   
#   # difference
#   ydiff <- ysub - ysame
#   ydiff <- as.data.table(ydiff)
#   setnames(ydiff, "ydiff", "Mean")
#   
#   # final results for entire composition
#   ydiff <- cbind(ydiff, newd)
#   
#   out <- list(ydiff)
#   names(out) <- i
#   out
# }
# 
# # check
# ggplot(wout$TST, aes(x = MinSubstituted, y = Mean)) +
#   geom_hline(yintercept = 0, size = 0.2, linetype = 2) +
#   geom_vline(xintercept = 0, size = 0.2, linetype = 2) +
#   geom_line(aes(color = Substitute), size = 1) +
#   scale_color_jco() +
#   facet_grid(~ Substitute) +
#   xlab("Change in Within-person Sleep (mins)") +
#   ylab("Change in Depression") +
#   theme_classic()
# 
# ggplot(bout$TST, aes(x = MinSubstituted, y = Mean)) +
#   geom_hline(yintercept = 0, size = 0.2, linetype = 2) +
#   geom_vline(xintercept = 0, size = 0.2, linetype = 2) +
#   geom_line(aes(color = Substitute), size = 1) +
#   scale_color_jco() +
#   facet_grid(~ Substitute) +
#   xlab("Change in Between-person Sleep (mins)") +
#   ylab("Change in Depression") +
#   theme_classic()

# multilevelcoda model -----------------------------------------------------------------------------

# simmodel <- function(database, sbpbase) {
#   
#   psub <- possub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
#   parts <- colnames(psub)
#   
#   cilr <- compilr(database, sbpbase, parts, total = 1440, idvar = "ID")
#   
#   model <- brmcoda(cilr,
#                    depression ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
#                      (1 + wilr2 | ID), cores = 4, chains = 4, iter = 2000, warmup = 1000,
#                    backend = "cmdstanr")
#   
#   modelout <- data.table(
#     bilr1 = summary(model$Model)$fixed[2, 1],
#     bilr1_CILow = summary(model$Model)$fixed[2, 3],
#     bilr1_CIHigh = summary(model$Model)$fixed[2, 4],
#     
#     bilr2 = summary(model$Model)$fixed[3, 1],
#     bilr2_CILow = summary(model$Model)$fixed[3, 3],
#     bilr2_CIHigh = summary(model$Model)$fixed[3, 4],
#     
#     bilr3 = summary(model$Model)$fixed[4, 1],
#     bilr3_CILow = summary(model$Model)$fixed[4, 3],
#     bilr3_CIHigh = summary(model$Model)$fixed[4, 4],
#     
#     bilr4 = summary(model$Model)$fixed[5, 1],
#     bilr4_CILow = summary(model$Model)$fixed[5, 3],
#     bilr4_CIHigh = summary(model$Model)$fixed[5, 4],
#     
#     wilr1 = summary(model$Model)$fixed[6, 1],
#     wilr1_CILow = summary(model$Model)$fixed[6, 3],
#     wilr1_CIHigh = summary(model$Model)$fixed[6, 4],
#     
#     wilr2 = summary(model$Model)$fixed[7, 1],
#     wilr2_CILow = summary(model$Model)$fixed[7, 3],
#     wilr2_CIHigh = summary(model$Model)$fixed[7, 4],
#     
#     wilr3 = summary(model$Model)$fixed[8, 1],
#     wilr3_CILow = summary(model$Model)$fixed[8, 3],
#     wilr3_CIHigh = summary(model$Model)$fixed[8, 4],
#     
#     wilr4 = summary(model$Model)$fixed[9, 1],
#     wilr4_CILow = summary(model$Model)$fixed[9, 3],
#     wilr4_CIHigh = summary(model$Model)$fixed[9, 4],
#     
#     Rhat = summary(model$Model)$fixed[, 5]
#   )
#   
#   bsubm <- bsub(model, substitute = psub, minute = 30)
#   wsubm <- wsub(model, substitute = psub, minute = 30)
#   
#   out <- list(
#     CompILR = cilr,
#     Result = modelout,
#     BetweenResult = bsubm,
#     WithinResult = wsubm,
#     N = N,
#     K = K
#   )
# }

simmodel <- function(database, sbpbase, N, K) {
  
  psub <- possub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
  parts <- colnames(psub)
  
  cilr <- compilr(database, sbpbase, parts, total = 1440, idvar = "ID")
  
  model <- brmcoda(cilr,
                   depression ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
                     (1 + wilr2 | ID), cores = 4, chains = 4, iter = 2000, warmup = 1000,
                   backend = "cmdstanr")
  
  summodel <- summary(model$Model)
  ndt <- sum(subset(nuts_params(model$Model), Parameter == "divergent__")$Value)
  
  bsubm <- bsub(model, substitute = psub, minute = 30)
  wsubm <- wsub(model, substitute = psub, minute = 30)
  
  out <- list(
    CompILR = cilr,
    Result = summodel,
    BetweenResult = bsubm,
    WithinResult = wsubm,
    N = N,
    K = K,
    ndt = ndt
  )
}

test <- simmodel(data = synd[ID %in% 1:1000, .SD[1:14], by = ID], sbp = sbp)

# testing for loop for sim models -------------------------------------------------------------------
# foreach - failed, keep here to document # ------------
future::availableCores()
# set.seed(123456, "L'Ecuyer-CMRG")
registerDoFuture()
plan(list(
  tweak(multisession, workers = 2L),
  tweak(sequential)
))

# work but %dopar% is not recommended
# out <- foreach (N = c(10, 20), .combine = c) %:%
#        foreach(k = 5) %dopar% {
#     
#     useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
#     dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
#     
#     simmodel(dat, sbp, N, k)
#        }

# does not parallel #
# out <- list()
# out <- foreach (N = c(10, 20),
#                 k = 5, .combine = c) %dorng% {
# 
#     useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
#     dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
#     
#     simmodel(dat, sbp, N, k)
#   }

# only parallel outer loop - ok, but need tidyup # ------------
## if use, need to edit simmodel() to pass N and k as arguments
registerDoFuture()
plan(list(
  tweak(multisession, workers = 2L),
  tweak(sequential)
))
out <- list()
out <- foreach (N = c(10, 20), .combine = c) %dorng% {
  mod <- list()
  for (k in 3:5) {
         
         useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
         dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]
         
         mod[[k]] <- simmodel(dat, sbp, N, k)
       }
  mod
}
# normal for loop - ok # ------------------
out <- list()
mod <- list()
obs <- c(3:4)
ppl <- c(10:10*(1:2))

system.time(
  for (n in seq_along(ppl)) {
    for (o in seq_along(obs)) {
      
      N = ppl[n]
      K = obs[o]
      
      useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
      dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), K, replace = FALSE)], by = ID]
      
      mod[[o]] <- simmodel(dat, sbpbase = sbp)
    }
    out[[n]] <- mod
  })

# check divergent transitions # -----------
N <- 10
k <- 3

useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), k, replace = FALSE)], by = ID]

psub <- possub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
parts <- colnames(psub)

cilr <- compilr(dat, sbp, parts, total = 1440, idvar = "ID")

model <- brmcoda(cilr,
                 depression ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
                   (1 + wilr2 | ID), cores = 4, chains = 4, iter = 2000, warmup = 1000,
                 backend = "cmdstanr")

summary(model$Model)
launch_shinystan(model$Model)

np <- nuts_params(model$Model)
dt <- subset(np, Parameter == "divergent__")
ndt <- sum(subset(np, Parameter == "divergent__")$Value)

# weighted distribution of n and k ----------------
obs <- data.table(K = 3:28)
obs[, Kwt := dbeta((K - min(K))/(max(K) - min(K)), 
                   1, 2)]
obs[, Kwt := Kwt/sum(Kwt)]

ppl <- data.table(N = c(10:1000))
ppl[, Nwt := dbeta((N - min(N))/(max(N) - min(N)),
                   1, 2)]
ppl[, Nwt := Nwt/sum(Nwt)]

d <- expand.grid(
  K = obs$K,
  N = ppl$N
)
d <- merge(d, obs, by = "K")
d <- merge(d, ppl, by = "N")
d <- as.data.table(d)
d[, wt := Kwt*Nwt]

set.seed(123) # set different for each script
sampledd <- d[sample(seq_len(.N), size = 10, replace = TRUE, prob = wt)]

ggplot(sampledd, aes(x = N, y = K)) + geom_density_2d_filled()

out <- list()
system.time(
  for (i in seq_len(nrow(sampledd))) {
    
    N = sampledd[i]$N
    K = sampledd[i]$K
    
    useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
    dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), K, replace = FALSE)], by = ID]
    
    out[[i]] <- simmodel(dat, sbp, N, K)
  })
# system time
# user   system  elapsed 
# 1266.378   21.395  612.166 

# foreach with weighted distribution (only 1 loop) - perf # ----------
obs <- data.table(K = 3:28)
obs[, Kwt := dbeta((K - min(K))/(max(K) - min(K)), 
                   1, 2)]
obs[, Kwt := Kwt/sum(Kwt)]

ppl <- data.table(N = c(10:1000))
ppl[, Nwt := dbeta((N - min(N))/(max(N) - min(N)),
                   1, 2)]
ppl[, Nwt := Nwt/sum(Nwt)]

d <- expand.grid(
  K = obs$K,
  N = ppl$N
)
d <- merge(d, obs, by = "K")
d <- merge(d, ppl, by = "N")
d <- as.data.table(d)
d[, wt := Kwt*Nwt]

set.seed(123) 
sampledd <- d[sample(seq_len(.N), size = 10, replace = TRUE, prob = wt)]

registerDoFuture()
plan(list(
  tweak(multisession, workers = 8L),
  tweak(sequential)
))

out <- foreach (i = seq_len(nrow(sampledd)), 
                .combine = c) %dorng% {
                  
                  N = sampledd[i]$N
                  K = sampledd[i]$K
                  
                  useIDs <- sample(unique(synd$ID), size = N, replace = FALSE)
                  dat <- synd[ID %in% useIDs, .SD[sample(seq_len(.N), K, replace = FALSE)], by = ID]
                  
                  list(simmodel(dat, sbp, N, K))
                }
# system time
# user  system elapsed 
# 44.354   1.410 496.234 