# main model
m <- lmer(depression ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
            (1 + wilr2 | ID),
          data = tmp, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))

summary(m)

## m2 <- lm(depression ~ 1 + bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4,
##          data = tmp)
## summary(m2)

# lmer substitution model --------------------------------------------------------------------------
ID <- 1
psub <- possub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
min <- 60

# mean composition
b <- cilr$BetweenComp
mcomp <- mean(b, robust = TRUE)
mcomp <- clo(mcomp, total = cilr$total)
mcomp <- as.data.table(t(mcomp))

# model for no change
bilr <- ilr(mcomp, V = cilr$psi)
bilr <- as.data.table(t(bilr))
wilr <- as.data.table(matrix(0, nrow = nrow(bilr), ncol = ncol(bilr)))
colnames(wilr) <- paste0("wilr", seq_len(ncol(wilr)))
colnames(bilr) <- paste0("bilr", seq_len(ncol(bilr)))

dsame <- cbind(bilr, wilr)
ysame <- predict(m, newdata = dsame, re.form = NA)

## between-person substitution model ---------------------------------------------------------------
bout <- foreach(i = colnames(psub), .combine = c) %dopar% {
  
  posub <- as.data.table(psub)
  posub <- posub[(get(i) != 0)]
  posub <- posub[order(-rank(get(i)))]
  
  # substitution variable names
  subvar <- colnames(posub) %snin% eval(i)
  iv <- i
  
  kout <- vector("list", length = nrow(posub))
  jout <- vector("list", length = min)
  
  for (j in seq_len(min)) { # time level
    sub <- posub * j
    for (k in seq_len(nrow(sub))) {
      newcomp <- mcomp + sub[k, ]
      names(newcomp) <- cilr$parts
      MinSubstituted <- sub[k, get(i)]
      kout[[k]] <- cbind(mcomp, newcomp, MinSubstituted)
    }
    jout[[j]] <- do.call(rbind, kout)
  }
  newd <- setDT(do.call(rbind, jout))
  
  # useful information for the final results
  newd[, Substitute := rep(subvar, length.out = nrow(newd))]
  newd$Predictor <- iv
  newd$MinSubstituted <- as.numeric(newd$MinSubstituted)
  
  # remove impossible reallocation that result in negative values 
  cols <- colnames(newd) %snin% c("MinSubstituted", "Substitute", "Predictor")
  newd <- newd[rowSums(newd[, ..cols] < 0) == 0]
  
  # compositions and ilrs for predictions
  bcomp <- acomp(newd[, colnames(cilr$BetweenComp), with = FALSE])
  tcomp <- acomp(newd[, cilr$parts, with = FALSE])
  bilr <- ilr(bcomp, V = cilr$psi)
  tilr <- ilr(tcomp, V = cilr$psi)
  
  wilr <- matrix(0, nrow = nrow(tilr), ncol = ncol(tilr))
  wilr <- as.data.table(wilr)
  
  colnames(tilr) <- paste0("bilr", seq_len(ncol(tilr)))
  colnames(wilr) <- paste0("wilr", seq_len(ncol(wilr)))
  
  # prediction
    subd <- cbind(newd, tilr, wilr, ID)
    ysub <- predict(m, newdata = subd, re.form = NA)
  
  # difference
    ydiff <- ysub - ysame
    ydiff <- as.data.table(ydiff)
    setnames(ydiff, "ydiff", "Mean")
    
    # final results for entire composition
    ydiff <- cbind(ydiff, newd)
    
    out <- list(ydiff)
    names(out) <- i
    out
  }
bout

## within-person substitution model ----------------------------------------------------------------
wout <- foreach(i = colnames(psub), .combine = c) %dopar% {
  
  posub <- as.data.table(psub)
  posub <- posub[(get(i) != 0)]
  posub <- posub[order(-rank(get(i)))]
  
  # substitution variable names
  subvar <- colnames(posub) %snin% eval(i)
  iv <- i
  
  kout <- vector("list", length = nrow(posub))
  jout <- vector("list", length = min)
  
  for (j in seq_len(min)) { # time level
    sub <- posub * j
    for (k in seq_len(nrow(sub))) {
      newcomp <- mcomp + sub[k, ]
      names(newcomp) <- cilr$parts
      MinSubstituted <- sub[k, get(i)]
      kout[[k]] <- cbind(mcomp, newcomp, MinSubstituted)
    }
    jout[[j]] <- do.call(rbind, kout)
  }
  newd <- setDT(do.call(rbind, jout))
  
  # useful information for the final results
  newd[, Substitute := rep(subvar, length.out = nrow(newd))]
  newd$Predictor <- iv
  newd$MinSubstituted <- as.numeric(newd$MinSubstituted)
  
  # remove impossible reallocation that result in negative values 
  cols <- colnames(newd) %snin% c("MinSubstituted", "Substitute", "Predictor")
  newd <- newd[rowSums(newd[, ..cols] < 0) == 0]
  
  # compositions and ilrs for predictions
  bcomp <- acomp(newd[, colnames(cilr$BetweenComp), with = FALSE])
  tcomp <- acomp(newd[, cilr$parts, with = FALSE])
  wcomp <- tcomp - bcomp 
  
  bilr <- ilr(bcomp, V = cilr$psi)
  wilr <- ilr(wcomp, V = cilr$psi)

  colnames(bilr) <- paste0("bilr", seq_len(ncol(bilr)))
  colnames(wilr) <- paste0("wilr", seq_len(ncol(wilr)))
  
  # prediction
  subd <- cbind(newd, bilr, wilr, ID)
  ysub <- predict(m, newdata = subd, re.form = NA)
  
  # difference
  ydiff <- ysub - ysame
  ydiff <- as.data.table(ydiff)
  setnames(ydiff, "ydiff", "Mean")
  
  # final results for entire composition
  ydiff <- cbind(ydiff, newd)
  
  out <- list(ydiff)
  names(out) <- i
  out
}
wout
## add minute substituted to the model

# check
ggplot(wout$TST, aes(x = MinSubstituted, y = Mean)) +
  geom_hline(yintercept = 0, size = 0.2, linetype = 2) +
  geom_vline(xintercept = 0, size = 0.2, linetype = 2) +
  geom_line(aes(color = Substitute), size = 1) +
  scale_color_jco() +
  facet_grid(~ Substitute) +
  xlab("Change in Within-person Sleep (mins)") +
  ylab("Change in Depression") +
  theme_classic()

ggplot(bout$TST, aes(x = MinSubstituted, y = Mean)) +
  geom_hline(yintercept = 0, size = 0.2, linetype = 2) +
  geom_vline(xintercept = 0, size = 0.2, linetype = 2) +
  geom_line(aes(color = Substitute), size = 1) +
  scale_color_jco() +
  facet_grid(~ Substitute) +
  xlab("Change in Between-person Sleep (mins)") +
  ylab("Change in Depression") +
  theme_classic()


# multilevelcoda model -----------------------------------------------------------------------------

simmodel <- function(data, sbp) {
  
  psub <- possub(c("TST", "WAKE", "MVPA", "LPA", "SB"))
  parts <- colnames(psub)
  
  cilr <- compilr(data, sbp, parts, total = 1440, idvar = "ID")
  
  model <- brmcoda(cilr,
                   depression ~ bilr1 + bilr2 + bilr3 + bilr4 + wilr1 + wilr2 + wilr3 + wilr4 +
                     (1 + wilr2 | ID), cores = 10, chains = 8, iter = 4000, warmup = 500,
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
    wilr4_CIHigh = summary(model$Model)$fixed[9, 4]

  )
  
  bsubm <- bsub(model, substitute = psub)
  wsubm <- wsub(model, substitute = psub)
  
  out <- list(
    CompILR = cilr,
    Result = modelout,
    BetweenResult = bsubm,
    WithinResult = wsubm
  )
}

synd$depression <- tmp$depression
test <- simmodel(data = synd[ID %in% 1:10, .SD[1:3], by = ID], sbp = sbp)

# testing ------------------------------------------------------------------------------------------
registerDoFuture()
plan(multisession, workers = 20)
foreach (N = 10:10000, 
         k = 3:200, .combine = c) %dopar% {
           
           dat <- synd[ID %in% 1:N, .SD[1:k], by = ID] # check to get random ID and obs
           out[[N]] <- simmodel(dat, sbp = sbp)
           }

registerDoFuture()
plan(multisession, workers = 8)
out <- foreach (N = 10:20,
                k = 3:200, .combine = c) %dopar% {
                  dat <- synd[ID %in% 1:N, .SD[1:k], by = ID] # check to get random ID and obs
                  mod <- simmodel(dat, sbp = sbp)
                  
                  out[[N]] <- list(mod)
                }
