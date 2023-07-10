
## Coverage
.get_cov <- function(object, data, estvarname, true, 
                     by = c("N", "K"),
                     methodvar = "condition",
                     ci.limits = c("CI_low", "CI_high")) {
  if (isTRUE(inherits(object, what = "simsum"))) {
    
    ci_level <- 0.95
    crit <- stats::qnorm(1 - (1 - ci_level) / 2)
    ref <- "base"
    obj <- list()
    
    # object$summ$lower <- object$summ$est - crit * object$summ$mcse
    # object$summ$upper <- object$summ$est + crit * object$summ$mcse
    
    data <- .split_by(data = data, by = by)
    data <- lapply(X = seq_along(data), FUN = function(i) .split_by(data = data[[i]], by = methodvar))
    
    summ <- lapply(X = seq_along(data), FUN = function(i) {
      out.out <- lapply(X = seq_along(data[[i]]), FUN = function(j) {
        empse_ref <- sqrt(stats::var(data[[i]][[ref]][[estvarname]], na.rm = TRUE))
        
        nsim <- sum(!is.na(data[[i]][[j]][[estvarname]]), na.rm = TRUE)
        
        cover <- 1 / nsim * sum(true >= data[[i]][[j]][[ci.limits[1]]] & true <= data[[i]][[j]][[ci.limits[2]]], na.rm = TRUE)
        becover <- 1 / nsim * sum(mean(data[[i]][[j]][[estvarname]], na.rm = TRUE) >= data[[i]][[j]][[ci.limits[1]]] & mean(data[[i]][[j]][[estvarname]], na.rm = TRUE) <= data[[i]][[j]][[ci.limits[2]]], na.rm = TRUE)
        
        cover_mcse <- sqrt(cover * (1 - cover) / nsim)
        becover_mcse <- sqrt(becover * (1 - becover) / nsim)
        
        obj$stat <- c("cover", "becover")
        obj$est <- c(cover, becover)
        obj$mcse <- c(cover_mcse, becover_mcse)
        
        obj$condition <- unique(data[[i]][[j]]$condition)
        obj$N <- unique(data[[i]][[j]]$N)
        obj$K <- unique(data[[i]][[j]]$K)
        
        obj <- as.data.frame(obj, stringsAsFactors = FALSE)
        
      })
      out.out <- do.call(rbind.data.frame, out.out)
      
    })
    
    summ <- do.call(rbind.data.frame, summ)
    summ$condition <- as.factor(summ$condition)
    summ$N <- as.factor(summ$N)
    summ$K <- as.factor(summ$K)
  }
  object$summ <- rbind(object$summ, summ)
  
  object
}

# merge multiple data tables
mergeDTs <- function(dt_list, by = NULL, sort = FALSE) {
  Reduce(
    function(...) {
      merge(..., by = by, all = TRUE, sort = sort)
    }, dt_list)
}

### Heat plot
.heat_plot <- function(data, methodvar, by, stats) {
  ### Create a .dgm column
  if (!is.null(by)) {
    tmp <- lapply(by, function(x) data[[x]])
    data[[".dgm"]] <- do.call(paste, c(tmp, sep = ", "))
  } else {
    data[[".dgm"]] <- "Single DGM"
  }
  ### Build basic plot
  if (!is.null(methodvar)) {
    methodvar <- rlang::sym(methodvar)
    gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = {{methodvar}}, y = .dgm, fill = est))
  } else {
    gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = "Single Method", y = .dgm, fill = est)) +
      ggplot2::labs(x = "")
  }
  gg <- gg +
    ggplot2::geom_raster() +
    ggplot2::labs(y = "", fill = stats) +
    geom_text(data = data, aes(y = .dgm, label = round(est, digits = 2)), color = "black")
  
  ### Return plot
  return(gg)
}

### Forest plot
.forest_plot <- function(object, methodvar, by, stats, 
                         scales = "fixed") {
  
  methodvar <- object$methodvar
  by <- object$by
  
  object <- summary(object)
  
  ci <- inherits(x = object, what = "summary.simsum")
  data <- tidy(object, stats = stats)
  
    if (stats %in% c("cover", "becover", "power")) {
      target <- 0.95
    } else if (stats %in% c("thetamean", "thetamedian")) {
      target <- object[["true"]]
    } else {
      target <- 0
    }
  
  ### Build basic plot
  if (!is.null(methodvar)) {
    methodvar <- rlang::sym(methodvar)
    gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = {{ methodvar }}, y = est, colour = {{ methodvar }}))
  } else {
    gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = "Single Method", y = est, colour = "Single Method")) +
      ggplot2::labs(x = "")
  }
  gg <- gg +
    ggplot2::geom_hline(yintercept = target, linetype = "dotted") +
    ggplot2::geom_point() +
    ggplot2::labs(y = stats)
  
  ### Wrap by 'by' factors if defined
  if (!is.null(by)) {
    by <- rlang::syms(by)
    gg <- gg +
      ggplot2::facet_wrap(facets = ggplot2::vars(!!!{{ by }}), labeller = ggplot2::label_both, scales = scales) + 
      coord_flip()
  }
  
  ### Add confidence intervals if we are calling autoplot on a summary object
  if (ci) {
    gg <- gg +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 1 / 3)
  }
  
  gg <- gg +
    scale_colour_manual(values = colour) + 
    theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      legend.position   = "bottom",
      panel.background  = element_rect(
        fill = "transparent",
        colour = "black",
        linewidth = 0.5
      )
    )
  
  ### Return plot
  return(gg)
}

.split_by <- function(data, by) {
  if (!is.null(by)) {
    out <- split(x = data, f = lapply(X = by, FUN = function(f) data[[f]]), sep = "~")
  } else {
    out <- list(data)
  }
  return(out)
}


## Forest parameter plot
.par_plot <- function(data) {
  
  if(all(data$stat == "bias")) {
    ylab <- "Bias"
    yintercept <- 0
  } else if (all(data$stat == "becover")) {
    ylab <- "Coverage"
    yintercept <- 0.95
  }
  
  gg <- 
    ggplot(data, 
           aes(x = by, y = est, 
               ymin = lower, ymax = upper,
               colour = by)) +
    geom_hline(yintercept = yintercept, color = "#666666", linetype = "dotted", linewidth = 1 / 2) +
    geom_point() +
    geom_errorbar(width = 1 / 2) +
    labs(x = "", y = ylab, colour = "Parameter") +
    scale_colour_manual(values = colour) + 
    coord_flip() +
    facet_wrap(ggplot2::vars(N, K), labeller = ggplot2::label_both) +
    theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      legend.position   = "bottom",
      panel.background  = element_rect(fill = "transparent", colour = "black", linewidth = 0.5),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_text(size = 12, face = "bold"),
      axis.title.x      = element_text(size = 12, face = "bold"),
      axis.text.x       = element_text(size = 11),
      axis.text.y       = element_blank(),
      title             = element_text(size = 12, face = "bold")
    )
  
  return(gg)
  
}
## Forest parameter plotn for shiny
.par_plot_shiny <- function(data) {
  
  if(all(data$stat == "bias")) {
    ylab <- "Bias"
    yintercept <- 0
  } else if (all(data$stat == "becover")) {
    ylab <- "Coverage"
    yintercept <- 0.95
  }
  
  gg <- 
    ggplot(data, 
           aes(x = by, y = est, 
               ymin = lower, ymax = upper,
               colour = by)) +
    geom_hline(yintercept = yintercept, color = "#666666", linetype = "dotted", linewidth = 0.75) +
    geom_point() +
    geom_errorbar(width = 0.5, linewidth = 0.75) +
    labs(x = "", y = ylab, colour = "Parameter") +
    scale_colour_manual(values = colour) + 
    coord_flip() +
    facet_wrap(ggplot2::vars(N, K), labeller = ggplot2::label_both) +
    theme_ipsum() +
    theme(
      axis.ticks        = element_blank(),
      legend.position   = "bottom",
      panel.background  = element_rect(fill = "transparent", colour = "black", linewidth = 0.75),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_text(size = 14, face = "bold"),
      axis.title.x      = element_text(size = 14, face = "bold"),
      axis.text.x       = element_text(size = 12),
      axis.text.y       = element_blank(),
      title             = element_text(size = 14, face = "bold"),
      legend.text       = element_text(size = 12),
      strip.text        = element_text(size = 12)
    
    )
  
  return(gg)
  
}
