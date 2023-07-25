
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
    scale_colour_manual(values = col) + 
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
.par_plot <- function(data, shiny = FALSE, d = 4, font = "Arial Narrow") {
  
  if (all(data$Stat == "bias")) {
    ylab <- "Bias"
    yintercept <- 0
    if ("Estimand" %in% colnames(data)) {
      y_lims <- c(-0.16, 0.16)
      y_breaks <- c(-0.1, 0, 0.1)
    } else {
      y_lims <- c(-0.075, 0.075)
      y_breaks <- c(-0.05, 0, 0.05)
    }
  } else if (all(data$Stat == "cover")) {
    ylab <- "Coverage"
    yintercept <- 0.95
    if ("Estimand" %in% colnames(data)) {
      y_lims <- c(0.9, 1)
      y_breaks <- c(0.9, 0.95, 1)
    } else {
      y_lims <- c(0.9, 1)
      y_breaks <- c(0.9, 0.95, 1)
    }
  } else if (all(data$Stat == "becover")) {
    ylab <- "Bias-Eliminated Coverage"
    yintercept <- 0.95
    if ("Estimand" %in% colnames(data)) {
      y_lims <- c(0.9, 1)
      y_breaks <- c(0.9, 0.95, 1)
    } else {
      y_lims <- c(0.9, 1)
      y_breaks <- c(0.9, 0.95, 1)
    }
  } else if (all(data$Stat == "mse")) {
    ylab <- "Empirical Standard Error"
    yintercept <- 0
    if ("Estimand" %in% colnames(data)) {
      y_lims <- c(0, 3.5)
      y_breaks <- c(0, 1.5, 3)
    } else {
      y_lims <- c(0, 1)
      y_breaks <- c(0, 0.5, 1)
    }
  } else if (all(data$Stat == "empse")) {
    ylab <- "Mean-squared Error"
    yintercept <- 0
    if ("Estimand" %in% colnames(data)) {
      y_lims <- c(0, 3)
      y_breaks <- c(0, 1.5, 3)
    } else {
      y_lims <- c(0, 1)
      y_breaks <- c(0, 0.5, 1)
    }
  }
  
  if (d == 4) {
    if ("Substitution" %in% colnames(data)) {
      xvar <- data$Substitution
      xtext <- 13
    } else {
      xvar <- data$Estimand
      xtext <- 10
    }
  } else if (d == 3) {
    if ("Substitution" %in% colnames(data)) {
      xvar <- data$Substitution
      xtext <- 7
    } else {
      xvar <- data$Estimand
      xtext <- 8
    }
  } else if (d == 5) {
    if ("Substitution" %in% colnames(data)) {
      xvar <- data$Substitution
      xtext <- 21
    } else {
      xvar <- data$Estimand
      xtext <- 12
    }
  }
  
  if (nlevels(xvar) == 7) {
    col <- col_brmcoda_d3
  } else if (nlevels(xvar) == 9) {
    col <- col_brmcoda_d4
  } else if (nlevels(xvar) == 11) {
    col <- col_brmcoda_d5
  } else if (nlevels(xvar) == 6) {
    col <- col_sub_d3
  } else if (nlevels(xvar) == 12) {
    col <- col_sub_d4
  } else if (nlevels(xvar) == 20) {
    col <- col_sub_d5
  }
  
  point_size <- ifelse(shiny == TRUE, 2, 2.25)
  line_size <- ifelse(shiny == TRUE, 0.75, 0.75)
  btext_size <- ifelse(shiny == TRUE, 14, 12)
  text_size <- ifelse(shiny == TRUE, 12, 13)
  yseg <- y_breaks[[1]]
  yendseg <- y_breaks[[3]]
  
  if (shiny == TRUE) {
  gg <- 
    ggplot(data, 
           aes(x = xvar, y = est, 
               ymin = lower, ymax = upper,
               colour = xvar)) +
    geom_hline(yintercept = yintercept, color = "#666666", linetype = "dashed", linewidth = 0.5) +
    geom_point(size = point_size) +
    geom_linerange(linewidth = line_size) +
    labs(x = "", y = ylab, colour = "Parameter") +
    scale_colour_manual(values = col) +
    scale_y_continuous(limits = y_lims,
                       breaks = y_breaks) +
    scale_x_discrete(drop = FALSE) +
    # facet_wrap(ggplot2::vars(N, K), labeller = ggplot2::label_both) +
    facet_wrap(ggplot2::vars(NK), labeller = ggplot2::label_context, strip.position = "top") +
    hrbrthemes::theme_ipsum() +
    coord_flip() +
    theme(
      axis.ticks        = element_blank(),
      panel.background  = element_rect(fill = "transparent", colour = "black", linewidth = line_size),
      panel.border      = element_rect(fill = "transparent", colour = "black", linewidth = line_size),
      # panel.grid.major  = element_blank(),
      # panel.grid.minor  = element_blank(),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      axis.title.y      = element_text(size = btext_size, face = "bold"),
      axis.title.x      = element_text(size = btext_size, face = "bold"),
      axis.text.x       = element_text(size = text_size),
      axis.text.y       = element_blank(),
      title             = element_text(size = btext_size, face = "bold"),
      legend.text       = element_text(size = text_size),
      strip.text.x      = element_text(size = text_size),
      legend.position   = "none",
      panel.spacing.y   = unit(0, "lines"),
      panel.spacing.x   = unit(0.75, "lines")
      # strip.text.x      = element_blank()
    )
    plotly::ggplotly(gg, height = 1300)
    
  } else {
    gg <- 
      ggplot(data, 
             aes(x = xvar, y = est, 
                 ymin = lower, ymax = upper,
                 colour = xvar)) +
      geom_segment(aes(x = 0.5, xend = xvar, y = yintercept, yend = yintercept), color = "#666666", linetype = "dashed", linewidth = 0.5) +
      geom_segment(aes(y = yseg, yend = yendseg, x = 0.5, xend = 0.5), color = "black", linewidth = 0.5) +
      geom_text(aes(label = NK, y = yintercept, x = xtext), color = "black", family = font, vjust = "inward", hjust = "inward") +
      # geom_hline(yintercept = yintercept, color = "#666666", linetype = "dashed", linewidth = 0.5) +
      geom_point(size = point_size) +
      geom_linerange(linewidth = line_size) +
      # geom_segment(aes(x = "sigma", xend = xvar, y = yintercept, yend = yintercept), color = "#666666", linetype = "dashed", linewidth = 0.25) +
      labs(x = "", y = ylab, colour = "Parameter") +
      scale_colour_manual(values = col) +
      scale_y_continuous(limits = y_lims,
                         breaks = y_breaks) +
      scale_x_discrete(drop = FALSE, expand = c(0,1.05)) +
      # facet_wrap(ggplot2::vars(N, K), labeller = ggplot2::label_both) +
      # facet_wrap(ggplot2::vars(NK), labeller = ggplot2::label_context, strip.position = "top") +
      hrbrthemes::theme_ipsum() + theme_void() +
      coord_flip() +
      theme(
        axis.ticks        = element_blank(),
        panel.background  = element_rect(fill = "transparent", colour = NA, linewidth = line_size),
        panel.border      = element_rect(fill = "transparent", colour = NA, linewidth = line_size),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        plot.background   = element_rect(fill = "transparent", colour = NA),
        axis.title.y      = element_text(size = btext_size, face = "bold"),
        # axis.title.x      = element_text(size = btext_size, face = "bold"),
        # axis.text.x       = element_text(size = text_size),
        # axis.text.y       = element_blank(),
        # title             = element_blank(),
        legend.text       = element_blank(),
        # strip.text.x      = element_text(size = text_size),
        legend.position   = "none",
        # panel.grid.major  = element_blank(),
        # panel.grid.minor  = element_blank(),
        axis.title.x      = element_blank(),
        axis.line.y       = element_blank(),
        axis.text.x       = element_text(size = text_size, family = font),
        axis.text.y       = element_blank()
        # strip.text.x      = element_text(size = text_size, family = font),
        # strip.background  = element_blank(),
        # strip.placement   = "outside"
        # strip.text.x      = element_blank()
      )
    gg
    
  }
}

