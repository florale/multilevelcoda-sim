
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
  
  ### Return plot
  return(gg)
}
