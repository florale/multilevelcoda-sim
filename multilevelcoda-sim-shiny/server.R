# source("plots_par.R")
# source("simsum_tidy.R")

server <- function(input, output) {
  
  ## Posterior Summary Statistics
  ## brmcoda()
  output$simsum_brmcoda_table <- DT::renderDataTable(DT::datatable({
    if (input$Parameter == "All") {
      by <- levels(brmcoda_dat$Parameter)
    } else if (input$Parameter == "b_Intercept") {
      by  <- "  b0"
    } else if (input$Parameter == "b_bilr1") {
      by <- "between ilr1 beta"
    } else if (input$Parameter == "b_bilr2") {
      by <- "between ilr2 beta"
    } else if (input$Parameter == "b_bilr3") {
      by <- "between ilr3 beta"
    } else if (input$Parameter == "b_bilr4") {
      by <- "between ilr4 beta"
    } else if (input$Parameter == "b_wilr1") {
      by <- "within ilr1 beta"
    } else if (input$Parameter == "b_wilr2") {
      by <- "within ilr2 beta"
    } else if (input$Parameter == "b_wilr3") {
      by <- "within ilr3 beta"
    } else if (input$Parameter == "b_wilr4") {
      by <- "within ilr4 beta"
    } else if (input$Parameter == "sd_ID_Intercept") {
      by <- "  u0"
    } else if (input$Parameter == "sigma") {
      by <- "  sigma"
    }
    

    if (input$rint_sd == "medium" & input$res_sd == "medium") {
      u0_sd <- 1
      e_sd <- 1
    } else if (input$rint_sd == "medium" & input$res_sd == "small") {
      u0_sd <- 1
      e_sd <- sqrt(0.5)
    } else if (input$rint_sd == "medium" & input$res_sd == "large") {
      u0_sd <- 1
      e_sd <- sqrt(1.5)
    } else if (input$rint_sd == "small" & input$res_sd == "large") {
      u0_sd <- sqrt(0.5)
      e_sd <- sqrt(1.5)
    } else if (input$rint_sd == "large" & input$res_sd == "small") {
      u0_sd <- sqrt(1.5)
      e_sd <- sqrt(0.5)
    }
    
    if (input$N != "All") {
      brmcoda_dat <- brmcoda_dat[N == input$N]
    }
    if (input$K != "All") {
      brmcoda_dat <- brmcoda_dat[K == input$K]
    }
    if (input$D != "All") {
      brmcoda_dat <- brmcoda_dat[D == input$D]
    }
    
    brmcoda_dat <- brmcoda_dat[rint_sd == u0_sd &
                                 res_sd  == e_sd &
                                 Parameter %in% by &
                                 stat  == input$stat,
                               .(stat, est, mcse, upper, lower,
                                 Parameter, N, K, D, rint_sd, res_sd)]
    
    brmcoda_dat[] <- lapply(brmcoda_dat, function(x) if(is.numeric(x)) round(x, 2) else x)
    
    brmcoda_dat
  }))
  
  ## substitution
  output$simsum_sub_table <- DT::renderDataTable(DT::datatable({
    
    if (input$rint_sd == "medium" & input$res_sd == "medium") {
      u0_sd <- 1
      e_sd <- 1
    } else if (input$rint_sd == "medium" & input$res_sd == "small") {
      u0_sd <- 1
      e_sd <- sqrt(0.5)
    } else if (input$rint_sd == "medium" & input$res_sd == "large") {
      u0_sd <- 1
      e_sd <- sqrt(1.5)
    } else if (input$rint_sd == "small" & input$res_sd == "large") {
      u0_sd <- sqrt(0.5)
      e_sd <- sqrt(1.5)
    } else if (input$rint_sd == "large" & input$res_sd == "small") {
      u0_sd <- sqrt(1.5)
      e_sd <- sqrt(0.5)
    }

    if (input$N != "All") {
      sub_dat <- sub_dat[N == input$N]
    }
    if (input$K != "All") {
      sub_dat <- sub_dat[K == input$K]
    }
    if (input$D != "All") {
      sub_dat <- sub_dat[D == input$D]
    }

    sub_dat <- sub_dat[rint_sd == u0_sd &
                         res_sd  == e_sd &
                         Reallocation %in% delta &
                         stat  == input$stat,
                       .(stat, est, mcse, upper, lower,
                         Reallocation, N, K, D, rint_sd, res_sd)]
    
    sub_dat[] <- lapply(sub_dat, function(x) if(is.numeric(x)) round(x, 2) else x)
    
    sub_dat
  }))
}
