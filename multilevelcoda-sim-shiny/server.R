# source("simsum_in.R")

server <- function(input, output) {
  
  # Summary Statistics -------------------
  ## brmcoda tab ---------------
  output$simsum_brmcoda_table <- DT::renderDataTable(DT::datatable({
    if (input$par_brmcoda == "All") {
      par_brmcoda <- levels(brmcoda_tab$by)
    } else if (input$par_brmcoda == "b_Intercept") {
      par_brmcoda  <- "  b0"
    } else if (input$par_brmcoda == "b_bilr1") {
      par_brmcoda <- "between ilr1 beta"
    } else if (input$par_brmcoda == "b_bilr2") {
      par_brmcoda <- "between ilr2 beta"
    } else if (input$par_brmcoda == "b_bilr3") {
      par_brmcoda <- "between ilr3 beta"
    } else if (input$par_brmcoda == "b_bilr4") {
      par_brmcoda <- "between ilr4 beta"
    } else if (input$par_brmcoda == "b_wilr1") {
      par_brmcoda <- "within ilr1 beta"
    } else if (input$par_brmcoda == "b_wilr2") {
      par_brmcoda <- "within ilr2 beta"
    } else if (input$par_brmcoda == "b_wilr3") {
      par_brmcoda <- "within ilr3 beta"
    } else if (input$par_brmcoda == "b_wilr4") {
      par_brmcoda <- "within ilr4 beta"
    } else if (input$par_brmcoda == "sd_ID_Intercept") {
      par_brmcoda <- "  u0"
    } else if (input$par_brmcoda == "sigma") {
      par_brmcoda <- "  sigma"
    }
    if (input$rint_sd_brmcoda == "medium" & input$res_sd1_brmcoda == "medium") {
      brmcoda_tab <- brmcoda_tab[condition == "base"]
    } else if (input$rint_sd_brmcoda == "medium" & input$res_sd1_brmcoda == "small") {
      brmcoda_tab <- brmcoda_tab[condition == "REbase_RESsmall"]
    } else if (input$rint_sd_brmcoda == "medium" & input$res_sd1_brmcoda == "large") {
      brmcoda_tab <- brmcoda_tab[condition == "REbase_RESlarge"]
    } else if (input$rint_sd_brmcoda == "small" & input$res_sd2_brmcoda == "large") {
      brmcoda_tab <- brmcoda_tab[condition == "REsmall_RESlarge"]
    } else if (input$rint_sd_brmcoda == "large" & input$res_sd3_brmcoda == "small") {
      brmcoda_tab <- brmcoda_tab[condition == "RElarge_RESsmall"]
    }
    if (input$N_brmcoda != "All") {
      brmcoda_tab <- brmcoda_tab[N == input$N_brmcoda]
    }
    if (input$K_brmcoda != "All") {
      brmcoda_tab <- brmcoda_tab[K == input$K_brmcoda]
    }
    if (input$D_brmcoda != "All") {
      brmcoda_tab <- brmcoda_tab[D == input$D_brmcoda]
    }
    
    brmcoda_tab <- brmcoda_tab[by %in% par_brmcoda & stat == input$stat_brmcoda, ]
    
    brmcoda_tab[] <- lapply(brmcoda_tab, function(x) if(is.numeric(x)) round(x, 2) else x)
    
    brmcoda_tab
  }))
  
  ## substitution tab ------------
  output$simsum_sub_table <- DT::renderDataTable(DT::datatable({
    
    if (input$rint_sd_sub == "medium" & input$res_sd1_sub == "medium") {
      sub_tab <- sub_tab[condition == "base"]
    } else if (input$rint_sd_sub == "medium" & input$res_sd1_sub == "small") {
      sub_tab <- sub_tab[condition == "REbase_RESsmall"]
    } else if (input$rint_sd_sub == "medium" & input$res_sd1_sub == "large") {
      sub_tab <- sub_tab[condition == "REbase_RESlarge"]
    } else if (input$rint_sd_sub == "small" & input$res_sd2_sub == "large") {
      sub_tab <- sub_tab[condition == "REsmall_RESlarge"]
    } else if (input$rint_sd_sub == "large" & input$res_sd3_sub == "small") {
      sub_tab <- sub_tab[condition == "RElarge_RESsmall"]
    }
    if (input$D_sub == 3) {
      sub_tab <- sub_tab[To == input$delta3_sub]
    } else if (input$D_sub == 4) {
      sub_tab <- sub_tab[To == input$delta4_sub]
    } else if (input$D_sub == 5) {
      sub_tab <- sub_tab[To == input$delta5_sub]
    }
    if (input$level_sub != "all") {
      sub_tab <- sub_tab[Level == input$level_sub]
    }
    if (input$N_sub != "All") {
      sub_tab <- sub_tab[N == input$N_sub]
    }
    if (input$K_sub != "All") {
      sub_tab <- sub_tab[K == input$K_sub]
    }
    if (input$D_sub != "All") {
      sub_tab <- sub_tab[D == input$D_sub]
    }
    sub_tab <- sub_tab[stat == input$stat_sub, ]
    
    sub_tab[] <- lapply(sub_tab, function(x) if(is.numeric(x)) round(x, 2) else x)
    
    sub_tab
  }))
  
  # Summary Plots -------------------
  ## brmcoda plot ---------------
  output$simsum_brmcoda_plot <- renderPlot({

    if (input$rint_sd_brmcoda_plot == "medium" & input$res_sd1_brmcoda_plot == "medium") {
      if (input$D_brmcoda_plot == 3) {
        .par_plot_shiny(brmcoda_d3[stat == input$stat_brmcoda_plot & condition == "base"])
      } else if (input$D_brmcoda_plot == 4) {
        .par_plot_shiny(brmcoda_d4[stat == input$stat_brmcoda_plot & condition == "base"])
      } else if (input$D_brmcoda_plot == 5) {
        .par_plot_shiny(brmcoda_d5[stat == input$stat_brmcoda_plot & condition == "base"])
      }
    } else if (input$rint_sd_brmcoda_plot == "medium" & input$res_sd1_brmcoda_plot == "small") {
      if (input$D_brmcoda_plot == 3) {
        .par_plot_shiny(brmcoda_d3[stat == input$stat_brmcoda_plot & condition == "REbase_RESsmall"])
      } else if (input$D_brmcoda_plot == 4) {
        .par_plot_shiny(brmcoda_d4[stat == input$stat_brmcoda_plot & condition == "REbase_RESsmall"])
      } else if (input$D_brmcoda_plot == 5) {
        .par_plot_shiny(brmcoda_d5[stat == input$stat_brmcoda_plot & condition == "REbase_RESsmall"])
      }
    } else if (input$rint_sd_brmcoda_plot == "medium" & input$res_sd1_brmcoda_plot == "large") {
      if (input$D_brmcoda_plot == 3) {
        .par_plot_shiny(brmcoda_d3[stat == input$stat_brmcoda_plot & condition == "REbase_RESlarge"])
      } else if (input$D_brmcoda_plot == 4) {
        .par_plot_shiny(brmcoda_d4[stat == input$stat_brmcoda_plot & condition == "REbase_RESlarge"])
      } else if (input$D_brmcoda_plot == 5) {
        .par_plot_shiny(brmcoda_d5[stat == input$stat_brmcoda_plot & condition == "REbase_RESlarge"])
      }
    } else if (input$rint_sd_brmcoda_plot == "small" & input$res_sd2_brmcoda_plot == "large") {
      if (input$D_brmcoda_plot == 3) {
        .par_plot_shiny(brmcoda_d3[stat == input$stat_brmcoda_plot & condition == "REsmall_RESlarge"])
      } else if (input$D_brmcoda_plot == 4) {
        .par_plot_shiny(brmcoda_d4[stat == input$stat_brmcoda_plot & condition == "REsmall_RESlarge"]) 
      } else if (input$D_brmcoda_plot == 5) {
        .par_plot_shiny(brmcoda_d5[stat == input$stat_brmcoda_plot & condition == "REsmall_RESlarge"])
      }
    } else if (input$rint_sd_brmcoda_plot == "large" & input$res_sd3_brmcoda_plot == "small") {
      if (input$D_brmcoda_plot == 3) {
        .par_plot_shiny(brmcoda_d3[stat == input$stat_brmcoda_plot & condition == "RElarge_RESsmall"])
      } else if (input$D_brmcoda_plot == 4) {
        .par_plot_shiny(brmcoda_d4[stat == input$stat_brmcoda_plot & condition == "RElarge_RESsmall"])
      } else if (input$D_brmcoda_plot == 5) {
        .par_plot_shiny(brmcoda_d5[stat == input$stat_brmcoda_plot & condition == "RElarge_RESsmall"])
      }
    }

  }, height = 1000, width = 800)
  
  ## substitution plot ---------------
  output$simsum_sub_plot <- renderPlot({
    
    if (input$rint_sd_sub_plot == "medium" & input$res_sd1_sub_plot == "medium") {
      if (input$D_sub_plot == 3) {
        .par_plot_shiny(sub_d3[stat == input$stat_sub_plot & condition == "base"])
      } else if (input$D_sub_plot == 4) {
        .par_plot_shiny(sub_d4[stat == input$stat_sub_plot & condition == "base"])
      } else if (input$D_sub_plot == 5) {
        .par_plot_shiny(sub_d5[stat == input$stat_sub_plot & condition == "base"])
      }
    } else if (input$rint_sd_sub_plot == "medium" & input$res_sd1_sub_plot == "small") {
      if (input$D_sub_plot == 3) {
        .par_plot_shiny(sub_d3[stat == input$stat_sub_plot & condition == "REbase_RESsmall"])
      } else if (input$D_sub_plot == 4) {
        .par_plot_shiny(sub_d4[stat == input$stat_sub_plot & condition == "REbase_RESsmall"])
      } else if (input$D_sub_plot == 5) {
        .par_plot_shiny(sub_d5[stat == input$stat_sub_plot & condition == "REbase_RESsmall"])
      }
    } else if (input$rint_sd_sub_plot == "medium" & input$res_sd1_sub_plot == "large") {
      if (input$D_sub_plot == 3) {
        .par_plot_shiny(sub_d3[stat == input$stat_sub_plot & condition == "REbase_RESlarge"])
      } else if (input$D_sub_plot == 4) {
        .par_plot_shiny(sub_d4[stat == input$stat_sub_plot & condition == "REbase_RESlarge"])
      } else if (input$D_sub_plot == 5) {
        .par_plot_shiny(sub_d5[stat == input$stat_sub_plot & condition == "REbase_RESlarge"])
      }
    } else if (input$rint_sd_sub_plot == "small" & input$res_sd2_sub_plot == "large") {
      if (input$D_sub_plot == 3) {
        .par_plot_shiny(sub_d3[stat == input$stat_sub_plot & condition == "REsmall_RESlarge"])
      } else if (input$D_sub_plot == 4) {
        .par_plot_shiny(sub_d4[stat == input$stat_sub_plot & condition == "REsmall_RESlarge"]) 
      } else if (input$D_sub_plot == 5) {
        .par_plot_shiny(sub_d5[stat == input$stat_sub_plot & condition == "REsmall_RESlarge"])
      }
    } else if (input$rint_sd_sub_plot == "large" & input$res_sd3_sub_plot == "small") {
      if (input$D_sub_plot == 3) {
        .par_plot_shiny(sub_d3[stat == input$stat_sub_plot & condition == "RElarge_RESsmall"])
      } else if (input$D_sub_plot == 4) {
        .par_plot_shiny(sub_d4[stat == input$stat_sub_plot & condition == "RElarge_RESsmall"])
      } else if (input$D_sub_plot == 5) {
        .par_plot_shiny(sub_d5[stat == input$stat_sub_plot & condition == "RElarge_RESsmall"])
      }
    }
    
  }, height = 1000, width = 800)
}
