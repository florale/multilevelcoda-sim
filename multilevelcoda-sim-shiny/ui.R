# source("simsum_tidy.R")

ui <- fluidPage(
  theme = shinytheme("yeti"),
  
  #Navbar structure for UI
  navbarPage(
    "multilevelcoda Simulation Study",
    
    navbarMenu("Simulation Summary Statistics",
               tabPanel("Bayesian compositional multilevel model", fluid = TRUE,
                        
                        fluidRow(
                          column(12,
                                 selectInput("stat",
                                             "Performance Measure:",
                                             c("Bias" = "bias",
                                               "Bias-Eliminated Coverage" = "becover"
                                             ), width = "100%"))),
                        
                        titlePanel("Simulation Condition"),
                        
                        sidebarLayout(sidebarPanel(
                          selectInput("N",
                                      "Number of participants:",
                                      c("All",
                                        unique(as.character(brmcoda_dat$N)))),
                          selectInput("K",
                                      "Number of participants:",
                                      c("All",
                                        unique(as.character(brmcoda_dat$K)))),
                          selectInput("D",
                                      "Number of compositional parts:",
                                      c("All",
                                        unique(as.character(brmcoda_dat$D)))),
                          selectInput("rint_sd",
                                      "Random Intercept variance:",
                                      c("medium", "small", "large")),
                          conditionalPanel(condition = "input.rint_sd == 'medium'",
                                           selectInput("res_sd",
                                                       "Residual variance:",
                                                       c("medium", "small", "large"))),
                          conditionalPanel(condition = "input.rint_sd == 'small'",
                                           selectInput("res_sd",
                                                       "Residual variance:",
                                                       c("large"))),
                          conditionalPanel(condition = "input.rint_sd == 'large'",
                                           selectInput("res_sd",
                                                       "Residual variance:",
                                                       c("small"))),
                          # selectInput("res_sd",
                          #             "Residual variance:",
                          #             c("medium", "small", "large"))
                        ), 
                        mainPanel(
                          fluidRow(
                            column(12,
                                   selectInput("Parameter",
                                               "Parameter:",
                                               c("All",
                                                 "b_Intercept",
                                                 "b_bilr1",
                                                 "b_bilr2",
                                                 "b_bilr3",
                                                 "b_bilr4",
                                                 "b_wilr1",
                                                 "b_wilr2",
                                                 "b_wilr3",
                                                 "b_wilr4",
                                                 "sd_ID_Intercept",
                                                 "sigma"
                                               ), width = "100%"))),
                          DT::dataTableOutput("simsum_brmcoda_table"))
                        )),
               
               tabPanel("Bayesian compositional multilevel substitution model", fluid = TRUE,
                        
                        fluidRow(
                          column(12,
                                 selectInput("stat",
                                             "Performance Measure:",
                                             c("Bias" = "bias",
                                               "Bias-Eliminated Coverage" = "becover"
                                             ), width = "100%"))),
                        
                        titlePanel("Simulation Condition"),
                        
                        sidebarLayout(sidebarPanel(
                          selectInput("N",
                                      "Number of participants:",
                                      c("All",
                                        unique(as.character(brmcoda_dat$N)))),
                          selectInput("K",
                                      "Number of participants:",
                                      c("All",
                                        unique(as.character(brmcoda_dat$K)))),
                          selectInput("D",
                                      "Number of compositional parts:",
                                      c("All",
                                        unique(as.character(brmcoda_dat$D)))),
                          selectInput("rint_sd",
                                      "Random Intercept variance:",
                                      c("medium", "small", "large")),
                          selectInput("res_sd",
                                      "Residual variance:",
                                      c("medium", "small", "large")),
                          selectInput("delta",
                                      "Reallocation:",
                                      c("All")), 
                          conditionalPanel(condition = "input.D == '3'",
                                           selectInput("delta",
                                                       "Reallocation:",
                                                       c(
                                                         "between Sleep - PA", "within Sleep - PA",
                                                         "between Sleep - SB", "within Sleep - SB",
                                                         
                                                         "between PA - SB",    "within PA - SB"
                                                       ), width = "100%")),
                          conditionalPanel(condition = "input.D == '4'",
                                           selectInput("delta",
                                                       "Reallocation:",
                                                       c(
                                                         "between Sleep - MVPA", "within Sleep - MVPA",
                                                         "between Sleep - LPA",  "within Sleep - LPA",
                                                         "between Sleep - SB",   "within Sleep - SB",
                                                         
                                                         "between MVPA - LPA",   "within MVPA - LPA",
                                                         "between MVPA - SB",    "within MVPA - SB",
                                                         
                                                         "between LPA - SB",     "within LPA - SB"
                                                       ), width = "100%")),
                          conditionalPanel(condition = "input.D == '5'",
                                           selectInput("delta",
                                                       "Reallocation:",
                                                       c(
                                                         "between TST - MVPA",  "within TST - MVPA",
                                                         "between TST - WAKE",  "within TST - WAKE",
                                                         "between TST - LPA",   "within TST - LPA",
                                                         "between TST - SB",    "within TST - SB",
                                                         
                                                         "between WAKE - MVPA", "within WAKE - MVPA",
                                                         "between WAKE - LPA",  "within WAKE - LPA",
                                                         "between WAKE - SB",   "within WAKE - SB",
                                                         
                                                         "between MVPA - LPA",  "within MVPA - LPA",
                                                         "between MVPA - SB",   "within MVPA - SB",
                                                         
                                                         "between LPA - SB",     "within LPA - SB"
                                                       ), width = "100%"))
                        )), 
                        mainPanel(
                          DT::dataTableOutput("simsum_sub_table"))
                        )
    ),
  ))
