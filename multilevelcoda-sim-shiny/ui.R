ui <- fluidPage(
  theme = floras,
  tags$style(
    HTML(
      "
        @media (min-width: 768px) {
            body > div .container-fluid {
                width: 750px;
            }
        }
        @media (min-width: 992px) {
            body > div > .container-fluid {
                width: 970px;
            }
        }
        @media (min-width: 1200px) {
            body > div .container-fluid {
                width: 1170px;
            }
        }
        body > div > .container-fluid:nth-of-type(1) {
            margin: 0 auto;
            padding-top: 55px;
        }
        body > div > nav .nav.navbar-nav {
            float: right;
        }
        .dataTables_wrapper .dataTables_filter input{
                      width: 30px;
                      background-color: #EFE3E0;}
        .well {background-color:#CFDAE2;}
        "
    )
  ),
  # .tabs-above > .nav > li[class=active] > a {
  #   background-color: #6171a9;
  #     color: #FFF;
  # }
  
  # .dataTables_wrapper .dataTables_length {
  #   float: right;}
  # .dataTables_wrapper .dataTables_filter {
  #   float: right;
  #   text-align: right;}
  # theme = shinytheme("sandstone"),
  # theme = "shinythemes/css/sandstone.min.css",
  # theme = shiny::bootstrapLib(),
  # tags$head(includeCSS("multilevelcoda-sim-shiny/www/florastheme.css")),
  # checkboxInput("dark_mode", "Dark mode", FALSE),
  navbarPage(
    position = "fixed-top",
    "multilevelcoda Simulation Study",
    theme = floras,
    
    # Simulation Summary -----------------------------
    navbarMenu(
      "Summary Statistics",
      
      ## brmcoda ------------------
      tabPanel(
        "Bayesian Compositional Multilevel",
        fluid = TRUE,
        
        fluidRow(column(
          12,
          selectInput(
            "stat_brmcoda",
            "Performance Measure:",
            c(
              "Bias" = "bias",
              "Coverage" = "cover",
              "Bias-Eliminated Coverage" = "becover",
              "Empirical Standard Error" = "empse",
              "Mean-squared Error" = "mse"
            ),
            width = "100%"
          )
        )),
        
        titlePanel("Simulation Condition"),
        
        sidebarLayout(
          sidebarPanel(
            style = "height:581px",
            # tags$style(".well {background-color:#CFDAE2;}"),
            selectInput("N_brmcoda",
                        "Number of individuals:",
                        c("All",
                          30, 50, 360, 1200)),
            selectInput("K_brmcoda",
                        "Number of days:",
                        c("All",
                          3, 5, 7, 14)),
            radioButtons(
              "D_brmcoda",
              "Number of compositional parts:",
              c(3, 4, 5),
              selected = 3,
              inline = TRUE
            ),
            selectInput(
              "rint_sd_brmcoda",
              "Random Intercept variance:",
              c("medium", "small", "large")
            ),
            conditionalPanel(
              condition = "input.rint_sd_brmcoda == 'medium'",
              selectInput(
                "res_sd1_brmcoda",
                "Residual variance:",
                c("medium", "small", "large")
              )
            ),
            conditionalPanel(
              condition = "input.rint_sd_brmcoda == 'small'",
              selectInput("res_sd2_brmcoda",
                          "Residual variance:",
                          c("large"))
            ),
            conditionalPanel(
              condition = "input.rint_sd_brmcoda == 'large'",
              selectInput("res_sd3_brmcoda",
                          "Residual variance:",
                          c("small"))
            )
            # selectInput("res_sd",
            #             "Residual variance:",
            #             c("medium", "small", "large"))
          ),
          mainPanel(
            fluidRow(column(
              12,
              selectInput(
                "par_brmcoda",
                "Parameter:",
                c(
                  "All",
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
                ),
                width = "100%"
              )
            )),
            DT::dataTableOutput("simsum_brmcoda_table")
          )
        )
      ),
      
      ## substitution ------------------
      tabPanel(
        "Bayesian Compositional Multilevel Substitution",
        fluid = TRUE,
        
        fluidRow(column(
          12,
          selectInput(
            "stat_sub",
            "Performance Measure:",
            c(
              "Bias" = "bias",
              "Coverage" = "cover",
              "Bias-Eliminated Coverage" = "becover",
              "Empirical Standard Error" = "empse",
              "Mean-squared Error" = "mse"
              ),
            width = "100%"
          )
        )),
        
        titlePanel("Simulation Condition"),
        
        sidebarLayout(
          sidebarPanel(
            selectInput("N_sub",
                        "Number of individuals:",
                        c("All",
                          30, 50, 360, 1200)),
            selectInput("K_sub",
                        "Number of days:",
                        c("All",
                          3, 5, 7, 14)),
            radioButtons(
              "D_sub",
              "Number of compositional parts:",
              c(3, 4, 5),
              selected = 3,
              inline = TRUE
            ),
            selectInput(
              "rint_sd_sub",
              "Random Intercept variance:",
              c("medium", "small", "large")
            ),
            conditionalPanel(
              condition = "input.rint_sd_sub == 'medium'",
              selectInput(
                "res_sd1_sub",
                "Residual variance:",
                c("medium", "small", "large")
              )
            ),
            conditionalPanel(
              condition = "input.rint_sd_sub == 'small'",
              selectInput("res_sd2_sub",
                          "Residual variance:",
                          c("large"))
            ),
            conditionalPanel(
              condition = "input.rint_sd_sub == 'large'",
              selectInput("res_sd3_sub",
                          "Residual variance:",
                          c("small"))
            )
            # selectInput("res_sd",
            #             "Residual variance:",
            #             c("medium", "small", "large"))
          ),
          mainPanel(
            fluidRow(
              column(
                6,
                conditionalPanel(
                  condition = "input.D_sub == '3'",
                  selectInput(
                    "delta3_sub",
                    "Substitution:",
                    c(
                      "Sleep",
                      "Physical Activity" = "PA",
                      "Sedentary Behaviour" = "SB"
                    ),
                    width = "100%"
                  )
                ),
                conditionalPanel(
                  condition = "input.D_sub == '4'",
                  selectInput(
                    "delta4_sub",
                    "Substitution:",
                    c(
                      "Sleep",
                      "Moderate-Vigorous Physical Activity" = "MVPA",
                      "Light Physical Activity" = "LPA",
                      "Sedentary Behaviour" = "SB"
                    ),
                    width = "100%"
                  )
                ),
                conditionalPanel(
                  condition = "input.D_sub == '5'",
                  selectInput(
                    "delta5_sub",
                    "Substitution:",
                    c(
                      "Sleep" = "TST",
                      "Awake in Bed" = "WAKE",
                      "Moderate-Vigorous Physical Activity" = "MVPA",
                      "Light Physical Activity" = "LPA",
                      "Sedentary Behaviour" = "SB"
                    ),
                    width = "100%"
                  )
                )
              ),
              column(
                6,
                radioButtons(
                  "level_sub",
                  "Level:",
                  c("between", "within", "all"),
                  selected = "all",
                  inline = TRUE,
                  width = "100%"
                )
              )
            )
            ,
            DT::dataTableOutput("simsum_sub_table")
          )
        )
      )
      
      
    ),
    # Simulation Plots -----------------------------
    navbarMenu(
      "Summary Plots",
      
      ## brmcoda plots ------------------
      tabPanel(
        "Bayesian Compositional Multilevel Parameters",
        fluid = TRUE,
        
        fluidRow(column(
          12,
          selectInput(
            "stat_brmcoda_plot",
            "Performance Measure:",
            c(
              "Bias" = "bias",
              "Coverage" = "cover",
              "Bias-Eliminated Coverage" = "becover",
              "Empirical Standard Error" = "empse",
              "Mean-squared Error" = "mse"
            ),
            width = "100%"
          )
        )),
        fluidRow(
          column(
            4,
            radioButtons(
              "D_brmcoda_plot",
              "Number of compositional parts:",
              c(3, 4, 5),
              selected = 3,
              inline = TRUE,
              width = "100%"
            )
          ),
          column(
            4,
            selectInput(
              "rint_sd_brmcoda_plot",
              "Random Intercept variance:",
              c("medium", "small", "large"),
              width = "100%"
            )
          ),
          column(
            4,
            conditionalPanel(
              condition = "input.rint_sd_brmcoda_plot == 'medium'",
              selectInput(
                "res_sd1_brmcoda_plot",
                "Residual variance:",
                c("medium", "small", "large"),
                width = "100%"
              )
            ),
            conditionalPanel(
              condition = "input.rint_sd_brmcoda_plot == 'small'",
              selectInput(
                "res_sd2_brmcoda_plot",
                "Residual variance:",
                c("large"),
                width = "100%"
              )
            ),
            conditionalPanel(
              condition = "input.rint_sd_brmcoda_plot == 'large'",
              selectInput(
                "res_sd3_brmcoda_plot",
                "Residual variance:",
                c("small"),
                width = "100%"
              )
            )
          )
        ),
        fluidRow(column(
          12,
          align = "center",
          plotlyOutput("simsum_brmcoda_plot",
                       height = "1300px")
        ))
      ),
      
      ## substitution plots ------------------
      tabPanel(
        "Bayesian Compositional Multilevel Substitution Estimates",
        fluid = TRUE,
        
        fluidRow(column(
          12,
          selectInput(
            "stat_sub_plot",
            "Performance Measure:",
            c(
              "Bias" = "bias",
              "Coverage" = "cover",
              "Bias-Eliminated Coverage" = "becover",
              "Empirical Standard Error" = "empse",
              "Mean-squared Error" = "mse"
              ),
            width = "100%"
          )
        )),
        fluidRow(
          column(
            4,
            radioButtons(
              "D_sub_plot",
              "Number of compositional parts:",
              c(3, 4, 5),
              selected = 3,
              inline = TRUE,
              width = "100%"
            )
          ),
          column(
            4,
            selectInput(
              "rint_sd_sub_plot",
              "Random Intercept variance:",
              c("medium", "small", "large"),
              width = "100%"
            )
          ),
          column(
            4,
            conditionalPanel(
              condition = "input.rint_sd_sub_plot == 'medium'",
              selectInput(
                "res_sd1_sub_plot",
                "Residual variance:",
                c("medium", "small", "large"),
                width = "100%"
              )
            ),
            conditionalPanel(
              condition = "input.rint_sd_sub_plot == 'small'",
              selectInput("res_sd2_sub_plot",
                          "Residual variance:",
                          c("large"),
                          width = "100%")
            ),
            conditionalPanel(
              condition = "input.rint_sd_sub_plot == 'large'",
              selectInput("res_sd3_sub_plot",
                          "Residual variance:",
                          c("small"), width = "100%")
            )
          )
        ),
        fluidRow(column(
          12, align = "center",
          plotlyOutput("simsum_sub_plot",
                       height = "1600px")
        ))
      )
    )
    
    
  )
)
