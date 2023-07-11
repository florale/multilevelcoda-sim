ui <- fluidPage(
  # theme = shinytheme("sandstone"),
  # theme = "shinythemes/css/sandstone.min.css",
  # theme = shiny::bootstrapLib(),
  # tags$head(includeCSS("multilevelcoda-sim-shiny/www/florastheme.css")),
  theme = bs_theme(
    bootswatch = "sandstone",
    bg = "#FBF9F6",
    fg = "#1C1718",
    primary = "#708885",
    secondary = "#8CAACB",
    success = "#8DA290",
    info = "#C99696",
    warning = "#bb847a",
    danger = "#944C4C",
    code_font = "Roboto",
    heading_font = NULL,
    font_scale = NULL
  ),
  # thematic::thematic_shiny(),
  navbarPage(
    "multilevelcoda Simulation Study",

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
            c("Bias" = "bias",
              "Bias-Eliminated Coverage" = "becover"),
            width = "100%"
          )
        )),
        
        titlePanel("Simulation Condition"),
        
        sidebarLayout(
          sidebarPanel(
            selectInput("N_brmcoda",
                        "Number of individuals:",
                        c("All",
                          30, 50, 360, 1200
                          )),
            selectInput("K_brmcoda",
                        "Number of days:",
                        c("All",
                          3, 5, 7, 14
                          )),
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
            c("Bias" = "bias",
              "Bias-Eliminated Coverage" = "becover"),
            width = "100%"
          )
        )),
        
        titlePanel("Simulation Condition"),
        
        sidebarLayout(
          sidebarPanel(
            selectInput("N_sub",
                        "Number of individuals:",
                        c("All",
                          30, 50, 360, 1200
                          )),
            selectInput("K_sub",
                        "Number of days:",
                        c("All",
                          3, 5, 7, 14
                          )),
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
            c("Bias" = "bias",
              "Bias-Eliminated Coverage" = "becover"),
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
            c("Bias" = "bias",
              "Bias-Eliminated Coverage" = "becover"),
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
