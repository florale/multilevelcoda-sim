library(data.table)
library(extraoperators)
library(shiny)
library(shinythemes)
library(shiny)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(plotly)
library(bslib)
library(sass)

# data --------------
brmcoda_tab <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/brmcoda_tab.RDS")
brmcoda_dat <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/brmcoda_dat.RDS")

sub_tab <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/sub_tab.RDS")
sub_dat <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/sub_dat.RDS")

brmcoda_d3 <- brmcoda_dat[["brmcoda_d3"]]
brmcoda_d4 <- brmcoda_dat[["brmcoda_d4"]]
brmcoda_d5 <- brmcoda_dat[["brmcoda_d5"]]

sub_d3 <- sub_dat[["sub_d3"]]
sub_d4 <- sub_dat[["sub_d4"]]
sub_d5 <- sub_dat[["sub_d5"]]

# colour palette --------------
col_brmcoda_d3 <- 
  c("#9A5C7D", "#B98AA3", "#DCD5CE", "#8DA290", "#708885", "#5A6367", 
    "#1C1718")
col_brmcoda_d4 <- 
  c("#9A5C7D", "#B98AA3", "#DCD5CE", "#8DA290", "#708885", "#5A6367", 
    "#456691", "#2A3E59", 
    "#1C1718")
col_brmcoda_d5 <- 
  c("#9A5C7D", "#B98AA3", "#DCD5CE", "#8DA290", "#708885", "#5A6367", 
    "#456691", "#2A3E59", 
    "#9c8aa4", "#5E4F65", 
    "#1C1718")

col_sub_d3 <- 
  c("#bf5b4b", "#A69188",
    "#EAD3BF", "#FAD899",
    "#8DA290", "#133A1B"
  )
col_sub_d4 <-
  c("#2A3E59", "#456691",
    "#944C4C", "#C99696",
    "#bf5b4b", "#A69188",
    "#EAD3BF", "#FAD899",
    "#8DA290", "#133A1B",
    "#6d765b", "#3d251e"
  )
col_sub_d5 <- 
  c("#1C1718", "#2A3E59",
    "#456691", "#647F9A",
    "#8CAACB", "#DCD5CE",
    "#DAA5AE", "#b6485d",
    "#944C4C", "#C99696",
    "#bf5b4b", "#bb847a",
    "#A69188", "#EAD3BF",
    "#FAD899", "#8DA290",
    "#133A1B", "#6d765b",
    "#3b4031", "#3d251e"
  )

## theme
shinyOptions(bslib = TRUE)
theme <- bs_theme(
  # version = 5,
  bootswatch = "minty",
  bg = "#fff",
  fg = "#1C1718",
  primary = "#A1B2C2",
  secondary = "#A69188",
  success = "#8DA290",
  info = "#d18d9a",
  warning = "#FAD899",
  danger = "#944C4C",
  # base_font = "Roboto",
  code_font = "Consolas",
  heading_font = NULL,
  font_scale = NULL,
)
floras <-
  bs_add_variables(
    theme,
    .where = "declarations",
    "body-color" = "#58504C",
    "panel-inner-border" = "#CFDAE2",
    "navbar-light-brand-color" = "#FAF7F3",
    "navbar-light-active-color" = "#b39b89",
    "navbar-light-hover-color" = "#d18d9a",
    "navbar-light-bg" = "#665C58",
    "navbar-default-link-hover-bg" = "#A1B2C2",
    "navbar-light-panel-bg" = "#A1B2C2",
    "navbar-inner-bg" = "#A1B2C2",
    
    "table-light-striped-bg" = "#FAF7F3",
    "table-light-striped-bg-active" = "#CFDAE2",
    "table-light-striped-bg-hover" = "#DCD5CE",
    "table-light-bg-accent" = "#CFDAE2",
    
    "pagination-color" = "#3d251e",
    "pagination-bg" = "#BEC7B4",
    "pagination-border-color" = "#BEC7B4",
    
    "pagination-hover-colour" = "#FAF7F3",
    "pagination-hover-bg" = "#708885",
    "pagination-hover-border-color" = "#708885",
    
    "pagination-active-bg" = "#8DA290",
    "pagination-active-border-color" = "#8DA290",
    
    "pagination-disabled-bg" = "#5A6367", 
    "pagination-disabled-border-color" = "#5A6367",
    "pagination-disabled-color" = "#708885",
    
    "navbar-light-table-striped-color" = "#1C1718",
    "navbar-light-table-striped-bg" = "#CFDAE2",
    "navbar-light-table-hover-bg" = "#CFDAE2",
    
    "dropdown-bg" = "#fff",
    # "table-border-color" = "#DCD5CE",
    
    "table-bg" = "#E5E8E1",   #E5E8E1 DFD7D6 
    "table-accent-bg" = "#fff", 
    
    "navbar-sidebar-bg" = "#A1B2C2",

    "table-thead-light" = "#d18d9a  !important",
    "table-thead-bg" = "#d18d9a !important",
    
    # "table-striped-bg" = "#FAF7F3",
    # "table-hover-bg-factor" = "1.25",
    "btn-default-bg" = "#A1B2C2",
    "btn-success-bg" = "#A1B2C2",
    "input-bg" = "#fff",
    
    
    "legend-color" = "#DCD5CE",
    "border-primary" = "#3d251e",
    "bg-primary" = "#3d251e",
    "table-active-bg" = "#F0DBDE  !important"
    # "bs-table-hover-bg" = "#CFDAE2",
    # 
    # "bs-table-bg-accent" = "#CFDAE2"
    
    # "inverse-bg" = "#DCD5CE"
    
  )

# dark <- bs_theme(
#   # version = 5,
#   # bootswatch = "sandstone",
#   bg = "#1C1718",
#   fg = "#FBF9F6",
#   primary = "#A1B2C2",
#   secondary = "#8CAACB",
#   success = "#8DA290",
#   info = "#C99696",
#   warning = "#bb847a",
#   danger = "#944C4C",
#   code_font = "Roboto",
#   heading_font = NULL,
#   font_scale = NULL
# )
# thematic::thematic_shiny()
# bs_theme_preview(floras, with_themer = FALSE)

