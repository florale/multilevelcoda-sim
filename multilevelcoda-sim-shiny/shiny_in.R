library(data.table)
library(shiny)
library(shinythemes)

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
    "#8DA290", "#133A1B")
col_sub_d4 <-
  c("#2A3E59", "#456691",
    "#944C4C", "#C99696",
    "#bf5b4b", "#A69188", 
    "#EAD3BF", "#FAD899",
    "#8DA290", "#133A1B",
    "#6d765b", "#3d251e")
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
    "#3b4031", "#3d251e")