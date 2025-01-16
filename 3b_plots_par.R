source("2e_simsum_tidy.R")

# library, colours ----------------------
colour <- c("#A69188", "#DCD5CE", "#FAF7F3", "#A1B2C2", "#CFDAE2")
col4 <- c("#C99696", "#BEACA2", "#DCD5CE", "#647A77", "#878E91") #sleep #wake #mvpa #lpa #sb
col4 <- wes_palette("IsleofDogs1", 4)
col20 <- c(
  "#1C1718",
  "#5A6367",
  "#2A3E59",
  "#456691",
  "#647F9A",
  "#8CAACB",
  "#9c8aa4",
  "#ABA2C3",
  "#9A5C7D",
  "#B98AA3",
  "#cc8a8c",
  "#A54E50",
  "#DCD5CE",
  "#B49797",
  "#C99696",
  "#DAA5AE",
  "#d18d9a",
  "#b6485d",
  "#D1ACA5",
  "#C7AAA5",
  "#4F7375",
  "#769798",
  "#944C4C",
  "#ba6c6e",
  "#bf5b4b",
  "#bb847a",
  "#A69188",
  "#EAD3BF",
  "#FAD899",
  "#353D60",
  "#6171a9",
  "#8DA290",
  "#133A1B",
  "#6d765b",
  "#3b4031",
  "#c48462",
  "#3d251e",
  "#ab8b8b",
  "#D1ACA5"
)

col_brmcoda_d3 <-
  c("#9A5C7D", "#B98AA3",
    "#DCD5CE", "#8DA290",
    "#708885", "#5A6367",
    "#1C1718")
col_brmcoda_d4 <-
  c(
    "#354140",
    "#5E4F65",
    "#ABA2C3",
    
    # "#5A6367",
    "#708885",
    "#99ABA9",
    
    "#EFE3E0",
    
    # "#DAA5AE",
    "#B98AA3",
    
    # "#6171a9",
    "#9A5C7D",
    "#3d251e",
    
    "#2A3E59", "#456691", 
    "#8DA290", "#708885", 
    "#9c8aa4", "#5E4F65",
    "#B98AA3", "#9A5C7D"
  )
col_brmcoda_d5 <-
  c(
    "#9A5C7D", "#B98AA3",
    "#DCD5CE", "#8DA290",
    "#708885", "#5A6367",
    "#456691", "#2A3E59",
    "#9c8aa4", "#5E4F65",
    "#1C1718"
  )

col_sub_d3 <-
  c("#bf5b4b", "#A69188",
    "#EAD3BF", "#FAD899",
    "#8DA290", "#133A1B")
col_sub_d4 <-
  c(
    "#2A3E59",
    "#456691",
    "#8CAACB",
    "#EAD3BF",
    "#FAD899",
    "#8DA290",
    "#5B6F5D",
    "#133A1B",
    # "#3d251e",
    "#944C4C",
    "#A69188",
    # "#A69188",
    "#bb847a",
    "#BEACA2"
  )
col_sub_d5 <-
  c(
    "#1C1718", "#2A3E59",
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

## d3 brmcoda base plot ----------------------------------------------------------------------------
(plot_bias_brmcoda_d3_base <-
    .par_plot(brmcoda_d3[stat == "bias" & condition == "base"]))

(plot_becover_brmcoda_d3_base <-
    .par_plot(brmcoda_d3[stat == "becover" & condition == "base"]))

## d3 sub base plot --------------------------------------------------------------------------------
(plot_bias_sub_d3_base <-
   .par_plot(sub_d3[stat == "bias" & condition == "base"]))

(plot_becover_sub_d3_base <-
    .par_plot(sub_d3[stat == "becover" & condition == "base"]))

## d4 brmcoda base plot ----------------------------------------------------------------------------
(plot_bias_brmcoda_d4_base <-
   .par_plot(brmcoda_d4[stat == "bias" & condition == "base"]))

(plot_becover_brmcoda_d4_base <-
    .par_plot(brmcoda_d4[stat == "becover" & condition == "base"]))

## d4 sub base plot --------------------------------------------------------------------------------
(plot_bias_sub_d4_base <-
   .par_plot(sub_d4[stat == "bias" & condition == "base"]))

(plot_becover_sub_d4_base <-
    .par_plot(sub_d4[stat == "becover" & condition == "base"]))

## d5 brmcoda base plot ----------------------------------------------------------------------------
(plot_bias_brmcoda_d5_base <-
   .par_plot(brmcoda_d5[stat == "bias" & condition == "base"]))

(plot_becover_brmcoda_d5_base <-
    .par_plot(brmcoda_d5[stat == "becover" & condition == "base"]))

## d5 sub base plot --------------------------------------------------------------------------------
(plot_bias_sub_d5_base <-
   .par_plot(sub_d5[stat == "bias" & condition == "base"]))

(plot_becover_sub_d5_base <-
    .par_plot(sub_d5[stat == "becover" & condition == "base"]))

## d3 brmcoda plot - u0 base e small ----------------------------------------------------------
(plot_bias_brmcoda_d3_ubes <-
   .par_plot(brmcoda_d3[stat == "bias" &
                          condition == "REbase_RESsmall"]))

(plot_becover_brmcoda_d3_ubes <-
    .par_plot(brmcoda_d3[stat == "becover" &
                           condition == "REbase_RESsmall"]))

## d3 sub plot - u0 base e small --------------------------------------------------------------
(plot_bias_sub_d3_ubes <-
   .par_plot(sub_d3[stat == "bias" & condition == "REbase_RESsmall"]))

(plot_becover_sub_d3_ubes <-
    .par_plot(sub_d3[stat == "becover" &
                       condition == "REbase_RESsmall"]))

## d4 brmcoda plot - u0 base e small ----------------------------------------------------------
(plot_bias_brmcoda_d4_ubes <-
   .par_plot(brmcoda_d4[stat == "bias" &
                          condition == "REbase_RESsmall"]))

(plot_becover_brmcoda_d4_ubes <-
    .par_plot(brmcoda_d4[stat == "becover" &
                           condition == "REbase_RESsmall"]))

## d4 sub plot - u0 base e small --------------------------------------------------------------
(plot_bias_sub_d4_ubes <-
   .par_plot(sub_d4[stat == "bias" & condition == "REbase_RESsmall"]))

(plot_becover_sub_d4_ubes <-
    .par_plot(sub_d4[stat == "becover" &
                       condition == "REbase_RESsmall"]))

## d5 brmcoda plot - u0 base e small ----------------------------------------------------------
(plot_bias_brmcoda_d5_ubes <-
   .par_plot(brmcoda_d5[stat == "bias" &
                          condition == "REbase_RESsmall"]))

(plot_becover_brmcoda_d5_ubes <-
    .par_plot(brmcoda_d5[stat == "becover" &
                           condition == "REbase_RESsmall"]))

## d5 sub plot - u0 base e small --------------------------------------------------------------
(plot_bias_sub_d5_ubes <-
   .par_plot(sub_d5[stat == "bias" & condition == "REbase_RESsmall"]))

(plot_becover_sub_d5_ubes <-
    .par_plot(sub_d5[stat == "becover" &
                       condition == "REbase_RESsmall"]))

## d3 brmcoda plot - u0 base e large ----------------------------------------------------------
(plot_bias_brmcoda_d3_ubel <-
   .par_plot(brmcoda_d3[stat == "bias" &
                          condition == "REbase_RESlarge"]))

(plot_becover_brmcoda_d3_ubel <-
    .par_plot(brmcoda_d3[stat == "becover" &
                           condition == "REbase_RESlarge"]))

## d3 sub plot - u0 base e large --------------------------------------------------------------
(plot_bias_sub_d3_ubel <-
   .par_plot(sub_d3[stat == "bias" & condition == "REbase_RESlarge"]))

(plot_becover_sub_d3_ubel <-
    .par_plot(sub_d3[stat == "becover" &
                       condition == "REbase_RESlarge"]))

## d4 brmcoda plot - u0 base e large ----------------------------------------------------------
(plot_bias_brmcoda_d4_ubel <-
   .par_plot(brmcoda_d4[stat == "bias" &
                          condition == "REbase_RESlarge"]))

(plot_becover_brmcoda_d4_ubel <-
    .par_plot(brmcoda_d4[stat == "becover" &
                           condition == "REbase_RESlarge"]))

## d4 sub plot - u0 base e large --------------------------------------------------------------
(plot_bias_sub_d4_ubel <-
   .par_plot(sub_d4[stat == "bias" & condition == "REbase_RESlarge"]))

(plot_becover_sub_d4_ubel <-
    .par_plot(sub_d4[stat == "becover" &
                       condition == "REbase_RESlarge"]))

## d5 brmcoda plot - u0 base e large ----------------------------------------------------------
(plot_bias_brmcoda_d5_ubel <-
   .par_plot(brmcoda_d5[stat == "bias" &
                          condition == "REbase_RESlarge"]))

(plot_becover_brmcoda_d5_ubel <-
    .par_plot(brmcoda_d5[stat == "becover" &
                           condition == "REbase_RESlarge"]))

## d5 sub plot - u0 base e large --------------------------------------------------------------
(plot_bias_sub_d5_ubel <-
   .par_plot(sub_d5[stat == "bias" & condition == "REbase_RESlarge"]))

(plot_becover_sub_d5_ubel <-
    .par_plot(sub_d5[stat == "becover" &
                       condition == "REbase_RESlarge"]))

## d3 brmcoda usel plot - u0 small e large ----------------------------------------------------------
(plot_bias_brmcoda_d3_usel <-
   .par_plot(brmcoda_d3[stat == "bias" &
                          condition == "REsmall_RESlarge"]))

(plot_becover_brmcoda_d3_usel <-
    .par_plot(brmcoda_d3[stat == "becover" &
                           condition == "REsmall_RESlarge"]))

## d3 sub usel plot - u0 small e large --------------------------------------------------------------
(plot_bias_sub_d3_usel <-
   .par_plot(sub_d3[stat == "bias" &
                      condition == "REsmall_RESlarge"]))

(plot_becover_sub_d3_usel <-
    .par_plot(sub_d3[stat == "becover" &
                       condition == "REsmall_RESlarge"]))

## d4 brmcoda usel plot - u0 small e large ----------------------------------------------------------
(plot_bias_brmcoda_d4_usel <-
   .par_plot(brmcoda_d4[stat == "bias" &
                          condition == "REsmall_RESlarge"]))

(plot_becover_brmcoda_d4_usel <-
    .par_plot(brmcoda_d4[stat == "becover" &
                           condition == "REsmall_RESlarge"]))

## d4 sub usel plot - u0 small e large --------------------------------------------------------------
(plot_bias_sub_d4_usel <-
   .par_plot(sub_d4[stat == "bias" & condition == "REsmall_RESlarge"]))

(plot_becover_sub_d4_usel <-
    .par_plot(sub_d4[stat == "becover" &
                       condition == "REsmall_RESlarge"]))

## d5 brmcoda usel plot - u0 small e large ----------------------------------------------------------
(plot_bias_brmcoda_d5_usel <-
   .par_plot(brmcoda_d5[stat == "bias" &
                          condition == "REsmall_RESlarge"]))

(plot_becover_brmcoda_d5_usel <-
    .par_plot(brmcoda_d5[stat == "becover" &
                           condition == "REsmall_RESlarge"]))

## d5 sub usel plot - u0 small e large --------------------------------------------------------------
(plot_bias_sub_d5_usel <-
   .par_plot(sub_d5[stat == "bias" & condition == "REsmall_RESlarge"]))

(plot_becover_sub_d5_usel <-
    .par_plot(sub_d5[stat == "becover" &
                       condition == "REsmall_RESlarge"]))

## d3 brmcoda ules plot - u0 large e small ----------------------------------------------------------
(plot_bias_brmcoda_d3_ules <-
   .par_plot(brmcoda_d3[stat == "bias" &
                          condition == "RElarge_RESsmall"]))

(plot_becover_brmcoda_d3_ules <-
    .par_plot(brmcoda_d3[stat == "becover" &
                           condition == "RElarge_RESsmall"]))

## d3 sub ules plot - u0 large e small --------------------------------------------------------------
(plot_bias_sub_d3_ules <-
   .par_plot(sub_d3[stat == "bias" & condition == "RElarge_RESsmall"]))

(plot_becover_sub_d3_ules <-
    .par_plot(sub_d3[stat == "becover" &
                       condition == "RElarge_RESsmall"]))

## d4 brmcoda ules plot - u0 large e small ----------------------------------------------------------
(plot_bias_brmcoda_d4_ules <-
   .par_plot(brmcoda_d4[stat == "bias" &
                          condition == "RElarge_RESsmall"]))

(plot_becover_brmcoda_d4_ules <-
    .par_plot(brmcoda_d4[stat == "becover" &
                           condition == "RElarge_RESsmall"]))

## d4 sub ules plot - u0 large e small --------------------------------------------------------------
(plot_bias_sub_d4_ules <-
   .par_plot(sub_d4[stat == "bias" & condition == "RElarge_RESsmall"]))

(plot_becover_sub_d4_ules <-
    .par_plot(sub_d4[stat == "becover" &
                       condition == "RElarge_RESsmall"]))

## d5 brmcoda ules plot - u0 large e small ----------------------------------------------------------
(plot_bias_brmcoda_d5_ules <-
   .par_plot(brmcoda_d5[stat == "bias" &
                          condition == "RElarge_RESsmall"]))

(plot_becover_brmcoda_d5_ules <-
    .par_plot(brmcoda_d5[stat == "becover" &
                           condition == "RElarge_RESsmall"]))

## d5 sub ules plot - u0 large e small --------------------------------------------------------------
(plot_bias_sub_d5_ules <-
  .par_plot(sub_d5[stat == "bias" & condition == "RElarge_RESsmall"]))

(plot_becover_sub_d5_ules <-
  .par_plot(sub_d5[stat == "becover" &
                     condition == "RElarge_RESsmall"]))
