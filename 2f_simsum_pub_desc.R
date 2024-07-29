brmcoda_tab <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/brmcoda_tab.RDS")
simsum_brmcoda_tab <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_brmcoda_tab.RDS")
sub_tab <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/sub_tab.RDS")
brmcoda_diag <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/brmcoda_diag.RDS")
simsum_diag <- readRDS("/Users/florale/Library/CloudStorage/OneDrive-MonashUniversity/PhD/Manuscripts/Project_multilevelcoda/multilevelcoda-sim-proj/Results/simsum_diag.RDS")

## DESC ----------
psych::describe(brmcoda_tab[Stat == "bias"]$est)
psych::describe(brmcoda_tab[Stat == "cover"]$est)
psych::describe(brmcoda_tab[Stat == "becover"]$est)

psych::describe(sub_tab[Stat == "bias"]$est)
psych::describe(sub_tab[Stat == "cover"]$est)
psych::describe(sub_tab[Stat == "becover"]$est)

psych::describe(brmcoda_diag[Diag_Stat == "bess"]$value)
psych::describe(brmcoda_diag[Diag_Stat == "tess"]$value)
psych::describe(brmcoda_diag[Diag_Stat == "rhat"]$value)
psych::describe(brmcoda_diag[Diag_Stat == "ndt"]$value)

# by cond ---------------------------
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 3, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 3, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 3, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 3, sigma: base"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 5, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 5, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 5, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 5, sigma: base"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 7, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 7, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 7, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 7, sigma: base"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 14, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 14, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: base"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 3, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 3, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 3, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 3, sigma: REsmall_RESlarge"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 5, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 5, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 5, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 5, sigma: REsmall_RESlarge"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 7, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 7, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 7, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 7, sigma: REsmall_RESlarge"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 14, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 14, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: REsmall_RESlarge"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 3, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 3, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 3, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 3, sigma: RElarge_RESsmall"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 5, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 5, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 5, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 5, sigma: RElarge_RESsmall"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 7, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 7, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 7, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 7, sigma: RElarge_RESsmall"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 14, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 14, sigma: RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall"]$est) # 0.79 0.97
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall"]$est) # 0.9   1 

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 3, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 3, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 3, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 3, sigma: REbase_RESlarge"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 5, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 5, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 5, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 5, sigma: REbase_RESlarge"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 7, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 7, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 7, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 7, sigma: REbase_RESlarge"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 14, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 14, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: REbase_RESlarge"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 3, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 3, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 3, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 3, sigma: REbase_RESsmall"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 5, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 5, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 5, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 5, sigma: base"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 7, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 7, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 7, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 7, sigma: REbase_RESsmall"]$est)

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 30, I: 14, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 50, I: 14, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: REbase_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: REbase_RESsmall"]$est)

# sub
psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 3, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 3, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 3, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 3, sigma: base"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 5, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 5, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 5, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 5, sigma: base"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 7, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 7, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 7, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 7, sigma: base"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 14, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 14, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: base"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: base"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 3, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 3, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 3, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 3, sigma: REsmall_RESlarge"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 5, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 5, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 5, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 5, sigma: REsmall_RESlarge"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 7, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 7, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 7, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 7, sigma: REsmall_RESlarge"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 14, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 14, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: REsmall_RESlarge"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 3, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 3, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 3, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 3, sigma: RElarge_RESsmall"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 5, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 5, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 5, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 5, sigma: RElarge_RESsmall"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 7, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 7, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 7, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 7, sigma: RElarge_RESsmall"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 14, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 14, sigma: RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall"]$est) # 0.91 0.98
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall"]$est) # 0.86   1

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 3, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 3, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 3, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 3, sigma: REbase_RESlarge"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 5, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 5, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 5, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 5, sigma: REbase_RESlarge"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 7, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 7, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 7, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 7, sigma: REbase_RESlarge"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 14, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 14, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: REbase_RESlarge"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 3, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 3, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 3, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 3, sigma: REbase_RESsmall"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 5, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 5, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 5, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 5, sigma: base"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 7, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 7, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 7, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 7, sigma: REbase_RESsmall"]$est)

psych::describe(sub_tab[Stat == "cover" & cond == "J: 30, I: 14, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 50, I: 14, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: REbase_RESsmall"]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: REbase_RESsmall"]$est)


# prob
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 3]$est) #0.91, 0.79 0.96
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 3]$est) #0.95 0.9   1

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 4]$est) 
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 4]$est) 

psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 5]$est) 
psych::describe(brmcoda_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 5]$est) 

psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 3]$est)  
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 3]$est) #0.93, 0.86   1 

psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 4]$est)
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 4]$est) 

psych::describe(sub_tab[Stat == "cover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 5]$est) 
psych::describe(sub_tab[Stat == "cover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 5]$est) 

# becover
psych::describe(brmcoda_tab[Stat == "becover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 3]$est) #0.94, 0.89 0.98
psych::describe(brmcoda_tab[Stat == "becover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 3]$est) #0.96 0.9   1

psych::describe(brmcoda_tab[Stat == "becover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 4]$est)
psych::describe(brmcoda_tab[Stat == "becover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 4]$est) 

psych::describe(brmcoda_tab[Stat == "becover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 5]$est) 
psych::describe(brmcoda_tab[Stat == "becover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 5]$est) 

psych::describe(sub_tab[Stat == "becover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 3]$est)  
psych::describe(sub_tab[Stat == "becover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 3]$est) #0.96, 0.86   1 

psych::describe(sub_tab[Stat == "becover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 4]$est)
psych::describe(sub_tab[Stat == "becover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 4]$est) 

psych::describe(sub_tab[Stat == "becover" & cond == "J: 360, I: 14, sigma: RElarge_RESsmall" & D == 5]$est) 
psych::describe(sub_tab[Stat == "becover" & cond == "J: 1200, I: 14, sigma: RElarge_RESsmall" & D == 5]$est) 

# by D  -----------------------------
egltable("est", g = "D", data = brmcoda_tab[Stat == "bias"])
egltable("est", g = "D", data = sub_tab[Stat == "bias"])

egltable("est", g = "D", data = brmcoda_tab[Stat == "cover"])
egltable("est", g = "D", data = sub_tab[Stat == "cover"])

egltable("est", g = "D", data = brmcoda_tab[Stat == "becover"])
egltable("est", g = "D", data = sub_tab[Stat == "becover"])

psych::describe(brmcoda_tab[Stat == "bias" & D == 3]$est)
psych::describe(brmcoda_tab[Stat == "bias" & D == 4]$est)
psych::describe(brmcoda_tab[Stat == "bias" & D == 5]$est) #

psych::describe(sub_tab[Stat == "bias" & D == 3]$est)
psych::describe(sub_tab[Stat == "bias" & D == 4]$est) #
psych::describe(sub_tab[Stat == "bias" & D == 5]$est)

psych::describe(brmcoda_tab[Stat == "cover" & D == 3]$est) #
psych::describe(brmcoda_tab[Stat == "cover" & D == 4]$est)
psych::describe(brmcoda_tab[Stat == "cover" & D == 5]$est)

psych::describe(sub_tab[Stat == "cover" & D == 3]$est) #
psych::describe(sub_tab[Stat == "cover" & D == 4]$est)
psych::describe(sub_tab[Stat == "cover" & D == 5]$est)

psych::describe(brmcoda_tab[Stat == "becover" & D == 3]$est) #
psych::describe(brmcoda_tab[Stat == "becover" & D == 4]$est)
psych::describe(brmcoda_tab[Stat == "becover" & D == 5]$est)

psych::describe(sub_tab[Stat == "becover" & D == 3]$est) #
psych::describe(sub_tab[Stat == "becover" & D == 4]$est)
psych::describe(sub_tab[Stat == "becover" & D == 5]$est)

# by J  -----------------------------
egltable("est", g = "J", data = brmcoda_tab[Stat == "bias"])
egltable("est", g = "J", data = sub_tab[Stat == "bias"])

egltable("est", g = "J", data = brmcoda_tab[Stat == "cover"])
egltable("est", g = "J", data = sub_tab[Stat == "cover"])

egltable("est", g = "J", data = brmcoda_tab[Stat == "becover"])
egltable("est", g = "J", data = sub_tab[Stat == "becover"])

psych::describe(brmcoda_tab[Stat == "bias" & J == 30]$est)
psych::describe(brmcoda_tab[Stat == "bias" & J == 50]$est)
psych::describe(brmcoda_tab[Stat == "bias" & J == 360]$est)
psych::describe(brmcoda_tab[Stat == "bias" & J == 1200]$est)

psych::describe(sub_tab[Stat == "bias" & J == 30]$est)
psych::describe(sub_tab[Stat == "bias" & J == 50]$est)
psych::describe(sub_tab[Stat == "bias" & J == 360]$est)
psych::describe(sub_tab[Stat == "bias" & J == 1200]$est)

psych::describe(brmcoda_tab[Stat == "cover" & J == 30]$est)
psych::describe(brmcoda_tab[Stat == "cover" & J == 50]$est)
psych::describe(brmcoda_tab[Stat == "cover" & J == 360]$est) #
psych::describe(brmcoda_tab[Stat == "cover" & J == 1200]$est) 

psych::describe(sub_tab[Stat == "cover" & J == 30]$est)
psych::describe(sub_tab[Stat == "cover" & J == 50]$est)
psych::describe(sub_tab[Stat == "cover" & J == 360]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200]$est) #

psych::describe(brmcoda_tab[Stat == "becover" & J == 30]$est)
psych::describe(brmcoda_tab[Stat == "becover" & J == 50]$est)
psych::describe(brmcoda_tab[Stat == "becover" & J == 360]$est) #
psych::describe(brmcoda_tab[Stat == "becover" & J == 1200]$est) #

psych::describe(sub_tab[Stat == "becover" & J == 30]$est)
psych::describe(sub_tab[Stat == "becover" & J == 50]$est)
psych::describe(sub_tab[Stat == "becover" & J == 360]$est) ##
psych::describe(sub_tab[Stat == "becover" & J == 1200]$est) #

# by I  -----------------------------
egltable("est", g = "I", data = brmcoda_tab[Stat == "bias"])
egltable("est", g = "I", data = sub_tab[Stat == "bias"])

egltable("est", g = "I", data = brmcoda_tab[Stat == "cover"])
egltable("est", g = "I", data = sub_tab[Stat == "cover"])

egltable("est", g = "I", data = brmcoda_tab[Stat == "becover"])
egltable("est", g = "I", data = sub_tab[Stat == "becover"])

psych::describe(brmcoda_tab[Stat == "bias" & I == 3]$est)
psych::describe(brmcoda_tab[Stat == "bias" & I == 5]$est)
psych::describe(brmcoda_tab[Stat == "bias" & I == 7]$est)
psych::describe(brmcoda_tab[Stat == "bias" & I == 14]$est)

psych::describe(sub_tab[Stat == "bias" & I == 3]$est)
psych::describe(sub_tab[Stat == "bias" & I == 5]$est)
psych::describe(sub_tab[Stat == "bias" & I == 7]$est)
psych::describe(sub_tab[Stat == "bias" & I == 14]$est)

psych::describe(brmcoda_tab[Stat == "cover" & I == 3]$est)
psych::describe(brmcoda_tab[Stat == "cover" & I == 5]$est)
psych::describe(brmcoda_tab[Stat == "cover" & I == 7]$est)
psych::describe(brmcoda_tab[Stat == "cover" & I == 14]$est) #

psych::describe(sub_tab[Stat == "cover" & I == 3]$est)
psych::describe(sub_tab[Stat == "cover" & I == 5]$est)
psych::describe(sub_tab[Stat == "cover" & I == 7]$est)
psych::describe(sub_tab[Stat == "cover" & I == 14]$est) #

psych::describe(brmcoda_tab[Stat == "becover" & I == 3]$est)
psych::describe(brmcoda_tab[Stat == "becover" & I == 5]$est)
psych::describe(brmcoda_tab[Stat == "becover" & I == 7]$est)
psych::describe(brmcoda_tab[Stat == "becover" & I == 14]$est) #

psych::describe(sub_tab[Stat == "becover" & I == 3]$est)
psych::describe(sub_tab[Stat == "becover" & I == 5]$est)
psych::describe(sub_tab[Stat == "becover" & I == 7]$est)
psych::describe(sub_tab[Stat == "becover" & I == 14]$est) #

# by sigma -----------------------------
# bias
psych::describe(brmcoda_tab[Stat == "bias" & condition == "base"]$est)
psych::describe(brmcoda_tab[Stat == "bias" & condition == "REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "bias" & condition == "RElarge_RESsmall"]$est)
psych::describe(brmcoda_tab[Stat == "bias" & condition == "REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "bias" & condition == "REbase_RESsmall"]$est)

psych::describe(sub_tab[Stat == "bias" & condition == "base"]$est)
psych::describe(sub_tab[Stat == "bias" & condition == "REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "bias" & condition == "RElarge_RESsmall"]$est)
psych::describe(sub_tab[Stat == "bias" & condition == "REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "bias" & condition == "REbase_RESsmall"]$est)

# cover
psych::describe(brmcoda_tab[Stat == "cover" & condition == "base"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & condition == "REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & condition == "RElarge_RESsmall"]$est) #
psych::describe(brmcoda_tab[Stat == "cover" & condition == "REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "cover" & condition == "REbase_RESsmall"]$est)

psych::describe(sub_tab[Stat == "cover" & condition == "base"]$est)
psych::describe(sub_tab[Stat == "cover" & condition == "REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall"]$est) #
psych::describe(sub_tab[Stat == "cover" & condition == "REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & condition == "REbase_RESsmall"]$est)

# becover
psych::describe(brmcoda_tab[Stat == "becover" & condition == "base"]$est)
psych::describe(brmcoda_tab[Stat == "becover" & condition == "REsmall_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "becover" & condition == "RElarge_RESsmall"]$est) #
psych::describe(brmcoda_tab[Stat == "becover" & condition == "REbase_RESlarge"]$est)
psych::describe(brmcoda_tab[Stat == "becover" & condition == "REbase_RESsmall"]$est)

psych::describe(sub_tab[Stat == "becover" & condition == "base"]$est)
psych::describe(sub_tab[Stat == "becover" & condition == "REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "becover" & condition == "RElarge_RESsmall"]$est) #
psych::describe(sub_tab[Stat == "becover" & condition == "REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "becover" & condition == "REbase_RESsmall"]$est)


# check problemtic combination
# RElarge_RESsmall
psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & J == 30]$est)
psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & J == 50]$est)
psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & J == 360]$est)
psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & J == 1200]$est) #

psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & I == 3]$est)
psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & I == 5]$est)
psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & I == 7]$est)
psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & I == 14]$est) #

psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & D == 3]$est) #
psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & D == 4]$est)
psych::describe(sub_tab[Stat == "cover" & condition == "RElarge_RESsmall" & D == 5]$est)

psych::describe(sub_tab[Stat == "cover" & J == 1200 & condition == "base"]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & condition == "REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & condition == "RElarge_RESsmall"]$est) #
psych::describe(sub_tab[Stat == "cover" & J == 1200 & condition == "REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & condition == "REbase_RESsmall"]$est)

psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 3]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 5]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 7]$est) 
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14]$est) #

psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "base"]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REsmall_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "RElarge_RESsmall"]$est) #
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REbase_RESlarge"]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REbase_RESsmall"]$est)

psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "base" & D == 3]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "base" & D == 4]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "base" & D == 5]$est)

psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REsmall_RESlarge" & D == 3]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REsmall_RESlarge" & D == 4]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REsmall_RESlarge" & D == 5]$est)

psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "RElarge_RESsmall" & D == 3]$est) #
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "RElarge_RESsmall" & D == 4]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "RElarge_RESsmall" & D == 5]$est)

psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REbase_RESlarge" & D == 3]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REbase_RESlarge" & D == 4]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REbase_RESlarge" & D == 5]$est)

psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REbase_RESsmall" & D == 3]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REbase_RESsmall" & D == 4]$est)
psych::describe(sub_tab[Stat == "cover" & J == 1200 & I == 14 & condition == "REbase_RESsmall" & D == 5]$est)
