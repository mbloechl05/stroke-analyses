## =================================
##  Descriptive data analysis
## =================================

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)  # for fill()
library(Rmisc) # for summarySEwithin()
library(reshape)
library(car) # for recode()
library(mitml)
library(tibble)
library(mitml)
library(cobalt)  # for bal.tab()
library(forcats) # for fct_rev()

# Source helper function describeBy_imp()
source("analyses/post-stroke-depression/fun_describeBy_imp.R")

# Load data 
load("data/processed/proc_data.RData") # preprocessed
load("data/processed/matched_data.RData") # matched
load("data/processed/imp_data.RData") # imputed
impdat <- mice::complete(imp, action = "long", include = FALSE)


# --------------------------------
# 2) Check balance diagnostics
# --------------------------------

# 2.1) Data wrangling
# ----------------------

# First, unlist list of lists
for (i in seq(matchlist)) 
  assign(paste0("matched",i), matchlist[[i]]$matched)

# List again lists of similar type (matched)
matchedlist <- list(matched1,  matched2,  matched3,  matched4,  matched5,  
                    matched6,  matched7,  matched8,  matched9,  matched10, 
                    matched11, matched12, matched13, matched14, matched15, 
                    matched16, matched17, matched18, matched19, matched20)


# 2.2) Summary of matching procedure in each data set
# -----------------------------------------------------

lapply(matchedlist, summary)


# 2.3) Figure S2
# ---------------

### Create and save plots of effect sizes before and after matching
for (i in seq(matchedlist)) {
  psm.bal             <- bal.tab(matchedlist[[i]], binary = "std", un = T)
  psm.bal$Balance$var <- seq.int(nrow(psm.bal$Balance))
  psm.bal             <- psm.bal$Balance[2:10,]
  psm.bal$var         <- as.factor(psm.bal$var)
  psm.bal$var         <- fct_rev(as_factor(psm.bal$var))
  psm.n.cont          <- matchedlist[[i]]$nn[[2]]
  psm.n.treat         <- matchedlist[[i]]$nn[[6]]
  # Start saving plot
  png(paste0("figures/bal/",i,".png"), width = 7.5, height = 6, 
      units = 'in', res = 700)
  # Define general graphic settings
  par(mfrow = c(1,1))
  par(mar = c(5,10,4,3))
  # Plot Unadjusted standardised differences 
  plot(psm.bal$Diff.Un, psm.bal$var, pch = 19, cex = 1.3, col = "#DC5474",
       xlim = c(-0.5,1), xlab = "Standardised mean difference", 
       yaxt = "n", ylab = "", cex.lab = 1.5, cex.axis = 1.5,
       frame.plot = F)
  # Add lines to the plot
  abline(v =  0.00, col = "black" , lwd = 1, lty = 1)
  abline(v = -0.10, col = "black" , lwd = 1, lty = 3)
  abline(v =  0.10, col = "black" , lwd = 1, lty = 3)
  abline(h =  1   , col = "grey70", lwd = 1, lty = 3)
  abline(h =  2   , col = "grey70", lwd = 1, lty = 3)
  abline(h =  3   , col = "grey70", lwd = 1, lty = 3)
  abline(h =  4   , col = "grey70", lwd = 1, lty = 3)
  abline(h =  5   , col = "grey70", lwd = 1, lty = 3)
  abline(h =  6   , col = "grey70", lwd = 1, lty = 3)
  abline(h =  7   , col = "grey70", lwd = 1, lty = 3)
  abline(h =  8   , col = "grey70", lwd = 1, lty = 3)
  abline(h =  9   , col = "grey70", lwd = 1, lty = 3)
  #  abline(h = 10, col = "grey70", lwd = 1, lty = 3)
  par(new = TRUE)
  # Plot adjusted standardised differences on top 
  plot(psm.bal$Diff.Adj, psm.bal$var, pch = 19, cex = 1.3, col = "#1B3A4E",
       xlim = c(-0.5,1), xlab = "", 
       yaxt = "n", ylab = "", cex.lab = 1.5, cex.axis = 1.5,
       frame.plot = F)
  # Add title, subtitle, and y-axis text to the plot
  mtext(paste("Matched: Stroke N =",psm.n.treat,"; Control N =", 
              psm.n.cont), 3, line = 1.3, cex = 1.4)
  mtext(paste("Sample", i),  3, line = 2.7, cex = 1.8, font = 2)
  axis(2, at = 1:9, labels = F, tck = 0)
  text(y = seq(1, 9, by = 1), par("usr")[1],
       labels = c("Cohabiting", "Hypertension", "BMI",
                  "Diabetes", "Smoking", "Education", "Ethnicity", "Gender", "Age"),
       srt = 0, pos = 2, xpd = TRUE, cex = 1.4)
  # Add legend to third plot
  if(i == 3){
    legend(0.39, 2.5, legend=c("Before matching", "After matching"), 
           col = c("#DC5474", "#1B3A4E"), pch = c(19, 19), cex = 1.2, box.lty = 1)
  } else {NULL}
  # End saving plot
  dev.off()
}


# 2.4) Additional figures: jitter plots 

for (i in seq(matchedlist)) {
  png(paste0("figures/jitter/",i,".png"), width = 6, height = 6, 
      units = 'in', res = 300)
  plot(matchedlist[[i]], type = "jitter")
  dev.off()
}


# 2.5) Additional figures: histograms 

for (i in seq(matchedlist)) {
  png(paste0("figures/hist/",i,".png"), width = 6, height = 6, 
      units = 'in', res = 300)
  plot(matchedlist[[i]], type = "hist")
  dev.off()
}

# -------------------------------------
#  Descriptive statistics (Table 2)
# -------------------------------------

# Descriptive stats for observed, imputded data by caseness 

### Calculate descriptives
obs.age <- describeBy_imp("w1_dhager", impdat, binary = F, strokevar = "n_strokes")
obs.gen <- describeBy_imp("w1_dhsex",  impdat, binary = T, strokevar = "n_strokes")
obs.eth <- describeBy_imp("w0_ethni",  impdat, binary = T, strokevar = "n_strokes")
obs.edu <- describeBy_imp("w0_educ",   impdat, binary = T, strokevar = "n_strokes")
obs.smo <- describeBy_imp("w1_heska",  impdat, binary = T, strokevar = "n_strokes")
obs.bmi <- describeBy_imp("w0_bmival", impdat, binary = F, strokevar = "n_strokes")
obs.hyp <- describeBy_imp("w1_hypt",   impdat, binary = T, strokevar = "n_strokes")
obs.dia <- describeBy_imp("w1_diab",   impdat, binary = T, strokevar = "n_strokes")
obs.liv <- describeBy_imp("w1_scptr",  impdat, binary = T, strokevar = "n_strokes")

### Show all descriptive stats in list
list("age"        = i.age, 
     "gender"     = i.gen, 
     "ethnicity"  = i.eth, 
     "education"  = i.edu, 
     "smoking"    = i.smo, 
     "bmi"        = i.bmi, 
     "hypert"     = i.hyp, 
     "diabetes"   = i.dia, 
     "cohab_marr" = i.liv)


# 1.2) Descriptive stats for matched, imputded data by caseness 

### Unlist list of dataframe list to obtain dataframes in wide format and 
for (i in seq(matchlist))
  assign(paste0("data.match.w",i), matchlist[[i]]$data.match.w)

### Re-list them as mitml list
datamatchwlist <- as.mitml.list(list(data.match.w1,  data.match.w2, 
                                     data.match.w3,  data.match.w4, 
                                     data.match.w5,  data.match.w6, 
                                     data.match.w7,  data.match.w8, 
                                     data.match.w9,  data.match.w10, 
                                     data.match.w11, data.match.w12, 
                                     data.match.w13, data.match.w14, 
                                     data.match.w15, data.match.w16, 
                                     data.match.w17, data.match.w18, 
                                     data.match.w19, data.match.w20))

### Rename list indices for mitml list
names(datamatchwlist) <- c("1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , "9" , "10", 
                           "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")

### Stack data frames from list onto each other
match_data_stacked <- map2_df(datamatchwlist, names(datamatchwlist), ~ 
                                mutate(.x, .imp = .y))

### Now finally, compute descriptives
mat.age <- describeBy_imp("w1_dhager", match_data_stacked, binary = F, strokevar = "n_strokes")
mat.gen <- describeBy_imp("w1_dhsex",  match_data_stacked, binary = T, strokevar = "n_strokes")
mat.eth <- describeBy_imp("w0_ethni",  match_data_stacked, binary = T, strokevar = "n_strokes")
mat.edu <- describeBy_imp("w0_educ",   match_data_stacked, binary = T, strokevar = "n_strokes")
mat.smo <- describeBy_imp("w1_heska",  match_data_stacked, binary = T, strokevar = "n_strokes")
mat.bmi <- describeBy_imp("w0_bmival", match_data_stacked, binary = F, strokevar = "n_strokes")
mat.hyp <- describeBy_imp("w1_hypt",   match_data_stacked, binary = T, strokevar = "n_strokes")
mat.dia <- describeBy_imp("w1_diab",   match_data_stacked, binary = T, strokevar = "n_strokes")
mat.liv <- describeBy_imp("w1_scptr",  match_data_stacked, binary = T, strokevar = "n_strokes")

### Show all descriptive stats in list
list("age"        = i.age, 
     "gender"     = i.gen, 
     "ethnicity"  = i.eth, 
     "education"  = i.edu, 
     "smoking"    = i.smo, 
     "bmi"        = i.bmi, 
     "hypert"     = i.hyp, 
     "diabetes"   = i.dia, 
     "cohab_marr" = i.liv)



