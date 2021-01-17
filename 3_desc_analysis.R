# =================================
#  Descriptive data analysis
# =================================

# Source helper functions
source("analyses/helpers_describe.R")

# Load data 
load("data/processed/proc_data.RData") # preprocessed
load("data/processed/matched_data.RData") # matched
load("data/processed/imp_data.RData") # imputed
impdat <- mice::complete(imp, action = "long", include = FALSE)


# --------------------------------------------
# 1) MI: Check missing data before and after 
# --------------------------------------------

# 1.1) Before imputation: Descriptive stats for each variable in data frame
# ----------------------------------------------------------------------------

# calcualte descriptive stats 
age <- describe(data_red$w1_dhager) 
gen <- descr_b (data_red$w1_dhsex) # male gender?
eth <- descr_b (data_red$w0_ethni) # white ethnicity?
edu <- descr_b (data_red$w0_educ)
smo <- descr_b (data_red$w1_heska)
bmi <- describe(data_red$w0_bmival)
hyp <- descr_b (data_red$w1_hypt)
dia <- descr_b (data_red$w1_diab)

# show all descriptive stats in list
list("age"        = age, 
     "gender"     = gen, 
     "ethnicity"  = eth, 
     "education"  = edu, 
     "smoking"    = smo, 
     "bmi"        = bmi, 
     "hypert"     = hyp, 
     "diabetes"   = dia)


# 1.2) Descriptives observed and imputed data (Table S1)
# ------------------------------------------------------

# compute mean and standard deviation in each imputed dataset
i.age <- describe_imputed("w1_dhager",  bin = F)
i.gen <- describe_imputed("w1_dhsex",   bin = T)
i.eth <- describe_imputed("w0_ethni",   bin = T)
i.edu <- describe_imputed("w0_educ",    bin = T)
i.smo <- describe_imputed("w1_heska",   bin = T)
i.bmi <- describe_imputed("w0_bmival",  bin = F)
i.hyp <- describe_imputed("w1_hypt",    bin = T)
i.dia <- describe_imputed("w1_diab",    bin = T)

# show all descriptive stats in list
list("age"        = i.age, 
     "gender"     = i.gen, 
     "ethnicity"  = i.eth, 
     "education"  = i.edu, 
     "smoking"    = i.smo, 
     "bmi"        = i.bmi, 
     "hypert"     = i.hyp, 
     "diabetes"   = i.dia)


# 1.3) Table S1: Descriptives observed and imputed covariates
# ---------------------------------------------------------

N <- 10797

### Now, we can create a data frame for stats that go into Table S1
mi_describe_frame = structure(list(
  # set variable names
  var = c("Age (in years)", "Male gender, yes", "White ethnicity, yes", 
          "Higher education, yes", "Current smoking, yes", "BMI (in kg/m²)", 
          "Hypertension, yes", "Diabetes, yes"), 
  # get means or N from observed data
  o.m = c(age$mean, gen$yes, eth$yes, edu$yes, smo$yes, bmi$mean, hyp$yes, 
          dia$yes),
  # get SDs or % from observed data
  o.sd = c(age$sd, po(gen), po(eth), po(edu), po(smo), bmi$sd, po(hyp), po(dia)),
  # get % missing from observed data
  o.miss = c(pm_c(age$n),pm_b(gen), pm_b(eth), pm_b(edu), pm_b(smo), 
             pm_c(bmi$n), pm_b(hyp), pm_b(dia)),
  # get pooled mean or N for imputed data
  i.pm = c(i.age$pooled_descriptives[1], i.gen$pooled_n, i.eth$pooled_n, 
           i.edu$pooled_n, i.smo$pooled_n, i.bmi$pooled_descriptives[1], 
           i.hyp$pooled_n, i.dia$pooled_n), 
  # get pooled SD or % for imputed data
  i.psd = c(i.age$pooled_descriptives[2],i.gen$pool_perc, i.eth$pool_perc, 
            i.edu$pool_perc, i.smo$pool_perc, i.bmi$pooled_descriptives[2], 
            i.hyp$pool_perc, i.dia$pool_perc), 
  # get SD of means or range for imputed data
  i.se = c(i.age$pooled_se$sd, i.gen$pool_range[1], i.eth$pool_range[1], 
           i.edu$pool_range[1], i.smo$pool_range[1], i.bmi$pooled_se$sd, 
           i.hyp$pool_range[1], i.dia$pool_range[1]),
  # get second value for range
  i.se2 = c(0, i.gen$pool_range[2], i.eth$pool_range[2], i.edu$pool_range[2], 
            i.smo$pool_range[2], 0, i.hyp$pool_range[2], i.dia$pool_range[2])), 
  class = "data.frame", 
  row.names = c(NA,-10L))


# --------------------------------
# 2) Check balance diagnostics
# --------------------------------

# 2.1) Summary of matching procedure in each data set
# -----------------------------------------------------

lapply(matchedlist, summary)

# 2.2) Figure S2
# ---------------

# Create and save plots of effect sizes before and after matching
for (i in seq(matchedlist)) {
  psm.bal             <- bal.tab(matchedlist[[i]], binary = "std", un = T)
  psm.bal$Balance$var <- seq.int(nrow(psm.bal$Balance))
  psm.bal             <- psm.bal$Balance[2:9,]
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
  axis(2, at = 1:8, labels = F, tck = 0)
  text(y = seq(1, 8, by = 1), par("usr")[1],
       labels = c("Hypertension", "BMI",
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


# 2.3) Additional figures: jitter plots 
# ---------------------------------------

for (i in seq(matchedlist)) {
  png(paste0("figures/jitter/",i,".png"), width = 6, height = 6, 
      units = 'in', res = 300)
  plot(matchedlist[[i]], type = "jitter")
  dev.off()
}


# 2.4) Additional figures: histograms 
# -------------------------------------

for (i in seq(matchedlist)) {
  png(paste0("figures/hist/",i,".png"), width = 6, height = 6, 
      units = 'in', res = 300)
  plot(matchedlist[[i]], type = "hist")
  dev.off()
}


# ---------------------------------------
#  3.) Descriptive statistics (Table 2)
# ---------------------------------------

# 3.1) Pooled means and SDs 
# -----------------------------

# 3.1.1) For observed, imputded data - by caseness 
# ----------------------------------------------------

# Calculate descriptives
obs.age <- describeBy_imp("w1_dhager",  impdat, binary = F, strokevar = "stroke")
obs.gen <- describeBy_imp("w1_dhsex",   impdat, binary = T, strokevar = "stroke")
obs.eth <- describeBy_imp("w0_ethni",   impdat, binary = T, strokevar = "stroke")
obs.edu <- describeBy_imp("w0_educ",    impdat, binary = T, strokevar = "stroke")
obs.smo <- describeBy_imp("w1_heska",   impdat, binary = T, strokevar = "stroke")
obs.bmi <- describeBy_imp("w0_bmival",  impdat, binary = F, strokevar = "stroke")
obs.hyp <- describeBy_imp("w1_hypt",    impdat, binary = T, strokevar = "stroke")
obs.dia <- describeBy_imp("w1_diab",    impdat, binary = T, strokevar = "stroke")

# Show all descriptive stats in list
list("age"        = obs.age, 
     "gender"     = obs.gen, 
     "ethnicity"  = obs.eth, 
     "education"  = obs.edu, 
     "smoking"    = obs.smo, 
     "bmi"        = obs.bmi, 
     "hypert"     = obs.hyp, 
     "diabetes"   = obs.dia)


# 3.1.2) For matched, imputded data - by caseness 
# ----------------------------------------------------

# Unlist list of dataframe list to obtain dataframes in wide format and 
for (i in seq(matchlist))
  assign(paste0("data.match.w",i), matchlist[[i]]$data.match.w)

# Re-list them as mitml list
datamatchwlist <- 
  as.mitml.list(list(data.match.w1,  data.match.w2,  data.match.w3, 
                     data.match.w4,  data.match.w5,  data.match.w6, 
                     data.match.w7,  data.match.w8,  data.match.w9,  
                     data.match.w10, data.match.w11, data.match.w12, 
                     data.match.w13, data.match.w14, data.match.w15, 
                     data.match.w16, data.match.w17, data.match.w18, 
                     data.match.w19, data.match.w20))

# Rename list indices for mitml list
names(datamatchwlist) <- c("1" , "2" , "3" , "4" , "5" , "6" , "7" , "8" , 
                           "9" , "10", "11", "12", "13", "14", "15", "16", 
                           "17", "18", "19", "20")

# Stack data frames from list onto each other
match_stack <- 
  map2_df(datamatchwlist, 
          names(datamatchwlist), ~ mutate(.x, .imp = .y))

### Now finally, compute descriptives
mat.age <- describeBy_imp("w1_dhager", match_stack, binary = F, strokevar = "stroke")
mat.gen <- describeBy_imp("w1_dhsex",  match_stack, binary = T, strokevar = "stroke")
mat.eth <- describeBy_imp("w0_ethni",  match_stack, binary = T, strokevar = "stroke")
mat.edu <- describeBy_imp("w0_educ",   match_stack, binary = T, strokevar = "stroke")
mat.smo <- describeBy_imp("w1_heska",  match_stack, binary = T, strokevar = "stroke")
mat.bmi <- describeBy_imp("w0_bmival", match_stack, binary = F, strokevar = "stroke")
mat.hyp <- describeBy_imp("w1_hypt",   match_stack, binary = T, strokevar = "stroke")
mat.dia <- describeBy_imp("w1_diab",   match_stack, binary = T, strokevar = "stroke")

# Show all descriptive stats in list
list("age"        = mat.age, 
     "gender"     = mat.gen, 
     "ethnicity"  = mat.eth, 
     "education"  = mat.edu, 
     "smoking"    = mat.smo, 
     "bmi"        = mat.bmi, 
     "hypert"     = mat.hyp, 
     "diabetes"   = mat.dia)


# 3.2) Calculate SMD 
# ---------------------

# (note that we rely on the bal.tab() function from the cobalt package function 
# to be consistent with how matchit operates; more info here: 
# https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html)
# This is particularly important for the matched data, as it uses the sd 
# from the unmatched data to calculate the SMD!

# 3.2.1) For unmatched, imputded data 
# ----------------------------------------------------

# First chreate a list with the bal.tab() results, getting mean differences
std.list <- lapply(matchedlist, 
                  bal.tab, binary = "std", un = T)

# Unlist list of dataframe list to obtain dataframes and select relevant one
for (i in seq(std.list))
  assign(paste0("std.un",i), std.list[[i]]$Balance$Diff.Un)

# Re-list those data frames
std.un.list <- 
  list(std.un1,  std.un2,  std.un3,  std.un4,  std.un5,  std.un6,  std.un7,  
       std.un8,  std.un9,  std.un10, std.un11, std.un12, std.un13, std.un14, 
       std.un15, std.un16, std.un17, std.un18, std.un19, std.un20)

# Convert them to a matrix
std.un.mat <- laply(std.un.list, as.matrix)
# Make a dataframe out of the matrix
std.un.mat <- as.data.frame(std.un.mat)
# Give column names for variables again
names(std.un.mat)[2:9] <- c("age", "gender", "ethnicity", "edcation", "smoking", 
                             "diabetes",  "bmi", "hypert")
# Now calculate column means of mean standardised differences in unmatched data
colMeans(std.un.mat[2:9])


# 3.2.2) For matched, imputded data 
# ----------------------------------------------------

# First chreate a list with the bal.tab() results, getting mean differences
std.list <- lapply(matchedlist, 
                  bal.tab, binary = "std", un = T)

# Unlist list of dataframe list to obtain dataframes and select relevant one
for (i in seq(std.list))
  assign(paste0("std.ma",i), std.list[[i]]$Balance$Diff.Adj)

# Re-list those data frames
std.ma.list <- 
  list(std.ma1,  std.ma2,  std.ma3,  std.ma4,  std.ma5,  std.ma6,  std.ma7,  
       std.ma8,  std.ma9,  std.ma10, std.ma11, std.ma12, std.ma13, std.ma14, 
       std.ma15, std.ma16, std.ma17, std.ma18, std.ma19, std.ma20)

# Convert them to a matrix
std.ma.mat <- laply(std.ma.list, as.matrix)
# Make a dataframe out of the matrix
std.ma.mat <- as.data.frame(std.ma.mat)
# Give column names for variables again
names(std.ma.mat)[2:9] <- c("age", "gender", "ethnicity", "edcation", "smoking", 
                             "diabetes",  "bmi", "hypert")
# Now calculate column means of mean standardised differences in unmatched data
colMeans(std.ma.mat[2:9])


