# ===============================
#  Longitudinal data analysis
# ===============================

# Source helper functions and packages
source("analyses/helpers_describe.R")
# source("analyses/00_prepare.R")

# Load imputed and matched data 
load("data/processed/proc_data.RData") # preprocessed
load("data/processed/matched_data.RData") # matched 
load("data/processed/imp_data.RData") # imputed
impdat <- mice::complete(imp, action = "long", include = FALSE)

# IMPORTANT: Always run the "00_prepare.R" script to load all packages used here! 

# ----------------------------------------------
# 1.) Data post-processing (for main analysis) 
# ----------------------------------------------

# 1.1.) Create data frame with all data and matched cases
# --------------------------------------------------------

# Extract matched matrix (contains matched cases) 
fun_mm <- function(sample){
  mm <- data.frame(sample$match.matrix)
  mml <- list(mm = mm)
  return(mml)
}

# Put all match matrixes in a list
mmlist <- lapply(matchedlist, fun_mm)

# Make each rownumber (indicates stroke case) a variable in list of match matrix
for( i in seq_along(mmlist)){
  mmlist[[i]]$mm$matched_cases <- as.numeric(rownames(mmlist[[i]]$mm))
}

# Make each data frame in the list to long format
for( i in seq_along(mmlist)){
  mmlist[[i]]$mm <- reshape2::melt(mmlist[[i]]$mm, 
                                   id.vars = "matched_cases", 
                                   value.name = "rowid2")
}
# the rowids in the matched.matrix are taken from the original data with 10810 obs... 
# --> this is not the same as the rowids in the matched data!
# --> implist and imputed data go into the matching procedure --> have to use those!

# Make each rownumber (indicates stroke case) a variable in implist
for( i in seq_along(implist)){
  implist[[i]]$rowid <- as.numeric(rownames(implist[[i]]))
}

# Merge match matrix with implist (data before matching without outcomes)
data01 <- merge(mmlist[[1]]$mm , implist[[1]] , by.x = "rowid2", by.y = "rowid", all.y = T)
data02 <- merge(mmlist[[2]]$mm , implist[[2]] , by.x = "rowid2", by.y = "rowid", all.y = T)
data03 <- merge(mmlist[[3]]$mm , implist[[3]] , by.x = "rowid2", by.y = "rowid", all.y = T)
data04 <- merge(mmlist[[4]]$mm , implist[[4]] , by.x = "rowid2", by.y = "rowid", all.y = T)
data05 <- merge(mmlist[[5]]$mm , implist[[5]] , by.x = "rowid2", by.y = "rowid", all.y = T)
data06 <- merge(mmlist[[6]]$mm , implist[[6]] , by.x = "rowid2", by.y = "rowid", all.y = T)
data07 <- merge(mmlist[[7]]$mm , implist[[7]] , by.x = "rowid2", by.y = "rowid", all.y = T)
data08 <- merge(mmlist[[8]]$mm , implist[[8]] , by.x = "rowid2", by.y = "rowid", all.y = T)
data09 <- merge(mmlist[[9]]$mm , implist[[9]] , by.x = "rowid2", by.y = "rowid", all.y = T)
data10 <- merge(mmlist[[10]]$mm, implist[[10]], by.x = "rowid2", by.y = "rowid", all.y = T)
data11 <- merge(mmlist[[11]]$mm, implist[[11]], by.x = "rowid2", by.y = "rowid", all.y = T)
data12 <- merge(mmlist[[12]]$mm, implist[[12]], by.x = "rowid2", by.y = "rowid", all.y = T)
data13 <- merge(mmlist[[13]]$mm, implist[[13]], by.x = "rowid2", by.y = "rowid", all.y = T)
data14 <- merge(mmlist[[14]]$mm, implist[[14]], by.x = "rowid2", by.y = "rowid", all.y = T)
data15 <- merge(mmlist[[15]]$mm, implist[[15]], by.x = "rowid2", by.y = "rowid", all.y = T)
data16 <- merge(mmlist[[16]]$mm, implist[[16]], by.x = "rowid2", by.y = "rowid", all.y = T)
data17 <- merge(mmlist[[17]]$mm, implist[[17]], by.x = "rowid2", by.y = "rowid", all.y = T)
data18 <- merge(mmlist[[18]]$mm, implist[[18]], by.x = "rowid2", by.y = "rowid", all.y = T)
data19 <- merge(mmlist[[19]]$mm, implist[[19]], by.x = "rowid2", by.y = "rowid", all.y = T)
data20 <- merge(mmlist[[20]]$mm, implist[[10]], by.x = "rowid2", by.y = "rowid", all.y = T)

# Make dataframes a list again
datalist <- list(data01, data02, data03, data04, data05, data06, data07, data08, 
                 data09, data10, data11, data12, data13, data14, data15, data16,
                 data17, data18, data19, data20)

# Some data wrangling across all data frames in list
for( i in seq_along(datalist)){
  # Convert matched_cases to numeric
  datalist[[i]]$matched_cases <- as.numeric(as.character(datalist[[i]]$matched_cases))
  # Convert rowid to numeric
  datalist[[i]]$rowid2        <- as.numeric(as.character(datalist[[i]]$rowid2))
  # Replace missing values matched_cases for stroke cases
  datalist[[i]]$matched_cases <- ifelse(datalist[[i]]$rowid2 < 426, 
                                        datalist[[i]]$rowid2, datalist[[i]]$matched_cases)
  # Exclude rows that weren't matched
  datalist[[i]] <- datalist[[i]][complete.cases(datalist[[i]][ , "matched_cases"]),]
}

# Now merge data with data-red, which contains the outcome data! :-)  
data_final01 <- merge(datalist[[1]] , data_red, by = "idauniq", all.x = T)
data_final02 <- merge(datalist[[2]] , data_red, by = "idauniq", all.x = T)
data_final03 <- merge(datalist[[3]] , data_red, by = "idauniq", all.x = T)
data_final04 <- merge(datalist[[4]] , data_red, by = "idauniq", all.x = T)
data_final05 <- merge(datalist[[5]] , data_red, by = "idauniq", all.x = T)
data_final06 <- merge(datalist[[6]] , data_red, by = "idauniq", all.x = T)
data_final07 <- merge(datalist[[7]] , data_red, by = "idauniq", all.x = T)
data_final08 <- merge(datalist[[8]] , data_red, by = "idauniq", all.x = T)
data_final09 <- merge(datalist[[9]] , data_red, by = "idauniq", all.x = T)
data_final10 <- merge(datalist[[10]], data_red, by = "idauniq", all.x = T)
data_final11 <- merge(datalist[[11]], data_red, by = "idauniq", all.x = T)
data_final12 <- merge(datalist[[12]], data_red, by = "idauniq", all.x = T)
data_final13 <- merge(datalist[[13]], data_red, by = "idauniq", all.x = T)
data_final14 <- merge(datalist[[14]], data_red, by = "idauniq", all.x = T)
data_final15 <- merge(datalist[[15]], data_red, by = "idauniq", all.x = T)
data_final16 <- merge(datalist[[16]], data_red, by = "idauniq", all.x = T)
data_final17 <- merge(datalist[[17]], data_red, by = "idauniq", all.x = T)
data_final18 <- merge(datalist[[18]], data_red, by = "idauniq", all.x = T)
data_final19 <- merge(datalist[[19]], data_red, by = "idauniq", all.x = T)
data_final20 <- merge(datalist[[20]], data_red, by = "idauniq", all.x = T)

# Make a dataframe list again, just for fun (well, not really)
finallist <- list(data_final01, data_final02, data_final03, data_final04, 
                  data_final05, data_final06, data_final07, data_final08, 
                  data_final09, data_final10, data_final11, data_final12, 
                  data_final13, data_final14, data_final15, data_final16, 
                  data_final17, data_final18, data_final19, data_final20)

# Some data wrangling across all data frames in list
for( i in seq_along(finallist)){
  # Order rows to have all matched cases in subsequent lines
  finallist[[i]] <- finallist[[i]][order(finallist[[i]]$matched_cases),]
  finallist[[i]] <- finallist[[i]] %>% fill(w1_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w2_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w3_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w4_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w5_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w6_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w7_time)
}


# 1.2.) Shape to long format 
# ----------------------------

for( i in seq_along(finallist)){
  # reshape each data frame into long format for modelling
  finallist[[i]] <- 
    reshape(finallist[[i]], 
            direction = "long", 
            idvar = "idauniq", 
            varying = list(c("w1_hedia"  , "w2_hediast", "w3_hediast", "w4_hediast", 
                             "w5_hediast", "w6_hediast", "w7_hediast"),
                           grep("time"   , colnames(finallist[[i]]), value = T), 
                           grep("dep_sum", colnames(finallist[[i]]), value = T), 
                           grep("adl_sum", colnames(finallist[[i]]), value = T), 
                           grep("mem_sum", colnames(finallist[[i]]), value = T), 
                           grep("psceda" , colnames(finallist[[i]]), value = T), 
                           grep("pscedb" , colnames(finallist[[i]]), value = T),
                           grep("pscedc" , colnames(finallist[[i]]), value = T),
                           grep("pscedd" , colnames(finallist[[i]]), value = T),
                           grep("pscede" , colnames(finallist[[i]]), value = T),
                           grep("pscedf" , colnames(finallist[[i]]), value = T),
                           grep("pscedg" , colnames(finallist[[i]]), value = T),
                           grep("pscedh" , colnames(finallist[[i]]), value = T)
            ), 
            timevar = "wave", 
            times = c("w1", "w2", "w3", "w4", "w5", "w6", "w7"), 
            v.names = c("stroke", "time", "depress", "adl", "mem", "dep",  
                        "eff", "sle", "hap", "lon", "enj", "sad", "goi"
            ))
  # make stroke variable a factor for the model
  finallist[[i]]$stroke.x <- as.factor(finallist[[i]]$stroke.x)
  }
  

# 1.3.) Code parameter variables 
# --------------------------------

for( i in seq_along(finallist)){
  # code slope before stroke from time variable
  finallist[[i]] <- finallist[[i]] %>% 
    mutate(pre = dplyr::recode(time, 
                               `-6`= 0, 
                               `-5`= 0,
                               `-4`= 0,
                               `-3`= 0, 
                               `-2`= 0, 
                               `-1`= 1, 
                               `0`= 0,
                               `1`= 0, 
                               `2`= 0, 
                               `3`= 0, 
                               `4`= 0, 
                               `5`= 0))
  # code jump intercept at stroke from time variable
  finallist[[i]] <- finallist[[i]] %>% 
    mutate(post = dplyr::recode(time, 
                                `-6`= 0,
                                `-5`= 0,
                                `-4`= 0,
                                `-3`= 0, 
                                `-2`= 0, 
                                `-1`= 0, 
                                `0`= 1,
                                `1`= 1, 
                                `2`= 1, 
                                `3`= 1, 
                                `4`= 1, 
                                `5`= 1))
  # code slope after stroke from time variable
  finallist[[i]] <- finallist[[i]] %>% 
    mutate(post_lin = dplyr::recode(time, 
                                   `-6`= 0,
                                   `-5`= 0,
                                   `-4`= 0,
                                   `-3`= 0, 
                                   `-2`= 0, 
                                   `-1`= 0, 
                                   `0`= 0,
                                   `1`= 1, 
                                   `2`= 2, 
                                   `3`= 3, 
                                   `4`= 4, 
                                   `5`= 5))
  # dummy -6
  finallist[[i]] <- finallist[[i]] %>% 
    mutate(dummy_minus6 = dplyr::recode(time, 
                                        `-6`= 1,
                                        `-5`= 0,
                                        `-4`= 0,
                                        `-3`= 0, 
                                        `-2`= 0, 
                                        `-1`= 0, 
                                        `0`= 0,
                                        `1`= 0, 
                                        `2`= 0, 
                                        `3`= 0, 
                                        `4`= 0, 
                                        `5`= 0))
  # dummy -5
  finallist[[i]] <- finallist[[i]] %>% 
    mutate(dummy_minus5 = dplyr::recode(time, 
                                        `-6`= 0,
                                        `-5`= 1,
                                        `-4`= 0,
                                        `-3`= 0, 
                                        `-2`= 0, 
                                        `-1`= 0, 
                                        `0`= 0,
                                        `1`= 0, 
                                        `2`= 0, 
                                        `3`= 0, 
                                        `4`= 0, 
                                        `5`= 0))
  # dummy -4
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_minus4 = dplyr::recode(time, 
                                        `-6`= 0,
                                        `-5`= 0,
                                        `-4`= 1,
                                        `-3`= 0, 
                                        `-2`= 0, 
                                        `-1`= 0, 
                                        `0`= 0,
                                        `1`= 0, 
                                        `2`= 0, 
                                        `3`= 0, 
                                        `4`= 0, 
                                        `5`= 0))
  # dummy -3
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_minus3 = dplyr::recode(time, 
                                        `-6`= 0,
                                        `-5`= 0,
                                        `-4`= 0,
                                        `-3`= 1, 
                                        `-2`= 0, 
                                        `-1`= 0, 
                                        `0`= 0,
                                        `1`= 0, 
                                        `2`= 0, 
                                        `3`= 0, 
                                        `4`= 0, 
                                        `5`= 0))
  # dummy -2
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_minus2 = dplyr::recode(time, 
                                        `-6`= 0,
                                        `-5`= 0,
                                        `-4`= 0,
                                        `-3`= 0, 
                                        `-2`= 1, 
                                        `-1`= 0, 
                                        `0`= 0,
                                        `1`= 0, 
                                        `2`= 0, 
                                        `3`= 0, 
                                        `4`= 0, 
                                        `5`= 0))
  # dummy -1
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_minus1 = dplyr::recode(time, 
                                        `-6`= 0,
                                        `-5`= 0,
                                        `-4`= 0,
                                        `-3`= 0, 
                                        `-2`= 0, 
                                        `-1`= 1, 
                                        `0`= 0,
                                        `1`= 0, 
                                        `2`= 0, 
                                        `3`= 0, 
                                        `4`= 0, 
                                        `5`= 0))
  
  # dummy 0
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_zero = dplyr::recode(time, 
                                        `-6`= 0,
                                        `-5`= 0,
                                        `-4`= 0,
                                        `-3`= 0, 
                                        `-2`= 0, 
                                        `-1`= 0, 
                                        `0`= 1,
                                        `1`= 0, 
                                        `2`= 0, 
                                        `3`= 0, 
                                        `4`= 0, 
                                        `5`= 0))
  # dummy +1
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_plus1 = dplyr::recode(time, 
                                       `-6`= 0,
                                       `-5`= 0,
                                       `-4`= 0,
                                       `-3`= 0, 
                                       `-2`= 0, 
                                       `-1`= 0, 
                                       `0`= 0,
                                       `1`= 1, 
                                       `2`= 0, 
                                       `3`= 0, 
                                       `4`= 0, 
                                       `5`= 0))
  # dummy +2
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_plus2 = dplyr::recode(time, 
                                       `-6`= 0,
                                       `-5`= 0,
                                       `-4`= 0,
                                       `-3`= 0, 
                                       `-2`= 0, 
                                       `-1`= 0, 
                                       `0`= 0,
                                       `1`= 0, 
                                       `2`= 1, 
                                       `3`= 0, 
                                       `4`= 0, 
                                       `5`= 0))
  # dummy +3
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_plus3 = dplyr::recode(time, 
                                       `-6`= 0,
                                       `-5`= 0,
                                       `-4`= 0,
                                       `-3`= 0, 
                                       `-2`= 0, 
                                       `-1`= 0, 
                                       `0`= 0,
                                       `1`= 0, 
                                       `2`= 0, 
                                       `3`= 1, 
                                       `4`= 0, 
                                       `5`= 0))
  # dummy +4
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_plus4 = dplyr::recode(time, 
                                       `-6`= 0,
                                       `-5`= 0,
                                       `-4`= 0,
                                       `-3`= 0, 
                                       `-2`= 0, 
                                       `-1`= 0, 
                                       `0`= 0,
                                       `1`= 0, 
                                       `2`= 0, 
                                       `3`= 0, 
                                       `4`= 1, 
                                       `5`= 0))
  # dummy +5
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_plus5 = dplyr::recode(time, 
                                       `-6`= 0,
                                       `-5`= 0,
                                       `-4`= 0,
                                       `-3`= 0, 
                                       `-2`= 0, 
                                       `-1`= 0, 
                                       `0`= 0,
                                       `1`= 0, 
                                       `2`= 0, 
                                       `3`= 0, 
                                       `4`= 0, 
                                       `5`= 1))
  # code jump intercept at stroke from time variable
  finallist[[i]] <- finallist[[i]] %>% 
    mutate(stroke.recode = dplyr::recode_factor(stroke.x, 
                                `1`= 0,
                                `0`= 1))
}


# 1.4.) Create mitml list again for analyses
# ---------------------------------------------

# Next, unlist list of dataframe list to obtain dataframes in long format 
for (i in seq(finallist))
  assign(paste0("data_final_long",i), finallist[[i]])

# Finally, re-list them as mitml list for pooling
finallist_model <- 
  as.mitml.list(list(data_final_long1,  data_final_long2,  data_final_long3,  
                     data_final_long4,  data_final_long5,  data_final_long6 , 
                     data_final_long7,  data_final_long8,  data_final_long9,  
                     data_final_long10, data_final_long11, data_final_long12, 
                     data_final_long13, data_final_long14, data_final_long15, 
                     data_final_long16, data_final_long17, data_final_long18, 
                     data_final_long19, data_final_long20)) 

# 1.5.) Check how many controls and strokes have data for each time point
# --------------------------------------------------------------------------

# How many participants in principle after re-structuring
table(finallist[[1]]$time, finallist[[1]]$stroke.x, useNA = "always")

# How many with outcome data (mean depressive symptoms) at each time point?

# First, select depression items
final_dep_items <- 
  finallist[[1]][,c("dep", "eff", "sle", "hap", "lon", "enj", "sad", "goi")] 

# Then, calculate sum score with sinlge missings (only for here to get n!)
finallist[[1]]$depr_n <- rowMeans(final_dep_items, na.rm = T) 

# Finally, describe data to get Ns
describeBy(finallist[[1]]$depr_n, list(finallist[[1]]$time, finallist[[1]]$stroke.x))


# -------------------------------------
# 2.) Longitudinal model - only stroke
# -------------------------------------

# subset stroke data 
# (note: only need 1 long data set, all are the same for stroke)
data_stroke <- subset(data_final_long1, stroke.x == 1)

data_stroke <- subset(finallist[[1]], stroke.x == 1)


# 2.1) Model 1: without linear post-change
# ------------------------------------------

# Define model
m.1 <- depress ~ 1 + pre + post + (1 + pre + post | idauniq)

# Fit model
fit.1 <- lmer(m.1, control = lmerControl(optimizer = "bobyqa"), 
              REML = T, data = data_stroke)

# get resutls
summary(fit.1)
confint(fit.1, level = 0.95, method = "Wald")


# 2.2) Model 2: with linear post-change
# ---------------------------------------

# Define model
m.2 <- depress ~ 1 + pre + post + post_lin + 
       (1 + pre + post + post_lin | idauniq)

# Fir model
fit.2 <- lmer(m.2, control = lmerControl(optimizer = "bobyqa"), 
              REML = T, data = data_stroke)

# get resutls
summary(fit.2)
confint(fit.2, level = 0.95, method = "Wald")


# 2.3) Model comparisons
# ----------------------------

# get AIC values
AIC(fit.1, fit.2)


# ----------------------------------------
# 3.) Longitudinal model - with controls
# ----------------------------------------

# Note that the model was first fitted with no stroke as reference group and 
# the results of this model were also used for plotting (see code below). 
# I still use this model for plotting (fit.dep.pool). 
# To report results in the manuscript, I however, decided to refit the model 
# with stroke as reference group, so I can report results of all models 
# nicely in one table. The results of fit.dep.pool.recode are therefore 
# reported in the manuscript.

# 3.1) Model for plotting (no strokes as reference)
# -------------------------------------------------

# Define and fit model
fit.dep <- with(finallist_model, 
                  lmer(depress ~ 1 + pre + post + 
                         stroke.x + stroke.x*pre + stroke.x*post +
                         (1 + pre + post | idauniq), 
                       control = lmerControl(optimizer = "bobyqa"), 
                       REML = T))

# Pooling (Rubin's rules)
fit.dep.pool <- testEstimates(fit.dep) 

# Get 95% confidence intervals for estimates
cis.dep.pool <- confint(fit.dep.pool, level = 0.95)


# 3.2) Model for table (strokes as reference)
# -------------------------------------------------

# Define and fit model
fit.dep.recode <- with(finallist_model, 
                lmer(depress ~ 1 + pre + post + 
                       stroke.recode + stroke.recode*pre + stroke.recode*post +
                       (1 + pre + post | idauniq), 
                     control = lmerControl(optimizer = "bobyqa"), 
                     REML = T))

# Pooling (Rubin's rules)
fit.dep.recode.pool <- testEstimates(fit.dep.recode) 
fit.dep.recode.pool

# Get 95% confidence intervals for estimates
cis.dep.recode.pool <- confint(fit.dep.recode.pool, level = 0.95)
cis.dep.recode.pool


# ----------------------------------------------
# 4.) Depression Percentage, per group
# ----------------------------------------------

# Create new variable indicating whether people are > 3
for( i in seq_along(finallist)){
  # code binary varibale indicating probable depression
  finallist[[i]] <- finallist[[i]] %>% 
    mutate(dep_bin = ifelse(depress > 2, 1, 0))
}

# Time -3 
# ----------

# Create list of dataframes that only have time = -3 data
data_list_m3 <- lapply(finallist, subset, time == -3 & 
                         (stroke.x == 1 | stroke.x == 0))

# Percentages 
dep_stat_m3 <- dep_stat(data_list_m3)
dep_stat_m3

# combine pooled percentage values to data frame
dep_res_m3 <- as.data.frame(c(dep_stat_m3[[1]]*100, dep_stat_m3[[2]]*100))
names(dep_res_m3) <- "percent"

# new variable indicating group
dep_res_m3$group <- c("stroke", "controls")

# Cramer's V
mean(dep_cramv(data_list_m3))


# Time -1
# ----------

# Create list of dataframes that only have time = -1 data
data_list_m1 <- lapply(finallist, subset, time == -1 & 
                         (stroke.x == 1 | stroke.x == 0))

# Percentages 
dep_stat_m1 <- dep_stat(data_list_m1)
dep_stat_m1

# combine pooled percentage values to data frame
dep_res_m1 <- as.data.frame(c(dep_stat_m1[[1]]*100, dep_stat_m1[[2]]*100))
names(dep_res_m1) <- "percent"

# new variable indicating group
dep_res_m1$group <- c("stroke", "controls")

# Cramer's V
mean(dep_cramv(data_list_m1))


# Time 0
# ----------

# Create list of dataframes that only have time = 0 data
data_list_0 <- lapply(finallist, subset, time == 0 & 
                        (stroke.x == 1 | stroke.x == 0))

# Percentages 
dep_stat_0 <- dep_stat(data_list_0)
dep_stat_0

# combine pooled percentage values to data frame
dep_res_0 <- as.data.frame(c(dep_stat_0[[1]]*100, dep_stat_0[[2]]*100))
names(dep_res_0) <- "percent"

# new variable indicating group
dep_res_0$group <- c("stroke", "controls")

# Cramer's V
mean(dep_cramv(data_list_0))


# Time 3
# ----------

# Create list of dataframes that only have time = 3 data
data_list_p3 <- lapply(finallist, subset, time == 3 & 
                         (stroke.x == 1 | stroke.x == 0))

# Percentages 
dep_stat_p3 <- dep_stat(data_list_p3)
dep_stat_p3

# combine pooled percentage values to data frame
dep_res_p3 <- as.data.frame(c(dep_stat_p3[[1]]*100, dep_stat_p3[[2]]*100))
names(dep_res_p3) <- "percent"

# new variable indicating group
dep_res_p3$group <- c("stroke", "controls")

# Cramer's V
mean(dep_cramv(data_list_p3))


# -------------------------
# 5.) Plots
# --------------------------

# 5.1) Figure 1A: Trajectories
# ------------------------------

# Create object with pooled model fixed effects
est <- fit.dep.pool$estimates[1:6]
cil <- cis.dep.pool[1:6]

# Make data frame for all fixes effects
fe <- data.frame(est, cil)

# Name pooled effects
fe$st.case <- c("Controls", "Controls", "Controls", 
                "Stroke", "Stroke", "Stroke")
fe$time    <- c(-10, -2, 0, -10, -2, 0)

# Calculate diff between est and ci
fe$ci[1] <- fe$est[1] - fe$cil[1]
fe$ci[2] <- fe$est[2] - fe$cil[2]
fe$ci[3] <- fe$est[3] - fe$cil[3]
fe$ci[4] <- fe$est[4] - fe$cil[4]
fe$ci[5] <- fe$est[5] - fe$cil[5]
fe$ci[6] <- fe$est[6] - fe$cil[6]

# Calculate fixed effects for plotting
fe$est[5] <- fe$est[1] + fe$est[2] + fe$est[4] + fe$est[5] 
fe$est[6] <- fe$est[1] + fe$est[3] + fe$est[4] + fe$est[6] 
fe$est[2] <- fe$est[1] + fe$est[2] 
fe$est[3] <- fe$est[1] + fe$est[3] 
fe$est[4] <- fe$est[1] + fe$est[4]  

# Make new data frame with remaining effects
fe.2 <- 
  data.frame(est = c(fe$est[1], fe$est[1], fe$est[1], fe$est[3], fe$est[3], 
                     fe$est[3], fe$est[3], fe$est[3], fe$est[4], fe$est[4], 
                     fe$est[4], fe$est[6], fe$est[6], fe$est[6], fe$est[6], 
                     fe$est[6]), 
             st.case = c("Controls", "Controls", "Controls", "Controls", 
                         "Controls", "Controls", "Controls", "Controls", 
                         "Stroke", "Stroke", "Stroke", "Stroke", 
                         "Stroke", "Stroke", "Stroke", "Stroke"), 
             time = c(-8,-6,-4,2,4,6,8,10, -8,-6,-4,2,4,6,8,10), 
             ci  = c(fe$ci[1], fe$ci[1], fe$ci[1], fe$ci[3], fe$ci[3], 
                     fe$ci[3], fe$ci[3], fe$ci[3], fe$ci[4], fe$ci[4], 
                     fe$ci[4], fe$ci[6], fe$ci[6], fe$ci[6], fe$ci[6], 
                     fe$ci[6]))

# Bind both data frames together
fe <- rbind(fe[,c(1,3:5)], fe.2)

# Draw figure
ggplot(fe, aes(x = time, y = est)) +
  geom_ribbon(aes(ymin = est-ci, ymax = est + ci, fill = st.case), alpha = 0.3) +
  geom_line(aes(colour = st.case), size = 1.2)  +
  geom_vline(xintercept = -1, linetype = "dashed", color = "black", size = 0.5) +
  labs(y = "Depressive symptoms", x = "Time to stroke (in years)") +
  scale_colour_manual(values = rev(c("#b7b7b7", "#d4738b")),
                      breaks = rev(c("Controls", "Stroke")),
                      labels = rev(c("Controls ", "Stroke survivors "))) +
  scale_fill_manual(values = rev(c("#b7b7b7", "#d4738b")),
                      breaks = rev(c("Controls", "Stroke")),
                      labels = rev(c("Controls ", "Stroke survivors ")), 
                    guide = "none") +
  coord_cartesian(ylim = c(1,2.6)) +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = c(1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6)) +
  scale_x_continuous(breaks = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10)) +
  theme_classic(base_size = 15)+
  theme(axis.title.x = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),         
        axis.text.x  = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 15, r = 0, b = 0, l = 0)), 
        axis.text.y  = element_text(colour = "black", size = 19), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.text  = element_text(colour = "black", size = 19), 
        legend.title = element_blank(),
        legend.key.size = unit(0.9,"cm"),
        legend.position = c(0.15,0.9))

# Save figure
ggsave("figures/figure_1a.pdf", width = 11.3, height = 6, units = "in")


# 5.2) Figure 1B: Percentages
# ------------------------------

# Create generic plotting function
plot_perc <- function(data, title, xlabels) {
  ggplot(data = data, aes(y = percent, x = group, fill = group)) + 
    geom_bar(position = position_dodge(), stat = "identity", size = .3, 
             show.legend = F) +
    theme_bw() +
    theme(panel.border     = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x     = element_text(size = 19, colour = "black"),
          axis.line        = element_line(colour = "black"),
          axis.text.y      = element_text(size = 19, colour = "black"),
          axis.text.x      = element_blank(),
          legend.text      = element_text(size = 19), 
          plot.title       = element_text(hjust = 0.5, size = 19), 
          axis.ticks.x     = element_blank(), 
          axis.title.y     = element_text(colour = "black", size = 19, 
                                          margin = margin(t = 0, r = 15, 
                                                          b = 0, l = 0))) +
    labs(x = title, y = "Prevalence (in %)") +
    scale_x_discrete(breaks = c("controls", "stroke"),
                     labels = xlabels) +
    scale_y_continuous(expand = c(0,0), limits = c(0,43),
                       breaks = c(0,10,20,30,40)) +
    scale_fill_manual(values = c('#d3d3d3','#8b4166'), name = "")
}

# Time -3
plot_perc(data = dep_res_m3, title = "", 
          xlabels = c("",""))
ggsave("figures/figure_1b_1.pdf", width = 3, height = 3.5, units = "in")

# Time -1
plot_perc(data = dep_res_m1, title = "", 
          xlabels = c("",""))
ggsave("figures/figure_1b_2.pdf", width = 3, height = 3.5, units = "in")

# Time 0
plot_perc(data = dep_res_0, title = "", 
          xlabels = c("",""))
ggsave("figures/figure_1b_3.pdf", width = 3, height = 3.5, units = "in")

# Time +3
plot_perc(data = dep_res_p3, title = "", 
          xlabels = c("",""))
ggsave("figures/figure_1b_4.pdf", width = 3, height = 3.5, units = "in")


