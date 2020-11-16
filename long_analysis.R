## =================================
##  Longitudinal data analysis
## =================================

# load packages
library(ggplot2)
library(dplyr)
library(tidyr)  # for fill()
library(Rmisc) # for summarySEwithin()
library(reshape)
library(lme4)
library(lmerTest)
library(car) # for recode()
library(arm) # for se.fixef()
library(mitml)


# Load imputed and matched data 
load("data/processed/proc_data.RData") # preprocessed
load("data/processed/matched_data.RData") # matched 
load("data/processed/imp_data.RData") # imputed
impdat <- mice::complete(imp, action = "long", include = FALSE)


# ----------------------------------------------
# 1.) Data post-processing (for main analysis) 
# ----------------------------------------------

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
  datalist[[i]]$matched_cases <- ifelse(datalist[[i]]$rowid2 < 376, 
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
finallist <- list(data_final01, data_final02, data_final03, data_final04, data_final05,
                  data_final06, data_final07, data_final08, data_final09, data_final10,
                  data_final11, data_final12, data_final13, data_final14, data_final15,
                  data_final16, data_final17, data_final18, data_final19, data_final20)

# Some data wrangling across all data frames in list
for( i in seq_along(finallist)){
  # Order rows to have all matched cases in subsequent lines
  finallist[[i]] <- finallist[[i]][order(finallist[[i]]$matched_cases),] 
  finallist[[i]] <- finallist[[i]] %>% fill(w2_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w3_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w4_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w5_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w6_time)
  finallist[[i]] <- finallist[[i]] %>% fill(w7_time)
}

# delete one depression w2 column
for( i in seq_along(finallist)){
  finallist[[i]] <- dplyr::select(finallist[[i]], -w2_dep_sum.x)
}

# shape to long format
for( i in seq_along(finallist)){
  # reshape each data frame into long format for modelling
  finallist[[i]] <- reshape(finallist[[i]], 
                            direction = "long", 
                            idvar = "idauniq", 
                            varying = list(c("w2_hediast", "w3_hediast", "w4_hediast", "w5_hediast", 
                                             "w6_hediast", "w7_hediast"),
                                           grep("time",     colnames(finallist[[i]]), value = T), 
                                           grep("dep_sum",  colnames(finallist[[i]]), value = T), 
                                           grep("swl_mean", colnames(finallist[[i]]), value = T), 
                                           grep("lon_mean", colnames(finallist[[i]]), value = T), 
                                           grep("adl_mean", colnames(finallist[[i]]), value = T)#, 
                                           # grep("psceda",   colnames(finallist[[i]]), value = T), 
                                           # grep("pscedb",   colnames(finallist[[i]]), value = T),
                                           # grep("pscedd",   colnames(finallist[[i]]), value = T),
                                           # grep("pscede",   colnames(finallist[[i]]), value = T),
                                           # grep("pscedf",   colnames(finallist[[i]]), value = T),
                                           # grep("pscedg",   colnames(finallist[[i]]), value = T),
                                           # grep("pscedh",   colnames(finallist[[i]]), value = T)
                            ), 
                            timevar = "wave", 
                            times = c("w2", "w3", "w4", "w5", "w6", "w7"), 
                            v.names = c("stroke", "time", "depress", "lifesat", "loneli", "adl", 
                                        "dep"#, "eff", "hap", "lon", "enj", "sad", "goi"
                            ))
  # make stroke variable a factor for the model
  finallist[[i]]$n_strokes.x <- as.factor(finallist[[i]]$n_strokes.x)
  # code slope before stroke from time variable
  finallist[[i]] <- finallist[[i]] %>% 
    mutate(slope_1 = dplyr::recode(time, 
                                   `-4`= 0,
                                   `-3`= 1, 
                                   `-2`= 2, 
                                   `-1`= 3, 
                                   `0`= 3,
                                   `1`= 3, 
                                   `2`= 3, 
                                   `3`= 3, 
                                   `4`= 3, 
                                   `5`= 3))
  # code jump intercept at stroke from time variable
  finallist[[i]] <- finallist[[i]] %>% 
    mutate(jump = dplyr::recode(time, 
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
  # code jump intercept at stroke from time variable
  finallist[[i]] <- finallist[[i]] %>% 
    mutate(jump_2 = dplyr::recode(time, 
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
  # code slope after stroke from time variable
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(slope_2 = dplyr::recode(time, 
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
  # dummy -4
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_minus4 = dplyr::recode(time, 
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
  # dummy +1
  finallist[[i]] <- finallist[[i]] %>% # 
    mutate(dummy_plus1 = dplyr::recode(time, 
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
}




# -------------------
# Actual data plots
# -------------------

# plot depressive symptoms
# --------------------------
sum_dep <- summarySE(data_final_long6, 
                     measurevar = "depress", 
                     groupvars = c("n_strokes.x", "time"),
                     na.rm = T, conf.interval = .95)

# Make the graph with the 95% confidence interval
ggplot(sum_dep, aes(x = time, y = depress, group = n_strokes.x, color = n_strokes.x)) +
  geom_errorbar(width = .1, aes(ymin = depress - ci, ymax = depress + ci)) +
  geom_point(shape = 21, size = 3) + 
  geom_line() +
  #ylim(1, 3) + 
  labs(y = "Depressive symptoms", x = "")


# plot adl
# ----------
sum_adl <- summarySE(data_final_long5, 
                     measurevar = "adl", 
                     groupvars = c("n_strokes.x", "time"),
                     na.rm = T, conf.interval = .95)

# Make the graph with the 95% confidence interval
ggplot(sum_adl, aes(x = time, y = adl, group = n_strokes.x, color = n_strokes.x)) +
  geom_errorbar(width = .1, aes(ymin = adl - ci, ymax = adl + ci)) +
  geom_point(shape = 21, size = 3) + 
  geom_line() +
  labs(y = "ADLs", x = "")



# ---------------------
# Longitudinal model
# ---------------------

# Data wrangling 
# ----------------

# First, unlist list of dataframe list to obtain dataframes in long format 
for (i in seq(finallist))
  assign(paste0("data_final_long",i), finallist[[i]])

# Then re-list them as mitml list for pooling
finallist_model <- as.mitml.list(list(data_final_long1,  data_final_long2 , 
                                      data_final_long3,  data_final_long4 , 
                                      data_final_long5,  data_final_long6 , 
                                      data_final_long7,  data_final_long8 , 
                                      data_final_long9,  data_final_long10,
                                      data_final_long11, data_final_long12, 
                                      data_final_long13, data_final_long14, 
                                      data_final_long15, data_final_long16, 
                                      data_final_long17, data_final_long18, 
                                      data_final_long19, data_final_long20)) 


# Model
# -----------------

# Fit random-intercept multilevel models and get pooled estimates
fit <- with(finallist_model,
            lmer(depress ~ 
                   1 + slope_1 + jump + slope_2 + n_strokes.x + n_strokes.x*slope_1 +
                   n_strokes.x*jump + n_strokes.x*slope_2 + 
                   (1 + jump + slope_2 | idauniq),
                 control = lmerControl(optimizer = "Nelder_Mead"), REML = F))

fit.pool <- testEstimates(fit) # Pooling (Rubin's rules)
fit.pool$estimates

# Get 95% confidence intervals for estimates
confint(fit.pool, level = 0.95)




# # ----------------------------
# # Model with dummy variables 
# # ---------------------------
# 
# ### Fit random-intercept multilevel models and get pooled estimates
# fit <- with(finallist_model, 
#             lmer(depress ~ 1 + 
#                    dummy_minus1 + 
#                    dummy_minus2 + 
#                    dummy_minus3 + 
#                    dummy_minus4 + 
#                    dummy_plus1 + 
#                    dummy_plus2 + 
#                    dummy_plus3 +
#                    dummy_plus4 + 
#                    dummy_plus5 +
#                    n_strokes.x + 
#                    n_strokes.x*dummy_minus1 + 
#                    n_strokes.x*dummy_minus2 + 
#                    n_strokes.x*dummy_minus3 + 
#                    n_strokes.x*dummy_minus4 + 
#                    n_strokes.x*dummy_plus1 + 
#                    n_strokes.x*dummy_plus2 + 
#                    n_strokes.x*dummy_plus3 +
#                    n_strokes.x*dummy_plus4 + 
#                    n_strokes.x*dummy_plus5
#                    + (1 | idauniq), REML = F)) 
# 
# fit.pool <- testEstimates(fit) # Pooling (Rubin's rules)
# fit.pool$estimates 
 

# ================= MODEL 1a ================================


m_Ia <- depress ~ 1 + slope_1 + jump + slope_2 + (1 + slope_1 + jump + slope_2 | idauniq)

fit_Ia <- lmer(m_Ia, data = data_long, control = lmerControl(optimizer = "Nelder_Mead"))
summary(fit_Ia)

# plot model
Ia <- allEffects(fit_Ia) 
plot(Ia)

# extract AIC
extractAIC(fit_Ia)


# ==================== MODEL 1b ============================
# random intercept, 1x random slope, lme4

m_Ib <- depress ~ 1 + slope_1 + jump + slope_2 + (1 + jump + slope_2 | idauniq)

fit_Ib <- lmer(m_Ib, data = data_long, control = lmerControl(optimizer = "Nelder_Mead"))
summary(fit_Ib)

# plot model
Ib <- allEffects(fit_Ib) 
plot(Ib)

# extract AIC
extractAIC(fit_Ib)

# ==================== MODEL 1c ============================
# all random nlme

fit_Ic <- lme(fixed = depress ~ 1 + slope_1 + jump + slope_2,
              data = data_long,
              random = ~ slope_1 + jump + slope_2 | idauniq, 
              na.action = na.omit) # all random failed to converge too
fit_Ic
anova(fit_Ic)



# ==================== MODEL 2a ============================
# different desing matrix, all random

data_long <- data_long %>% # slope before stroke
  mutate(slope_1 = dplyr::recode(time, 
                                 `-4`= 0,
                                 `-3`= 1, 
                                 `-2`= 2, 
                                 `-1`= 3, 
                                 `0`= 3,
                                 `1`= 3, 
                                 `2`= 3, 
                                 `3`= 3, 
                                 `4`= 3, 
                                 `5`= 3))

data_long <- data_long %>% # jump intercept at stroke
  mutate(jump = dplyr::recode(time, 
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

data_long <- data_long %>% # slope after stroke
  mutate(slope_2 = dplyr::recode(time, 
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

fit_IIa <- lmer(m_Ia, data = data_long, control = lmerControl(optimizer = "Nelder_Mead"))
summary(fit_IIa)

IIa <- allEffects(fit_IIa) # plot model
plot(IIa)

extractAIC(fit_IIa) # extract AIC


# ==================== MODEL 2b ============================
# different desing matrix, 1x random

fit_IIb <- lmer(m_Ib, data = data_long, control = lmerControl(optimizer = "Nelder_Mead"))
summary(fit_IIb)

IIb <- allEffects(fit_IIb) # plot model
plot(IIb)

extractAIC(fit_IIb) # extract AIC


library(nlme)
# ==================== MODEL 2c ============================
# all random, nlme

fit_IIc <- lme(fixed = depress ~ 1 + slope_1 + jump + slope_2,
               data = data_long,
               random = ~ jump + slope_2 | idauniq, 
               na.action = na.omit) # all random failed to converge, too

fit_IIc
anova(fit_IIc)
