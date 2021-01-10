## ===============================
##  Longitudinal data analysis
## ===============================

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
finallist <- list(data_final01, data_final02, data_final03, data_final04, 
                  data_final05, data_final06, data_final07, data_final08, 
                  data_final09, data_final10, data_final11, data_final12, 
                  data_final13, data_final14, data_final15, data_final16, 
                  data_final17, data_final18, data_final19, data_final20)

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
  finallist[[i]] <- dplyr::select(finallist[[i]], -c(w1_adl_sum.x, 
                                                     w1_mem_sum.x))
}

# shape to long format
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
}



# ---------------------
# Longitudinal model
# ---------------------

# Data wrangling 
# ----------------

# First, unlist list of dataframe list to obtain dataframes in long format 
for (i in seq(finallist))
  assign(paste0("data_final_long",i), finallist[[i]])

# Then re-list them as mitml list for pooling
finallist_model <- 
  as.mitml.list(list(data_final_long1,  data_final_long2,  data_final_long3,  
                     data_final_long4,  data_final_long5,  data_final_long6 , 
                     data_final_long7,  data_final_long8,  data_final_long9,  
                     data_final_long10, data_final_long11, data_final_long12, 
                     data_final_long13, data_final_long14, data_final_long15, 
                     data_final_long16, data_final_long17, data_final_long18, 
                     data_final_long19, data_final_long20)) 

# Model with dummy-coded variables
# ----------------------------------

# 1. Depressive symptoms
# ------------------------

# Fit random-intercept multilevel models and get pooled estimates
fit.dep <- with(finallist_model,
            lmer(depress ~ 1 +
                 #  dummy_minus6 +
                   dummy_minus5 +
                   dummy_minus4 +
                   dummy_minus3 +
                   dummy_minus2 +
                   dummy_minus1 +
                   dummy_plus1 +
                   dummy_plus2 +
                   dummy_plus3 +
                   dummy_plus4 +
                   dummy_plus5 +
                   stroke.x +
                  # n_strokes.x*dummy_minus6 +
                   stroke.x*dummy_minus5 +
                   stroke.x*dummy_minus4 +
                   stroke.x*dummy_minus3 +
                   stroke.x*dummy_minus2 +
                   stroke.x*dummy_minus1 +
                   stroke.x*dummy_plus1 +
                   stroke.x*dummy_plus2 +
                   stroke.x*dummy_plus3 +
                   stroke.x*dummy_plus4 +
                   stroke.x*dummy_plus5 + 
                   (1 | idauniq), 
                 REML = F))

fit.dep.pool <- testEstimates(fit.dep) # Pooling (Rubin's rules)
fit.dep.pool$estimates


# 2. Activities daily living
# ----------------------------

# Fit random-intercept multilevel models and get pooled estimates
fit.adl <- with(finallist_model,
                lmer(adl ~ 1 +
                       #  dummy_minus6 +
                       dummy_minus5 +
                       dummy_minus4 +
                       dummy_minus3 +
                       dummy_minus2 +
                       dummy_minus1 +
                       dummy_plus1 +
                       dummy_plus2 +
                       dummy_plus3 +
                       dummy_plus4 +
                       dummy_plus5 +
                       stroke.x +
                       # n_strokes.x*dummy_minus6 +
                       stroke.x*dummy_minus5 +
                       stroke.x*dummy_minus4 +
                       stroke.x*dummy_minus3 +
                       stroke.x*dummy_minus2 +
                       stroke.x*dummy_minus1 +
                       stroke.x*dummy_plus1 +
                       stroke.x*dummy_plus2 +
                       stroke.x*dummy_plus3 +
                       stroke.x*dummy_plus4 +
                       stroke.x*dummy_plus5 + 
                       (1 | idauniq), 
                     REML = F))

fit.adl.pool <- testEstimates(fit.adl) # Pooling (Rubin's rules)
fit.adl.pool$estimates


# 3. Memory functioning
# ----------------------------

# Fit random-intercept multilevel models and get pooled estimates
fit.mem <- with(finallist_model,
                lmer(mem ~ 1 +
                       #  dummy_minus6 +
                       dummy_minus5 +
                       dummy_minus4 +
                       dummy_minus3 +
                       dummy_minus2 +
                       dummy_minus1 +
                       dummy_plus1 +
                       dummy_plus2 +
                       dummy_plus3 +
                       dummy_plus4 +
                       dummy_plus5 +
                       stroke.x +
                       # n_strokes.x*dummy_minus6 +
                       stroke.x*dummy_minus5 +
                       stroke.x*dummy_minus4 +
                       stroke.x*dummy_minus3 +
                       stroke.x*dummy_minus2 +
                       stroke.x*dummy_minus1 +
                       stroke.x*dummy_plus1 +
                       stroke.x*dummy_plus2 +
                       stroke.x*dummy_plus3 +
                       stroke.x*dummy_plus4 +
                       stroke.x*dummy_plus5 + 
                       (1 | idauniq), 
                     REML = F))

fit.mem.pool <- testEstimates(fit.mem) # Pooling (Rubin's rules)
fit.mem.pool$estimates



# ----------------------
# Model with controls
# ----------------------

# 1.) Without random effects

# Fit random-intercept multilevel models and get pooled estimates
fit <- with(finallist_model,
            lmer(depress ~
                   1 + 
                   pre + 
                   post + 
                   stroke.x + 
                   stroke.x*pre +
                   stroke.x*post +
                   (1 + post | idauniq),
                 control = lmerControl(optimizer = "bobyqa"), REML = F))

fit.pool <- testEstimates(fit) # Pooling (Rubin's rules)
fit.pool$estimates

# Get 95% confidence intervals for estimates
confint(fit.pool, level = 0.95)


# 2.) With random effects

# Fit random-intercept multilevel models and get pooled estimates
fit <- with(finallist_model,
            lmer(depress ~
                   1 + pre + post + stroke.x + stroke.x*pre +
                   stroke.x*post +
                   (1 + pre + post | idauniq),
                 control = lmerControl(optimizer = "bobyqa"), REML = F))

fit.pool <- testEstimates(fit) # Pooling (Rubin's rules)
fit.pool$estimates

# Get 95% confidence intervals for estimates
confint(fit.pool, level = 0.95)


# 3.) saturated model

# I am not sure how to get AICs with the pooled models, so i will just compare 
# the fit using the data from only strokes, as below 


# -------------------------------------
# Model only strokes, random effects
# -------------------------------------

# subset stroke data 
# (note: only need 1 long data set, all are the same for stroke)
data_stroke <- subset(data_final_long1, stroke.x == 1)

# a) without linear post-change
# --------------------------------

# Fit random-intercept multilevel models and get pooled estimates
fit_a <- lmer(depress ~ 
                # fixed effects 
                1 + 
                pre + 
                post + 
                # random effects
                (1 + 
                   pre + 
                   post | 
                   idauniq),
              # control parameters
              control = lmerControl(optimizer = "bobyqa"), 
              REML = T, 
              data = data_stroke)

# summary of fit
summary(fit_a)
AIC(fit_a)


# b) with linear post-change
# ----------------------------

# Fit random-intercept multilevel models and get pooled estimates
fit_b <- lmer(depress ~ 1 + 
                pre + 
                post + 
                post_lin + 
                (1 + 
                   pre + 
                   post + 
                   post_lin | 
                   idauniq),
              control = lmerControl(optimizer = "bobyqa"), 
              REML = T, 
              data = data_stroke)

# summary of fit
summary(fit_b)
AIC(fit_b)


# c) saturated model
# ----------------------------

# Fit random-intercept multilevel models and get pooled estimates
fit_c <- lmer(depress ~ 
                1 + 
                pre + 
                dummy_zero + 
                dummy_plus1 + 
                dummy_plus2 + 
                dummy_plus3 + 
                dummy_plus4 + 
                dummy_plus5 +
                (1 + 
                   pre + 
                   dummy_zero |
                   idauniq),
              control = lmerControl(optimizer = "bobyqa"), 
              REML = T, 
              data = data_stroke)

# summary of fit
summary(fit_c)
AIC(fit_c)


# d) predictors
# ----------------------------

# Fit random-intercept multilevel models and get pooled estimates
fit_d <- lmer(depress ~ 
                # fixed effects 
                1 + 
                pre + 
                post + 
                w1_dhsex.y +
                w1_dhsex.y:pre +
                w1_dhsex.y:post +
                # random effects
                (1 + 
                   post | 
                   idauniq),
              # control parameters
              control = lmerControl(optimizer = "bobyqa"), 
              REML = F, 
              data = data_stroke)

# summary of fit
summary(fit_d)



# create new variable indicating ADL at time 0
data_stroke <- 
  data_stroke %>%
  group_by(idauniq) %>%
  mutate(
    adl_zero = adl[time == 0]
  )
  

# create new variable indicating ADL at time 0
data_stroke <- 
  data_stroke %>%
  group_by(idauniq) %>%
  mutate(
    mem_zero = mem[time == 0]
  )


# -----------------------
# Model-implied plots
# -----------------------

# 1. Depressive symptoms 
# -------------------------

# Create object with pooled model fit coefficients
ef.fit.b  <- fit.dep.pool$estimates[1:22]
ef.fit.se <- fit.dep.pool$estimates[23:44]
ef.fit    <- data.frame(ef.fit.b, ef.fit.se)

# Calculate means for plotting
ef.fit$ef.fit.b[13] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[2]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[13] 
ef.fit$ef.fit.b[14] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[3]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[14] 
ef.fit$ef.fit.b[15] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[4]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[15] 
ef.fit$ef.fit.b[16] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[5]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[16] 
ef.fit$ef.fit.b[17] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[6]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[17] 
ef.fit$ef.fit.b[18] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[7]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[18] 
ef.fit$ef.fit.b[19] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[8]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[19] 
ef.fit$ef.fit.b[20] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[9]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[20] 
ef.fit$ef.fit.b[21] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[10] + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[21] 
ef.fit$ef.fit.b[22] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[11] + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[22] 
ef.fit$ef.fit.b[12] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[12] 
ef.fit$ef.fit.b[2]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[2] 
ef.fit$ef.fit.b[3]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[3] 
ef.fit$ef.fit.b[4]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[4] 
ef.fit$ef.fit.b[5]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[5] 
ef.fit$ef.fit.b[6]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[6] 
ef.fit$ef.fit.b[7]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[7] 
ef.fit$ef.fit.b[8]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[8] 
ef.fit$ef.fit.b[9]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[9] 
ef.fit$ef.fit.b[10] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[10] 
ef.fit$ef.fit.b[11] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[11]

# Name pooled effects
ef.fit$st.case <- c("Controls", "Controls", "Controls", "Controls", "Controls", 
                    "Controls", "Controls", "Controls", "Controls", "Controls", 
                    "Controls", 
                    "Stroke", "Stroke", "Stroke", "Stroke", "Stroke", "Stroke", 
                    "Stroke", "Stroke", "Stroke", "Stroke", "Stroke")
ef.fit$time    <- c(0, -10, -8, -6, -4, -2, 2, 4, 6, 8, 10, 
                    0, -10, -8, -6, -4, -2, 2, 4, 6, 8, 10)

# Draw figure
fig_dep_long <- 
  ggplot(ef.fit, aes(x = time, y = ef.fit.b)) +
  geom_line(aes(colour = st.case), size = 1.2)  +
  geom_errorbar(aes(ymin = ef.fit.b - ef.fit.se, ymax = ef.fit.b + ef.fit.se, 
                    colour = st.case), 
                size = 0.7, width = .3) +
  geom_vline(xintercept = -1, linetype = "dashed", 
             color = "black", size = 0.5) +
  labs(y = "Depressive symptoms", x = "Time in years") +
  scale_colour_manual(values = c("#cccccc", "#d4738b"), 
                      breaks = c("Controls", "Stroke"),
                      labels = c("Matched controls ", "Stroke survivors ")) +
  coord_cartesian(ylim = c(0,2.6)) +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = c(0.0,1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6)) +
  scale_x_continuous(breaks = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10)) +
  theme_classic(base_size = 15)+
  theme(axis.title.x = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)), 
        axis.title.y = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),         
        axis.text.x  = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 15, r = 0, b = 0, l = 0)), 
        axis.text.y  = element_text(colour = "black", size = 19), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.text  = element_text(colour = "black", size = 15), 
        legend.title = element_blank(),
        legend.position = "top")


# 2. Activities daily living
# ----------------------------

# Create object with pooled model fit coefficients
ef.fit.b  <- fit.adl.pool$estimates[1:22]
ef.fit.se <- fit.adl.pool$estimates[23:44]
ef.fit    <- data.frame(ef.fit.b, ef.fit.se)

# Calculate means for plotting
ef.fit$ef.fit.b[13] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[2]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[13] 
ef.fit$ef.fit.b[14] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[3]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[14] 
ef.fit$ef.fit.b[15] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[4]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[15] 
ef.fit$ef.fit.b[16] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[5]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[16] 
ef.fit$ef.fit.b[17] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[6]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[17] 
ef.fit$ef.fit.b[18] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[7]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[18] 
ef.fit$ef.fit.b[19] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[8]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[19] 
ef.fit$ef.fit.b[20] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[9]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[20] 
ef.fit$ef.fit.b[21] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[10] + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[21] 
ef.fit$ef.fit.b[22] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[11] + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[22] 
ef.fit$ef.fit.b[12] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[12] 
ef.fit$ef.fit.b[2]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[2] 
ef.fit$ef.fit.b[3]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[3] 
ef.fit$ef.fit.b[4]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[4] 
ef.fit$ef.fit.b[5]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[5] 
ef.fit$ef.fit.b[6]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[6] 
ef.fit$ef.fit.b[7]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[7] 
ef.fit$ef.fit.b[8]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[8] 
ef.fit$ef.fit.b[9]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[9] 
ef.fit$ef.fit.b[10] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[10] 
ef.fit$ef.fit.b[11] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[11]

# Name pooled effects
ef.fit$st.case <- c("Controls", "Controls", "Controls", "Controls", "Controls", 
                    "Controls", "Controls", "Controls", "Controls", "Controls", 
                    "Controls", 
                    "Stroke", "Stroke", "Stroke", "Stroke", "Stroke", "Stroke", 
                    "Stroke", "Stroke", "Stroke", "Stroke", "Stroke")
ef.fit$time    <- c(0, -10, -8, -6, -4, -2, 2, 4, 6, 8, 10, 
                    0, -10, -8, -6, -4, -2, 2, 4, 6, 8, 10)

# Draw figure
fig_adl_long <- 
  ggplot(ef.fit, aes(x = time, y = ef.fit.b)) +
  geom_line(aes(colour = st.case), size = 1.2)  +
  geom_errorbar(aes(ymin = ef.fit.b - ef.fit.se, ymax = ef.fit.b + ef.fit.se, 
                    colour = st.case), 
                size = 0.7, width = .3) +
  geom_vline(xintercept = -1, linetype = "dashed", 
             color = "black", size = 0.5) +
  labs(y = "ADL", x = "Time in years") +
  scale_colour_manual(values = c("#cccccc", "#7fb5bd"), 
                      breaks = c("Controls", "Stroke"),
                      labels = c("Matched controls ", "Stroke survivors ")) +
  coord_cartesian(ylim = c(0, 2)) +
#  scale_y_continuous(expand = c(0, 0), 
#                     breaks = c(1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6)) +
  scale_x_continuous(breaks = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10)) +
  theme_classic(base_size = 15)+
  theme(axis.title.x = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)), 
        axis.title.y = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),         
        axis.text.x  = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 15, r = 0, b = 0, l = 0)), 
        axis.text.y  = element_text(colour = "black", size = 19), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.text  = element_text(colour = "black", size = 15), 
        legend.title = element_blank(),
        legend.position = "top")


# 3. Memory functioning
# ----------------------------

# Create object with pooled model fit coefficients
ef.fit.b  <- fit.mem.pool$estimates[1:22]
ef.fit.se <- fit.mem.pool$estimates[23:44]
ef.fit    <- data.frame(ef.fit.b, ef.fit.se)

# Calculate means for plotting
ef.fit$ef.fit.b[13] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[2]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[13] 
ef.fit$ef.fit.b[14] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[3]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[14] 
ef.fit$ef.fit.b[15] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[4]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[15] 
ef.fit$ef.fit.b[16] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[5]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[16] 
ef.fit$ef.fit.b[17] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[6]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[17] 
ef.fit$ef.fit.b[18] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[7]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[18] 
ef.fit$ef.fit.b[19] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[8]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[19] 
ef.fit$ef.fit.b[20] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[9]  + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[20] 
ef.fit$ef.fit.b[21] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[10] + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[21] 
ef.fit$ef.fit.b[22] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[11] + ef.fit$ef.fit.b[12] + ef.fit$ef.fit.b[22] 
ef.fit$ef.fit.b[12] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[12] 
ef.fit$ef.fit.b[2]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[2] 
ef.fit$ef.fit.b[3]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[3] 
ef.fit$ef.fit.b[4]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[4] 
ef.fit$ef.fit.b[5]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[5] 
ef.fit$ef.fit.b[6]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[6] 
ef.fit$ef.fit.b[7]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[7] 
ef.fit$ef.fit.b[8]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[8] 
ef.fit$ef.fit.b[9]  <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[9] 
ef.fit$ef.fit.b[10] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[10] 
ef.fit$ef.fit.b[11] <- ef.fit$ef.fit.b[1] + ef.fit$ef.fit.b[11]

# Name pooled effects
ef.fit$st.case <- c("Controls", "Controls", "Controls", "Controls", "Controls", 
                    "Controls", "Controls", "Controls", "Controls", "Controls", 
                    "Controls", 
                    "Stroke", "Stroke", "Stroke", "Stroke", "Stroke", "Stroke", 
                    "Stroke", "Stroke", "Stroke", "Stroke", "Stroke")
ef.fit$time    <- c(0, -10, -8, -6, -4, -2, 2, 4, 6, 8, 10, 
                    0, -10, -8, -6, -4, -2, 2, 4, 6, 8, 10)

# Draw figure
fig_mem_long <- 
  ggplot(ef.fit, aes(x = time, y = ef.fit.b)) +
  geom_line(aes(colour = st.case), size = 1.2)  +
  geom_errorbar(aes(ymin = ef.fit.b - ef.fit.se, ymax = ef.fit.b + ef.fit.se, 
                    colour = st.case), 
                size = 0.7, width = .3) +
  geom_vline(xintercept = -1, linetype = "dashed", 
             color = "black", size = 0.5) +
  labs(y = "Memory score", x = "Time in years") +
  scale_colour_manual(values = c("#cccccc", "#7fb5bd"), 
                      breaks = c("Controls", "Stroke"),
                      labels = c("Matched controls ", "Stroke survivors ")) +
 # coord_cartesian(ylim = c(1.0,2.6)) +
  #  scale_y_continuous(expand = c(0, 0), 
  #                     breaks = c(1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6)) +
  scale_x_continuous(breaks = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10)) +
  theme_classic(base_size = 15)+
  theme(axis.title.x = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)), 
        axis.title.y = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),         
        axis.text.x  = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 15, r = 0, b = 0, l = 0)), 
        axis.text.y  = element_text(colour = "black", size = 19), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.text  = element_text(colour = "black", size = 15), 
        legend.title = element_blank(),
        legend.position = "top")



# -------------------
# Actual data plots
# -------------------

# plot depressive symptoms
# --------------------------
sum_dep <- summarySE(data_final_long15, 
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

# plot mobility
# ---------------
sum_mob <- summarySE(data_final_long5, 
                     measurevar = "mob", 
                     groupvars = c("n_strokes.x", "time"),
                     na.rm = T, conf.interval = .95)

# Make the graph with the 95% confidence interval
ggplot(sum_mob, aes(x = time, y = mob, group = n_strokes.x, color = n_strokes.x)) +
  geom_errorbar(width = .1, aes(ymin = mob - ci, ymax = mob + ci)) +
  geom_point(shape = 21, size = 3) + 
  geom_line() +
  labs(y = "Mobility", x = "")

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


# plot memory
# -------------
sum_mem <- summarySE(data_final_long5, 
                     measurevar = "mem", 
                     groupvars = c("n_strokes.x", "time"),
                     na.rm = T, conf.interval = .95)

# Make the graph with the 95% confidence interval
ggplot(sum_mem, aes(x = time, y = mem, group = n_strokes.x, color = n_strokes.x)) +
  geom_errorbar(width = .1, aes(ymin = mem - ci, ymax = mem + ci)) +
  geom_point(shape = 21, size = 3) + 
  geom_line() +
  labs(y = "Memory", x = "")


