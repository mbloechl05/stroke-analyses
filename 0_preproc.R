# =====================
#  Data preprocessing 
# =====================

# clean work space
rm(list = ls()) 

# load packages 
# source("analyses/00_prepare.R")

# ---------------------------------------------------------
# 1) Get raw data, select variables and do basic pre-proc
# ---------------------------------------------------------

# Load data 
# -------------

# wave 0, part 1
wave01 <- read.table("data/raw/tab/wave_0_1998_data.tab", sep = "\t", header = T) 

# wave 0, part 2
wave02 <- read.table("data/raw/tab/wave_0_1999_data.tab", sep = "\t", header = T) 

# wave 0, part 3
wave03 <- read.table("data/raw/tab/wave_0_2001_data.tab", sep = "\t", header = T) 

# Combine  all data from wave 0 and remove unnecessary files
wave_0_c <- rbind.fill(wave01, wave02, wave03)  
rm(list = c("wave01", "wave02", "wave03")) 

# Load data from all other waves (starting from wave 2)
wave_1_c <- read.table("data/raw/tab/wave_1_core_data_v3.tab", sep = "\t", header = T) 
wave_2_c <- read.table("data/raw/tab/wave_2_core_data_v4.tab", sep = "\t", header = T)
wave_3_c <- read.table("data/raw/tab/wave_3_elsa_data_v4.tab", sep = "\t", header = T, fill = T) 
wave_4_c <- read.table("data/raw/tab/wave_4_elsa_data_v3.tab", sep = "\t", header = T)
wave_5_c <- read.table("data/raw/tab/wave_5_elsa_data_v4.tab", sep = "\t", header = T)
wave_6_c <- read.table("data/raw/tab/wave_6_elsa_data_v2.tab", sep = "\t", header = T)
wave_7_c <- read.table("data/raw/tab/wave_7_elsa_data.tab"   , sep = "\t", header = T)


# Select relevant variables from each wave
# -------------------------------------------

# wave 0
wave_0_c  <- 
  wave_0_c[,c("idauniq", "ager", "educend", "topqual2", "bmival", "ethnicr")]

# wave 1
wave_1_c  <- 
  wave_1_c[,c("idauniq", "elsa", "dhsex", "dhager", "fqethnr", "indoc", "heska",
              # cvd variables
              "hedia01", "hedia02", "hedia03", "hedia04", "hedia05", "hedia06", 
              "hedia07", "hedia08", "hedia09", "hedia10",
              # adl variables
              "headb01", "headb02", "headb03", "headb04", "headb05", "headb06", 
              "headb07", "headb08", "headb09", "headb10", "headb11", "headb12", 
              "headb13",
              # memory 
              "cflisen", "cflisd",
              # depressive symptoms
              "psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", 
              "pscedg", "pscedh"
              )]

# wave 2
wave_2_c  <- 
  wave_2_c[,c("idauniq", "fqethnr", "indager",
              # cvd variables
              "hedia01", "hedia02", "hedia03", "hedia04", "hedia05" , "hedia06", 
              "hedia07", "hedia08", "hedia09", "w2indout", "HeAge"  , "HeAgeR" , 
              "HeAgeRY", "Henmst" ,
              # adl variables
              "headb01", "headb02", "headb03", "headb04", "headb05", "headb06", 
              "headb07", "headb08", "headb09", "headb10", "headb11", "headb12", 
              "headb13",
              # memory
              "CfLisEn", "CfLisD",
              # depressive symptoms
              "PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", 
              "PScedG", "PScedH")]

# wave 3
wave_3_c  <- 
  wave_3_c[,c("idauniq", "w3indout", "indager",
              # stroke newly diagnosed
              "hediast", "dhedimst", "heage", "heager", "heagery", "henmst",
              # adl variables
              "headldr", "headlwa", "headlba", "headlea", "headlbe", "headlwc", 
              # memory 
              "cflisen", "cflisd",
              # depressive symptoms
              "psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", 
              "pscedg", "pscedh")]

# wave 4
wave_4_c  <- 
  wave_4_c[,c("idauniq", "indager", "outindw4",
              # stroke newly diagnosed
              "hediast", "hedimst", "heage", "heager", "heagery", "henmst" ,
              # adl variables
              "headldr", "headlwa", "headlba", "headlea", "headlbe", "headlwc", 
              # memory 
              "cflisen", "cflisd",
              # depressive symptoms
              "psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", 
              "pscedg", "pscedh")]

# wave 5
wave_5_c  <- 
  wave_5_c[,c("idauniq", "indager", "w5indout", 
              # stroke newly diagnosed
              "hediast", "hedimst", "heage", "heager", "heagery", "henmst",
              # adl variables
              "headldr", "headlwa", "headlba", "headlea", "headlbe", "headlwc", 
              # memory 
              "cflisen", "cflisd",
              # depressive symptoms
              "psceda", "pscedb", "pscedc", "pscedd" , "pscede" , "pscedf", 
              "pscedg", "pscedh")]

# wave 6
wave_6_c  <- 
  wave_6_c[,c("idauniq", "indager", "w6indout",
              # stroke newly diagnosed
              "hediast", "hedimst", "HeAge", "HeAgeR", "HeAgeRY", "HeNmSt",
              # adl variables
              "headldr", "headlwa", "headlba", "headlea", "headlbe", "headlwc", 
              # memory 
              "CfLisEn", "CfLisD",
              # depressive symptoms
              "PScedA", "PScedB", "PScedC", "PScedD" , "PScedE" , "PScedF", 
              "PScedG", "PScedH")]

# wave 7
wave_7_c  <- 
  wave_7_c[,c("idauniq", "indager", 
              # stroke newly diagnosed
              "hediast", "hedimst", "HeAge", "HeAgeR", "HeAgeRY",
              # adl variables
              "headldr", "headlwa", "headlba", "headlea", "headlbe", "headlwc", 
              # memory 
              "CfLisEn", "CfLisD",
              # depressive symptoms
              "PScedA", "PScedB", "PScedC", "PScedD" , "PScedE" , "PScedF", 
              "PScedG", "PScedH")]


# Harmonise naming of depression variables across waves 
# ------------------------------------------------------

# rename depression variables wave 2
setnames(wave_2_c, 
         # to be renamed
         old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", 
                 "PScedG", "PScedH"), 
         # new harmonised variable names
         new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", 
                 "pscedg", "pscedh"))

# rename depression variables wave 6
setnames(wave_6_c, 
         # to be renamed
         old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", 
                 "PScedG", "PScedH"), 
         # new harmonised variable names
         new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", 
                 "pscedg", "pscedh"))

# rename depression variables wave 7
setnames(wave_7_c, 
         # to be renamed
         old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", 
                 "PScedG", "PScedH"), 
         # new harmonised variable names
         new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", 
                 "pscedg", "pscedh"))


# Harmonise naming of cognition variables across waves 
# -------------------------------------------------------

# rename adl variables wave 2
setnames(wave_2_c, 
         old = c("CfLisEn", "CfLisD"), 
         new = c("cflisen", "cflisd"))

# rename adl variables wave 6
setnames(wave_6_c, 
         old = c("CfLisEn", "CfLisD"), 
         new = c("cflisen", "cflisd"))

# rename adl variables wave 7
setnames(wave_7_c, 
         old = c("CfLisEn", "CfLisD"), 
         new = c("cflisen", "cflisd"))


# Final steps to create 1 data set
# -----------------------------------

# Add indicator for the respective wave to each variable (except idauniq)
wave_0_c <- wave_0_c %>% rename_at(vars(-idauniq), ~ paste0("w0_",.))
wave_1_c <- wave_1_c %>% rename_at(vars(-idauniq), ~ paste0("w1_",.))
wave_2_c <- wave_2_c %>% rename_at(vars(-idauniq), ~ paste0("w2_",.))
wave_3_c <- wave_3_c %>% rename_at(vars(-idauniq), ~ paste0("w3_",.))
wave_4_c <- wave_4_c %>% rename_at(vars(-idauniq), ~ paste0("w4_",.))
wave_5_c <- wave_5_c %>% rename_at(vars(-idauniq), ~ paste0("w5_",.))
wave_6_c <- wave_6_c %>% rename_at(vars(-idauniq), ~ paste0("w6_",.))
wave_7_c <- wave_7_c %>% rename_at(vars(-idauniq), ~ paste0("w7_",.))

# Create variables indicating whether people participated in wave: 1 = YES
wave_0_c$wave_0_c <- 1
wave_1_c$wave_1_c <- 1
wave_2_c$wave_2_c <- 1
wave_3_c$wave_3_c <- 1
wave_4_c$wave_4_c <- 1
wave_5_c$wave_5_c <- 1
wave_6_c$wave_6_c <- 1
wave_7_c$wave_7_c <- 1

# Merge data from waves into one dataframe (wide format) and only keep people with 
# data from wave 1, i.e. exclude top-ups
data = merge(wave_1_c, wave_2_c, all   = T, by = "idauniq", sort = T)
data = merge(data,     wave_3_c, all.x = T, by = "idauniq", sort = T)
data = merge(data,     wave_4_c, all.x = T, by = "idauniq", sort = T)
data = merge(data,     wave_5_c, all.x = T, by = "idauniq", sort = T)
data = merge(data,     wave_6_c, all.x = T, by = "idauniq", sort = T)
data = merge(data,     wave_7_c, all.x = T, by = "idauniq", sort = T)

# Add wave 0 data 
data = merge(data, wave_0_c, all = F, all.x = T, by = "idauniq", sort = T)

# Fill up variables with indicating participation in specific wave, 0 = NO
data$wave_0_c[is.na(data$wave_0_c)] <- 0
data$wave_1_c[is.na(data$wave_1_c)] <- 0
data$wave_2_c[is.na(data$wave_2_c)] <- 0
data$wave_3_c[is.na(data$wave_3_c)] <- 0
data$wave_4_c[is.na(data$wave_4_c)] <- 0
data$wave_5_c[is.na(data$wave_5_c)] <- 0
data$wave_6_c[is.na(data$wave_6_c)] <- 0
data$wave_7_c[is.na(data$wave_7_c)] <- 0

# Recode values for missing data (e.g. -9, -8, -1) as NA
data <- data %>% mutate_all(funs(na_if(., -11)))
data <- data %>% mutate_all(funs(na_if(., -10)))
data <- data %>% mutate_all(funs(na_if(., -9)))
data <- data %>% mutate_all(funs(na_if(., -8)))
data <- data %>% mutate_all(funs(na_if(., -7)))
data <- data %>% mutate_all(funs(na_if(., -6)))
data <- data %>% mutate_all(funs(na_if(., -5)))
data <- data %>% mutate_all(funs(na_if(., -4)))
data <- data %>% mutate_all(funs(na_if(., -3)))
data <- data %>% mutate_all(funs(na_if(., -2)))
data <- data %>% mutate_all(funs(na_if(., -1)))


# ------------------------------------------
# Exclusion and inclusion of participants
# ------------------------------------------

# Include only core members
table(data$w1_elsa, useNA = "always") # Show how many core members (=1)
data <- subset(data, w1_elsa == 1) # Keep only core members

# Exclude participants with previous stroke or missing data on this at baseline
data$w1_hedia <- ifelse(data$w1_hedia01 == 8, 1, 0) # Create stroke var from CVD-vars at wave 1
data$w1_hedia[data$w1_hedia02 == 8] <- 1
data$w1_hedia[data$w1_hedia03 == 8] <- 1
data$w1_hedia[data$w1_hedia04 == 8] <- 1
data$w1_hedia[data$w1_hedia05 == 8] <- 1
data$w1_hedia[data$w1_hedia06 == 8] <- 1
data$w1_hedia[data$w1_hedia07 == 8] <- 1
data$w1_hedia[data$w1_hedia08 == 8] <- 1
data$w1_hedia[data$w1_hedia09 == 8] <- 1
data$w1_hedia[data$w1_hedia10 == 8] <- 1

table(data$w1_hedia, useNA = "always") # Show how many people
data <- subset(data, data$w1_hedia == 0) # Keep only non-stroke at wave 1


# ------------------------------------------------------------
# Select stroke cases and participants without stroke
# ------------------------------------------------------------

# Create stroke variable for wave 2 (newly diagn.)
data$w2_hediast <- ifelse(data$w2_hedia01 == 8, 1, 0)
data$w2_hediast[data$w2_hedia02 == 8] <- 1
data$w2_hediast[data$w2_hedia03 == 8] <- 1
data$w2_hediast[data$w2_hedia04 == 8] <- 1
data$w2_hediast[data$w2_hedia05 == 8] <- 1
data$w2_hediast[data$w2_hedia06 == 8] <- 1
data$w2_hediast[data$w2_hedia07 == 8] <- 1
data$w2_hediast[data$w2_hedia08 == 8] <- 1
data$w2_hediast[data$w2_hedia09 == 8] <- 1

table(data$w2_hediast, useNA = "always")
table(data$w3_hediast, useNA = "always")
table(data$w4_hediast, useNA = "always")
table(data$w5_hediast, useNA = "always")
table(data$w6_hediast, useNA = "always")
table(data$w7_hediast, useNA = "always")

table(data$wave_1_c, useNA = "always")
table(data$wave_2_c, useNA = "always")
table(data$wave_3_c, useNA = "always")
table(data$wave_4_c, useNA = "always")
table(data$wave_5_c, useNA = "always")
table(data$wave_6_c, useNA = "always")
table(data$wave_7_c, useNA = "always")

# Create variable indicating number of strokes within study
data$stroke <- 
  rowSums(data[c("w2_hediast", 
                 "w3_hediast", 
                 "w4_hediast", 
                 "w5_hediast", 
                 "w6_hediast", 
                 "w7_hediast")], 
          na.rm = T)
table(data$stroke, useNA = "always")

# Change name of full data set to data_full
data_full <- data

# Select only people with 1 stroke during study
data <- subset(data_full, stroke == 1) 

# Check peoples dates for their stroke in each wave
table(data$w2_HeAgeRY, useNA = "always") 
table(data$w3_heagery, useNA = "always") 
table(data$w4_heagery, useNA = "always")
table(data$w5_heagery, useNA = "always")
table(data$w6_HeAgeRY, useNA = "always")
table(data$w7_HeAgeRY, useNA = "always")

# Only include people with reasonable dates for their stroke
data <- subset(data, data$w2_HeAgeRY > 2001 | is.na(data$w2_HeAgeRY))  
data <- subset(data, data$w3_heagery > 2003 | is.na(data$w3_heagery))  
data <- subset(data, data$w4_heagery > 2005 | is.na(data$w4_heagery))  
data <- subset(data, data$w5_heagery > 2007 | is.na(data$w5_heagery))  
data <- subset(data, data$w6_HeAgeRY > 2009 | is.na(data$w6_HeAgeRY)) 
data <- subset(data, data$w7_HeAgeRY > 2011 | is.na(data$w7_HeAgeRY)) 

# At which wave did stroke occur? 
table(data$w2_hediast, useNA = "always")
table(data$w3_hediast, useNA = "always")
table(data$w4_hediast, useNA = "always")
table(data$w5_hediast, useNA = "always")
table(data$w6_hediast, useNA = "always")
table(data$w7_hediast, useNA = "always")

# Create new time variables indicating the time of stroke, before and after
(data <- data %>%
    mutate(w1_time = case_when( # new var w1_time coding first wave in relation to stroke
      w2_hediast == 1 ~ -1,    # if stroke in wave 2, w1_time = -1
      w3_hediast == 1 ~ -2,    # if stroke in wave 3, w1_time = -2
      w4_hediast == 1 ~ -3,    # if stroke in wave 4, w1_time = -3
      w5_hediast == 1 ~ -4,    # if stroke in wave 5, w1_time = -4
      w6_hediast == 1 ~ -5,    # if stroke in wave 6, w1_time = -5
      w7_hediast == 1 ~ -6     # if stroke in wave 7, w1_time = -6
    )))

(data <- data %>%
    mutate(w2_time = case_when( # new var w2_time coding second wave in relation to stroke
      w2_hediast == 1 ~  0,    # if stroke in wave 2, w2_time =  0
      w3_hediast == 1 ~ -1,    # if stroke in wave 3, w2_time = -1
      w4_hediast == 1 ~ -2,    # if stroke in wave 4, w2_time = -2
      w5_hediast == 1 ~ -3,    # if stroke in wave 5, w2_time = -3
      w6_hediast == 1 ~ -4,    # if stroke in wave 6, w2_time = -4
      w7_hediast == 1 ~ -5     # if stroke in wave 7, w2_time = -5
    )))

(data <- data %>%
    mutate(w3_time = case_when( # new var w3_time
      w2_hediast == 1 ~  1,     # if stroke in wave 2, w3_time =  1
      w3_hediast == 1 ~  0,     # if stroke in wave 3, w3_time =  0
      w4_hediast == 1 ~ -1,     # if stroke in wave 4, w3_time = -1
      w5_hediast == 1 ~ -2,     # if stroke in wave 5, w3_time = -2
      w6_hediast == 1 ~ -3,     # if stroke in wave 6, w3_time = -3
      w7_hediast == 1 ~ -4      # if stroke in wave 7, w3_time = -4
    )))

(data <- data %>%
    mutate(w4_time = case_when(
      w2_hediast == 1 ~  2,
      w3_hediast == 1 ~  1,
      w4_hediast == 1 ~  0, 
      w5_hediast == 1 ~ -1,
      w6_hediast == 1 ~ -2,
      w7_hediast == 1 ~ -3
    )))

(data <- data %>%
    mutate(w5_time = case_when(
      w2_hediast == 1 ~  3,
      w3_hediast == 1 ~  2,
      w4_hediast == 1 ~  1, 
      w5_hediast == 1 ~  0,
      w6_hediast == 1 ~ -1,
      w7_hediast == 1 ~ -2
    )))


(data <- data %>%
    mutate(w6_time = case_when(
      w2_hediast == 1 ~  4,
      w3_hediast == 1 ~  3,
      w4_hediast == 1 ~  2, 
      w5_hediast == 1 ~  1,
      w6_hediast == 1 ~  0,
      w7_hediast == 1 ~ -1
    )))

(data <- data %>%
    mutate(w7_time = case_when(
      w2_hediast == 1 ~  5,
      w3_hediast == 1 ~  4,
      w4_hediast == 1 ~  3, 
      w5_hediast == 1 ~  2,
      w6_hediast == 1 ~  1,
      w7_hediast == 1 ~  0
    )))

# Select only people with 0 strokes during study
data_nostroke <- subset(data_full, stroke == 0) 

# Merge data again
data <- bind_rows(data, data_nostroke)


# ---------------------------------------------
# Recode and prepare all relevant variables
# ---------------------------------------------
 
# Age
# ------

# Recode age variable; 109 adults aged > 89 have age coded as 99;
# these are set as missings
data$w1_dhager[data$w1_dhager == 99] <- NA
 

# Sex and Smoking
# ------------------

# Recode all binary variables to 0 (no) and 1 (yes); before 1 (yes), 2 (no)
for (i in names(data[,c(grep("w1_dhsex", colnames(data)), # 0 = female
                        grep("w1_heska", colnames(data)))])) {
  data[[i]][data[[i]] == 2] <- 0}
 

# Ethnicity
# ------------

# Recode ethnicity so "white" is coded as 1 and "non-white" as 0
data$w0_ethni <- ifelse(data$w0_ethnicr == 1, 1, 0)
 

# Education
# ------------

# Re-code education variable (higher education? (yes / no))
data$w0_topqual2[data$w0_topqual2 == 6] <- NA # recode foreign / other qual to NA
data$w0_topqual2[data$w0_topqual2 == 8] <- NA # recode full-time students to NA
data$w0_educ <- ifelse(data$w0_topqual2 == 1, 1, 0)


# Hypertension
# ---------------

# Create hypertension variable from CVD-variables at wave 1
data$w1_hypt <- ifelse(data$w1_hedia01 == 1, 1, 0)
data$w1_hypt[data$w1_hedia02 == 1 | 
               data$w1_hedia03 == 1 | 
               data$w1_hedia04 == 1 |
               data$w1_hedia05 == 1 | 
               data$w1_hedia06 == 1 | 
               data$w1_hedia07 == 1 |
               data$w1_hedia08 == 1 | 
               data$w1_hedia09 == 1] <- 1

# Have to recode those with NA to NA again 
data$w1_hypt[is.na(data$w1_hedia01)] <- NA


# Diabetes
# --------------

# Create diabetes variable from CVD-variables at wave 1
data$w1_diab <- ifelse(data$w1_hedia01 == 7, 1, 0)
data$w1_diab[data$w1_hedia02 == 7 | 
               data$w1_hedia03 == 7 | 
               data$w1_hedia04 == 7 |
               data$w1_hedia05 == 7 | 
               data$w1_hedia06 == 7 | 
               data$w1_hedia07 == 7 |
               data$w1_hedia08 == 7 | 
               data$w1_hedia09 == 7] <- 1

# Have to recode those with NA to NA again 
data$w1_diab[is.na(data$w1_hedia01)] <- NA


# Depressive symptoms (CESD, 8 items)
# --------------------------------------

# Recode items to dummies (0 = no, 1 = yes)
for (i in names(data[,c(grep("psced", colnames(data)))])) {
  data[[i]][data[[i]] == 2] <- 0}

# Reverse item d 
data$w1_pscedd_r <- 1 - data$w1_pscedd
data$w2_pscedd_r <- 1 - data$w2_pscedd
data$w3_pscedd_r <- 1 - data$w3_pscedd
data$w4_pscedd_r <- 1 - data$w4_pscedd
data$w5_pscedd_r <- 1 - data$w5_pscedd
data$w6_pscedd_r <- 1 - data$w6_pscedd
data$w7_pscedd_r <- 1 - data$w7_pscedd

# Reverse item f 
data$w1_pscedf_r <- 1 - data$w1_pscedf
data$w2_pscedf_r <- 1 - data$w2_pscedf
data$w3_pscedf_r <- 1 - data$w3_pscedf
data$w4_pscedf_r <- 1 - data$w4_pscedf
data$w5_pscedf_r <- 1 - data$w5_pscedf
data$w6_pscedf_r <- 1 - data$w6_pscedf
data$w7_pscedf_r <- 1 - data$w7_pscedf

# Select all CES-D items for each wave
# wave 1
w1_dep_items <- 
  data[,c("w1_psceda", "w1_pscedb"  , "w1_pscedc", "w1_pscedd_r",
          "w1_pscede", "w1_pscedf_r", "w1_pscedg", "w1_pscedh")] 
# wave 2
w2_dep_items <- 
  data[,c("w2_psceda", "w2_pscedb"  , "w2_pscedc", "w2_pscedd_r",
          "w2_pscede", "w2_pscedf_r", "w2_pscedg", "w2_pscedh")] 
# wave 3
w3_dep_items <- 
  data[,c("w3_psceda", "w3_pscedb"  , "w3_pscedc", "w3_pscedd_r",
          "w3_pscede", "w3_pscedf_r", "w3_pscedg", "w3_pscedh")] 
# wave 4
w4_dep_items <- 
  data[,c("w4_psceda", "w4_pscedb"  , "w4_pscedc", "w4_pscedd_r",
          "w4_pscede", "w4_pscedf_r", "w4_pscedg", "w4_pscedh")] 
# wave 5
w5_dep_items <- 
  data[,c("w5_psceda", "w5_pscedb"  , "w5_pscedc", "w5_pscedd_r",
          "w5_pscede", "w5_pscedf_r", "w5_pscedg", "w5_pscedh")] 
# wave 6
w6_dep_items <- 
  data[,c("w6_psceda", "w6_pscedb"  , "w6_pscedc", "w6_pscedd_r",
          "w6_pscede", "w6_pscedf_r", "w6_pscedg", "w6_pscedh")] 
# wave 7
w7_dep_items <- 
  data[,c("w7_psceda", "w7_pscedb"  , "w7_pscedc", "w7_pscedd_r",
          "w7_pscede", "w7_pscedf_r", "w7_pscedg", "w7_pscedh")] 

# Calculate sum score of depr symptoms (people with missing values are excluded)
data$w1_dep_sum <- rowSums(w1_dep_items)
data$w2_dep_sum <- rowSums(w2_dep_items)
data$w3_dep_sum <- rowSums(w3_dep_items)
data$w4_dep_sum <- rowSums(w4_dep_items)
data$w5_dep_sum <- rowSums(w5_dep_items)
data$w6_dep_sum <- rowSums(w6_dep_items)
data$w7_dep_sum <- rowSums(w7_dep_items)


# Activities of daily living
# -----------------------------

# Get adl data from wave 1 and 2 (10 items) and recode 
for (i in names(data[,c(grep("headb", colnames(data)))])) {
  data[[i]] <- data[[i]] %>% dplyr::recode(., 
                                           '96' = 0,
                                           '7'  = 0,
                                           '8'  = 0,
                                           '9'  = 0,
                                           '10' = 0,
                                           '11' = 0,
                                           '12' = 0,
                                           '13' = 0,
                                           '1'  = 1,
                                           '2'  = 1,
                                           '3'  = 1,
                                           '4'  = 1,
                                           '5'  = 1,
                                           '6'  = 1)
} # note that in this wave, only answers 1-6 are considered *basic* ADL

# Select all adl items
# Wave 1
w1_adl_items <- 
  data[,c( "w1_headb01", "w1_headb02", "w1_headb03", "w1_headb04", "w1_headb05", 
           "w1_headb06", "w1_headb07", "w1_headb08", "w1_headb09", "w1_headb10", 
           "w1_headb11", "w1_headb12", "w1_headb13")] 

# wave 2
w2_adl_items <- 
  data[,c( "w2_headb01", "w2_headb02", "w2_headb03", "w2_headb04", "w2_headb05", 
           "w2_headb06", "w2_headb07", "w2_headb08", "w2_headb09", "w2_headb10", 
           "w2_headb11", "w2_headb12", "w2_headb13")] 

# wave 3
w3_adl_items <- 
  data[,c( "w3_headldr", "w3_headlwa", "w3_headlba", "w3_headlea", "w3_headlbe", 
           "w3_headlwc")] 

# wave 4
w4_adl_items <- 
  data[,c( "w4_headldr", "w4_headlwa", "w4_headlba", "w4_headlea", "w4_headlbe", 
           "w4_headlwc")] 

# wave 5
w5_adl_items <-  
  data[,c( "w5_headldr", "w5_headlwa", "w5_headlba", "w5_headlea", "w5_headlbe", 
           "w5_headlwc")] 

# wave 6
w6_adl_items <- 
  data[,c( "w6_headldr", "w6_headlwa", "w6_headlba", "w6_headlea", "w6_headlbe", 
           "w6_headlwc")] 

# wave 7
w7_adl_items <- 
  data[,c( "w7_headldr", "w7_headlwa", "w7_headlba", "w7_headlea", "w7_headlbe", 
           "w7_headlwc")]

# Calculate sum scores for each wave
data$w1_adl_sum <- rowSums(w1_adl_items, na.rm = T)
data$w2_adl_sum <- rowSums(w2_adl_items, na.rm = T)
data$w3_adl_sum <- rowSums(w3_adl_items)
data$w4_adl_sum <- rowSums(w4_adl_items)
data$w5_adl_sum <- rowSums(w5_adl_items)
data$w6_adl_sum <- rowSums(w6_adl_items)
data$w7_adl_sum <- rowSums(w7_adl_items)

# Have to recode those with NA to NA again 
data$w1_adl_sum[is.na(data$w1_headb01)] <- NA
data$w2_adl_sum[is.na(data$w2_headb01)] <- NA


# Memory score
# ---------------

# Calculate sum scores for each wave
data$w1_mem_sum <- rowSums(data[,c( "w1_cflisen", "w1_cflisd")])
data$w2_mem_sum <- rowSums(data[,c( "w2_cflisen", "w2_cflisd")])
data$w3_mem_sum <- rowSums(data[,c( "w3_cflisen", "w3_cflisd")])
data$w4_mem_sum <- rowSums(data[,c( "w4_cflisen", "w4_cflisd")])
data$w5_mem_sum <- rowSums(data[,c( "w5_cflisen", "w5_cflisd")])
data$w6_mem_sum <- rowSums(data[,c( "w6_cflisen", "w6_cflisd")])
data$w7_mem_sum <- rowSums(data[,c( "w7_cflisen", "w7_cflisd")])


# ---------------
# Reformat data
# ---------------

# Recode some variables as factors for imputation
data$w0_ethni <- as.factor(data$w0_ethni)
data$w1_dhsex <- as.factor(data$w1_dhsex)
data$w0_educ  <- as.factor(data$w0_educ)
data$w1_hypt  <- as.factor(data$w1_hypt)
data$w1_heska <- as.factor(data$w1_heska)
data$w1_diab  <- as.factor(data$w1_diab)

# Reduce dataframe to relevant variables
data_red <- 
  data[,c("idauniq"   , "w1_dhsex"  , "w0_ethni"  , "w0_educ"   , "w1_dhager" , 
          "stroke"    , "w1_hypt"   , "w1_heska"  , "w1_diab"   , "w0_bmival" , 
          "w1_time"   , "w2_time"   , "w3_time"   , "w4_time"   , "w5_time"   , 
          "w6_time"   , "w7_time"   , "w1_hedia"  , "w2_hediast", "w3_hediast", 
          "w4_hediast", "w5_hediast", "w6_hediast", "w7_hediast", "w1_dep_sum", 
          "w2_dep_sum", "w3_dep_sum", "w4_dep_sum", "w5_dep_sum", "w6_dep_sum", 
          "w7_dep_sum", "w1_adl_sum", "w2_adl_sum", "w3_adl_sum", "w4_adl_sum", 
          "w5_adl_sum", "w6_adl_sum", "w7_adl_sum", "w1_mem_sum", "w2_mem_sum", 
          "w3_mem_sum", "w4_mem_sum", "w5_mem_sum", "w6_mem_sum", "w7_mem_sum",
          "w1_psceda" , "w1_pscedb" , "w1_pscedc" , "w1_pscedd" , "w1_pscede" , 
          "w1_pscedf" , "w1_pscedg" , "w1_pscedh" , "w2_psceda" , "w2_pscedb" , 
          "w2_pscedc" , "w2_pscedd" , "w2_pscede" , "w2_pscedf" , "w2_pscedg" , 
          "w2_pscedh" , "w3_psceda" , "w3_pscedb" , "w3_pscedc" , "w3_pscedd" , 
          "w3_pscede" , "w3_pscedf" , "w3_pscedg" , "w3_pscedh" , "w4_psceda" , 
          "w4_pscedb" , "w4_pscedc" , "w4_pscedd" , "w4_pscede" , "w4_pscedf" , 
          "w4_pscedg" , "w4_pscedh" , "w5_psceda" , "w5_pscedb" , "w5_pscedc" , 
          "w5_pscedd" , "w5_pscede" , "w5_pscedf" , "w5_pscedg" , "w5_pscedh" , 
          "w6_psceda" , "w6_pscedb" , "w6_pscedc" , "w6_pscedd" , "w6_pscede" , 
          "w6_pscedf" , "w6_pscedg" , "w6_pscedh" , "w7_psceda" , "w7_pscedb" , 
          "w7_pscedc" , "w7_pscedd" , "w7_pscede" , "w7_pscedf" , "w7_pscedg" , 
          "w7_pscedh" , "n_waves")]


# ---------------------------
# Save preprocessed data
# ---------------------------

save(data, 
     data_red, 
     file = "data/processed/proc_data.RData")

