## =====================
##  Data preprocessing 
## =====================

# TO DO:
# Exclude people with stroke at wave 7?
# Exclude people with other diseases at baseline? 
# Exclude people with multiple strokes (I think answer was yes)
# How do I code time with respect to stroke? 

# Check interviews by proxy and so on
# Check dates of the stroke if available > select appropriate data here!


# clean work space
rm(list = ls()) 

# load packages
library(plyr)   # for rbind.fill()
library(dplyr)  # for select()
library(tidyr)  # for case_when()
library(VIM)    # for reaname_at()
library(stats)  # for reshape ()
library(psych)  # for alpha()
library(data.table) # for setnames()


# ---------------------------------------------------------
# 1) Get raw data, select variables and do basic pre-proc
# ---------------------------------------------------------

# Load data 
wave01 <- read.table("data/raw/tab/wave_0_1998_data.tab", sep = "\t", header = T) # wave 0, part 1
wave02 <- read.table("data/raw/tab/wave_0_1999_data.tab", sep = "\t", header = T) # wave 0, part 2
wave03 <- read.table("data/raw/tab/wave_0_2001_data.tab", sep = "\t", header = T) # wave 0, part 3

# Combine  all data from wave 0 and remove unnecessary files
wave_0_c <- rbind.fill(wave01, wave02, wave03)  
rm(list = c("wave01", "wave02", "wave03")) 

# Load data from all other waves (starting from wave 2)
wave_1_c <- read.table("data/raw/tab/wave_1_core_data_v3.tab", sep = "\t", header = T) # wave 1
wave_2_c <- read.table("data/raw/tab/wave_2_core_data_v4.tab", sep = "\t", header = T)
wave_3_c <- read.table("data/raw/tab/wave_3_elsa_data_v4.tab", sep = "\t", header = T, fill = T) 
wave_4_c <- read.table("data/raw/tab/wave_4_elsa_data_v3.tab", sep = "\t", header = T)
wave_5_c <- read.table("data/raw/tab/wave_5_elsa_data_v4.tab", sep = "\t", header = T)
wave_6_c <- read.table("data/raw/tab/wave_6_elsa_data_v2.tab", sep = "\t", header = T)
wave_7_c <- read.table("data/raw/tab/wave_7_elsa_data.tab"   , sep = "\t", header = T)

# Select relevant variables from each wave
wave_0_c  <- wave_0_c[,c("idauniq", "ager", "educend", "topqual2", "bmival", "ethnicr")]

wave_1_c  <- wave_1_c[,c("idauniq", "elsa", "dhsex", "dhager", "fqethnr", "indoc",
                         "heska", "scptr", "scghqa", "scghqb",
                         # CVD variables
                         "hedia01", "hedia02", "hedia03", "hedia04", "hedia05", "hedia06", 
                         "hedia07", "hedia08", "hedia09", "hedia10")]

wave_2_c  <- wave_2_c[,c("idauniq", "fqethnr", "indager",
                         # CVD variables
                         "hedia01", "hedia02", "hedia03", "hedia04", "hedia05", "hedia06", 
                         "hedia07", "hedia08", "hedia09", 
                         "w2indout", "HeAge", "HeAgeR", "HeAgeRY", "Henmst",
                         # IADL variables
                         "headb01", "headb02", "headb03", "headb04", "headb05", "headb06", 
                         "headb07", "headb08", "headb09", "headb10", "headb11", "headb12", 
                         "headb13",
                         # depressive symptoms
                         "PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", 
                         "PScedG", "PScedH",
                         # life satisfacrtion
                         "sclifea", "sclifeb", "sclifec", "sclifed", "sclifee", 
                         # loneliness
                         "scfeela", "scfeelb", "scfeelc")]

wave_3_c  <- wave_3_c[,c("idauniq", "w3indout", "indager",
                         
                         "scghqa", "scghqb", "scghqc", "scghqd",
                         "scghqe", "scghqf", "scghqg", "scghqh", "scghqi", "scghqj",
                         "scghqk", "scghql", 
                         # stroke newly diagnosed
                         "hediast", "dhedimst", "heage", "heager", "heagery", "henmst",
                         # depressive symptoms
                         "psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", 
                         "pscedg", "pscedh",
                         # life satisfaction
                         "sclifea", "sclifeb", "sclifec", "sclifed", "sclifee", 
                         # loneliness
                         "scfeela", "scfeelb", "scfeelc")]

wave_4_c  <- wave_4_c[,c("idauniq", "indager", "outindw4",
                         # stroke newly diagnosed
                         "hediast", "hedimst", "heage", "heager", "heagery", "henmst",
                         # depressive symptoms
                         "psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", 
                         "pscedg", "pscedh", 
                         # life satisfaction
                         "sclifea", "sclifeb", "sclifec", "sclifed", "sclifee", 
                         # loneliness
                         "scfeela", "scfeelb", "scfeelc")]

wave_5_c  <- wave_5_c[,c("idauniq", "indager", "w5indout", 
                         # stroke newly diagnosed
                         "hediast", "hedimst", "heage", "heager", "heagery", "henmst",
                         # depressive symptoms
                         "psceda", "pscedb", "pscedc", "pscedd" , "pscede" , "pscedf", 
                         "pscedg", "pscedh", 
                         # life satisfaction
                         "sclifea", "sclifeb", "sclifec", "sclifed", "sclifee", 
                         # loneliness
                         "scfeela", "scfeelb", "scfeelc")]

wave_6_c  <- wave_6_c[,c("idauniq", "indager", "w6indout",
                         # stroke newly diagnosed
                         "hediast", "hedimst", "HeAge", "HeAgeR", "HeAgeRY", "HeNmSt",
                         # depressive symptoms
                         "PScedA", "PScedB", "PScedC", "PScedD" , "PScedE" , "PScedF", 
                         "PScedG", "PScedH", 
                         # life satisfaction
                         "sclifea", "sclifeb", "sclifec", "sclifed", "sclifee",
                         # loneliness
                         "scfeela", "scfeelb", "scfeelc")]

wave_7_c  <- wave_7_c[,c("idauniq", "indager", 
                         # stroke newly diagnosed
                         "hediast", "hedimst", 
                         # depressive symptoms
                         "PScedA", "PScedB", "PScedC", "PScedD" , "PScedE" , "PScedF", 
                         "PScedG", "PScedH", 
                         # life satisfaction
                         "sclifea", "sclifeb", "sclifec", "sclifed", "sclifee", 
                         # loneliness
                         "scfeela", "scfeelb", "scfeelc")]

# Harmonise naming of depression variables across waves (rename wave 2, 6, and 7 items)
setnames(wave_2_c, 
         old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", "PScedG", "PScedH"), 
         new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", "pscedg", "pscedh"))

setnames(wave_6_c, 
         old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", "PScedG", "PScedH"), 
         new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", "pscedg", "pscedh"))

setnames(wave_7_c, 
         old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", "PScedG", "PScedH"), 
         new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", "pscedg", "pscedh"))


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
data$wave_1_c[is.na(data$wave_2_c)] <- 0
data$wave_1_c[is.na(data$wave_2_n)] <- 0
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


# # ------------------------------------------------------------
# # 2) Recode variables demographic variables and co-variates
# # ------------------------------------------------------------
# 
# # 2.1) Age
# 
# ### Recode age variable; 109 adults aged > 89 have age coded as 99; 
# ### these are set as missings
# data$w1_dhager[data$w1_dhager == 99] <- NA
# 
# 
# # 2.2) Sex and Smoking
# 
# ### Recode all binary variables to 0 (no) and 1 (yes); before 1 (yes), 2 (no)
# for (i in names(data[,c(grep("w1_dhsex", colnames(data)), # 0 = female 
#                         grep("w1_heska", colnames(data)), # current smoking
#                         grep("w1_scptr", colnames(data)) # living with someone
# )])) {
#   data[[i]][data[[i]]==2] <- 0}
# 
# 
# # 2.3) Ethnicity
# 
# ### Recode ethnicity so "white" is coded as 1 and "non-white" as 0
# data$w0_ethni <- ifelse(data$w0_ethnicr == 1, 1, 0)
# 
# 
# # 2.4) Education
# 
# ### Re-code education variable (higher education? (yes / no))
# data$w0_topqual2[data$w0_topqual2 == 6] <- NA # recode foreign / other qual to NA
# data$w0_topqual2[data$w0_topqual2 == 8] <- NA # recode full-time students to NA
# data$w0_educ <- ifelse(data$w0_topqual2 == 1, 1, 0) 
# 
# 
# # 2.5) Hypertension
# 
# ### Create hypertension variable from CVD-variables at wave 1
# data$w1_hypt <- ifelse(data$w1_hedia01 == 1, 1, 0)
# data$w1_hypt[data$w1_hedia02 == 1 | data$w1_hedia03 == 1 | data$w1_hedia04 == 1 |
#                data$w1_hedia05 == 1 | data$w1_hedia06 == 1 | data$w1_hedia07 == 1 |
#                data$w1_hedia08 == 1 | data$w1_hedia09 == 1] <- 1 
# 
# 
# # 2.6) Diabetes
# 
# ### Create diabetes variable from CVD-variables at wave 1
# data$w1_diab <- ifelse(data$w1_hedia01 == 7, 1, 0)
# data$w1_diab[data$w1_hedia02 == 7 | data$w1_hedia03 == 7 | data$w1_hedia04 == 7 |
#                data$w1_hedia05 == 7 | data$w1_hedia06 == 7 | data$w1_hedia07 == 7 |
#                data$w1_hedia08 == 7 | data$w1_hedia09 == 7] <- 1 
# 
# 
# # 2.7) Recode some variables as factors for imputation
# 
# data$w0_ethni <- as.factor(data$w0_ethni)
# data$w1_dhsex <- as.factor(data$w1_dhsex)
# data$w0_educ  <- as.factor(data$w0_educ)
# data$w1_hypt  <- as.factor(data$w1_hypt)
# data$w1_heska <- as.factor(data$w1_heska)
# data$w1_diab  <- as.factor(data$w1_diab)
# data$w1_scptr <- as.factor(data$w1_scptr)
# 
# 
# # ---------------------------------------------
# # 3) Prepare treatment variable: Stroke cases
# # ---------------------------------------------
# 
# # Create variable: did participants report a "stroke ever diagnosed" in wave 1?
# data$w1_stroke <- ifelse(data$w1_hedia01 == 8, 1, 0)
# data$w1_stroke[data$w1_hedia02 == 8 | data$w1_hedia03 == 8 | data$w1_hedia04 == 8 |
#                  data$w1_hedia05 == 8 | data$w1_hedia06 == 8 | data$w1_hedia07 == 8 |
#                  data$w1_hedia08 == 8 | data$w1_hedia09 == 8] <- 1 
# 
# 
# # # Create stroke: did participants report a "newly diagnosed stroke" in wave 2?
# # data$w2_stroke <- ifelse(data$w2_hedia01 == 8, 1, 0)
# # data$w2_stroke[data$w2_hedia02 == 8 | data$w2_hedia03 == 8 | data$w2_hedia04 == 8 |
# #                  data$w2_hedia05 == 8 | data$w2_hedia06 == 8 | data$w2_hedia07 == 8 |
# #                  data$w2_hedia08 == 8 | data$w2_hedia09 == 8] <- 1 
# 
# # Create variable indicating whether participants had a new stroke 
# # in wave 2 or 3
# # data$st_case <- ifelse(data$w1_stroke == 0 & 
# #                          (data$w2_stroke == 1 | data$w3_hediast == 1), 1, 0)
# 
# # data$st_case[data$w1_stroke == 0 | data$w1_stroke  == 1] <- 0
# # data$st_case[is.na(data$w2_stroke) & is.na(data$w3_hediast)] <- NA
# # data$st_case[data$w2_stroke  == 0] <- 0
# # data$st_case[data$w3_hediast == 0] <- 0
# # #data$st_case[data$w1_stroke == 0 & data$w2_stroke  == 1] <- 1
# # data$st_case[data$w1_stroke == 0 & data$w3_hediast == 1] <- 1
# 
# # How many? 
# # table(data$st_case, useNA = "always") # 206


# ----------------------------
# Prepare outcome variables
# ----------------------------

# Depressive Symptoms (CESD, 8 items)
# --------------------------------------

# Recode items to dummies (0 = no, 1 = yes)
for (i in names(data[,c(grep("psced", colnames(data)))])) {
  data[[i]][data[[i]] == 2] <- 0}

# Reverse item d 
data$w2_pscedd_r <- 1 - data$w2_pscedd
data$w3_pscedd_r <- 1 - data$w3_pscedd
data$w4_pscedd_r <- 1 - data$w4_pscedd
data$w5_pscedd_r <- 1 - data$w5_pscedd
data$w6_pscedd_r <- 1 - data$w6_pscedd
data$w7_pscedd_r <- 1 - data$w7_pscedd

# Reverse item f 
data$w2_pscedf_r <- 1 - data$w2_pscedf
data$w3_pscedf_r <- 1 - data$w3_pscedf
data$w4_pscedf_r <- 1 - data$w4_pscedf
data$w5_pscedf_r <- 1 - data$w5_pscedf
data$w6_pscedf_r <- 1 - data$w6_pscedf
data$w7_pscedf_r <- 1 - data$w7_pscedf

# Select all CES-D items for each wave 
w2_dep_items <- data[,c("w2_psceda"  , "w2_pscedb", "w2_pscedd_r", "w2_pscede", 
                        "w2_pscedf_r", "w2_pscedg", "w2_pscedh")] # wave 2
w3_dep_items <- data[,c("w3_psceda"  , "w3_pscedb", "w3_pscedd_r", "w3_pscede", 
                        "w3_pscedf_r", "w3_pscedg", "w3_pscedh")] # wave 3
w4_dep_items <- data[,c("w4_psceda"  , "w4_pscedb", "w4_pscedd_r", "w4_pscede", 
                        "w4_pscedf_r", "w4_pscedg", "w4_pscedh")] # wave 4
w5_dep_items <- data[,c("w5_psceda"  , "w5_pscedb", "w5_pscedd_r", "w5_pscede", 
                        "w5_pscedf_r", "w5_pscedg", "w5_pscedh")] # wave 5
w6_dep_items <- data[,c("w6_psceda"  , "w6_pscedb", "w6_pscedd_r", "w6_pscede", 
                        "w6_pscedf_r", "w6_pscedg", "w6_pscedh")] # wave 6
w7_dep_items <- data[,c("w7_psceda"  , "w7_pscedb", "w7_pscedd_r", "w7_pscede", 
                        "w7_pscedf_r", "w7_pscedg", "w7_pscedh")] # wave 7

# Calculate sum score of depressive symptoms (people with missing values are excluded)
data$w2_dep_sum <- rowSums(w2_dep_items)
data$w3_dep_sum <- rowSums(w3_dep_items)
data$w4_dep_sum <- rowSums(w4_dep_items)
data$w5_dep_sum <- rowSums(w5_dep_items)
data$w6_dep_sum <- rowSums(w6_dep_items)
data$w7_dep_sum <- rowSums(w7_dep_items)

# # Double check missing items 
# aggr(data[c("w2_psceda", "w2_pscedb", "w2_pscedd", "w2_pscede", "w2_pscedf", "w2_pscedg", 
#             "w2_pscedh", "w2_dep_sum")], numbers = TRUE, prop = c(TRUE, FALSE))


# Life satisfaction (SWLS, 5 items)
# -----------------------------------

# Select all SWLS items for each wave 
w2_swl_items <- data[,c("w2_sclifea", "w2_sclifeb", "w2_sclifec", "w2_sclifed", "w2_sclifee")] # wave 2
w3_swl_items <- data[,c("w3_sclifea", "w3_sclifeb", "w3_sclifec", "w3_sclifed", "w3_sclifee")] # wave 3
w4_swl_items <- data[,c("w4_sclifea", "w4_sclifeb", "w4_sclifec", "w4_sclifed", "w4_sclifee")] # wave 4
w5_swl_items <- data[,c("w5_sclifea", "w5_sclifeb", "w5_sclifec", "w5_sclifed", "w5_sclifee")] # wave 5
w6_swl_items <- data[,c("w6_sclifea", "w6_sclifeb", "w6_sclifec", "w6_sclifed", "w6_sclifee")] # wave 6
w7_swl_items <- data[,c("w7_sclifea", "w7_sclifeb", "w7_sclifec", "w7_sclifed", "w7_sclifee")] # wave 7

# Calculate mean scores for life satisfaction
data$w2_swl_mean <- rowMeans(w2_swl_items)
data$w3_swl_mean <- rowMeans(w3_swl_items)
data$w4_swl_mean <- rowMeans(w4_swl_items)
data$w5_swl_mean <- rowMeans(w5_swl_items)
data$w6_swl_mean <- rowMeans(w6_swl_items)
data$w7_swl_mean <- rowMeans(w7_swl_items)

# Recode scale so that higher values indicate higher life satisfaction
data$w2_swl_mean <- 8-data$w2_swl_mean
data$w3_swl_mean <- 8-data$w3_swl_mean
data$w4_swl_mean <- 8-data$w4_swl_mean
data$w5_swl_mean <- 8-data$w5_swl_mean
data$w6_swl_mean <- 8-data$w6_swl_mean
data$w7_swl_mean <- 8-data$w7_swl_mean


# Loneliness (UCLA scale, 3 items)
# ---------------------------------------

# Select all loneliness items for each wave 
w2_lon_items <- data[,c("w2_scfeela", "w2_scfeelb", "w2_scfeelc")] # wave 2
w3_lon_items <- data[,c("w3_scfeela", "w3_scfeelb", "w3_scfeelc")] # wave 3
w4_lon_items <- data[,c("w4_scfeela", "w4_scfeelb", "w4_scfeelc")] # wave 4
w5_lon_items <- data[,c("w5_scfeela", "w5_scfeelb", "w5_scfeelc")] # wave 5
w6_lon_items <- data[,c("w6_scfeela", "w6_scfeelb", "w6_scfeelc")] # wave 6
w7_lon_items <- data[,c("w7_scfeela", "w7_scfeelb", "w7_scfeelc")] # wave 7

# Calculate mean scores for loneliness
data$w2_lon_mean <- rowMeans(w2_lon_items)
data$w3_lon_mean <- rowMeans(w3_lon_items)
data$w4_lon_mean <- rowMeans(w4_lon_items)
data$w5_lon_mean <- rowMeans(w5_lon_items)
data$w6_lon_mean <- rowMeans(w6_lon_items)
data$w7_lon_mean <- rowMeans(w7_lon_items)


# # -------------------------------------------------
# # 5) Prepare mediator variable: Disability items
# # -------------------------------------------------
# 
# # Get disability data from wave 2 (13 items)
# pditems.2 <- select(data, contains("w2_headb"))
# 
# # Code 96 to 0 and all others to 1
# # i.e. if people report any endorsement of a disability, the item is coded as
# pditems.2 <- pditems.2 %>% mutate_all(funs(recode(., '96' = 0, 
#                                                   '1'  = 1, 
#                                                   '2'  = 1, 
#                                                   '3'  = 1, 
#                                                   '4'  = 1, 
#                                                   '5'  = 1,
#                                                   '6'  = 1, 
#                                                   '7'  = 1, 
#                                                   '8'  = 1, 
#                                                   '9'  = 1, 
#                                                   '10' = 1, 
#                                                   '11' = 1, 
#                                                   '12' = 1,
#                                                   '13' = 1)))
# 
# # Calculate sum scores items
# data$disability <- rowSums(pditems.2, na.rm = T)
# table(data$disability)


# ----------------------------
# Exclusion of participants
# ----------------------------

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
data <- subset(data, data$w1_hedia == 0) # Keep only non-stroke


# --------------------------------------------------
# Indentify participants with stroke during study
# --------------------------------------------------

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


data$n_strokes <- 
  rowSums(data[c("w2_hediast", "w3_hediast", "w4_hediast", "w5_hediast", "w6_hediast")], 
          na.rm = T)
table(data$n_strokes, useNA = "always")

# Only include people with 1 stroke during study
data <- subset(data, n_strokes == 1) # 411 

# Check peoples dates for their stroke in each wave
table(data$w2_HeAgeRY, useNA = "always") 
table(data$w3_heagery, useNA = "always") 
table(data$w4_heagery, useNA = "always")
table(data$w5_heagery, useNA = "always")
table(data$w6_HeAgeRY, useNA = "always")

# Only include people with reasonable dates for their stroke
data <- subset(data, data$w2_HeAgeRY > 2001 | is.na(data$w2_HeAgeRY))  
data <- subset(data, data$w3_heagery > 2003 | is.na(data$w3_heagery))  
data <- subset(data, data$w4_heagery > 2005 | is.na(data$w4_heagery))  
data <- subset(data, data$w5_heagery > 2007 | is.na(data$w5_heagery))  
data <- subset(data, data$w6_HeAgeRY > 2009 | is.na(data$w6_HeAgeRY)) 

# Create new time variables indicating the time of stroke, before and after
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

# ---------------
# Reformat data
# ---------------

data_red <- 
  data[,c("idauniq", 
          "w2_time"    , "w3_time"    , "w4_time"    , "w5_time"    , "w6_time"    , "w7_time"    ,
          "w2_hediast" , "w3_hediast" , "w4_hediast" , "w5_hediast" , "w6_hediast" , "w7_hediast" , 
          "w2_dep_sum" , "w3_dep_sum" , "w4_dep_sum" , "w5_dep_sum" , "w6_dep_sum" , "w7_dep_sum" ,
          "w2_swl_mean", "w3_swl_mean", "w4_swl_mean", "w5_swl_mean", "w6_swl_mean", "w7_swl_mean",
          "w2_lon_mean", "w3_lon_mean", "w4_lon_mean", "w5_lon_mean", "w6_lon_mean", "w7_lon_mean")]

data_long <- reshape(data_red, direction = "long",
                     idvar = "idauniq", 
                     varying = list(c("w2_hediast", "w3_hediast", "w4_hediast", "w5_hediast", 
                                      "w6_hediast", "w7_hediast"),
                                    grep("time",     colnames(data), value = T), 
                                    grep("dep_sum",  colnames(data), value = T), 
                                    grep("swl_mean", colnames(data), value = T), 
                                    grep("lon_mean", colnames(data), value = T)), 
                     timevar = "wave", 
                     times = c("w2", "w3", "w4", "w5", "w6", "w7"), 
                     v.names = c("stroke", "time", "depress", "lifesat", "loneli"))
                  
# ---------------------------
# Save preprocessed data
# ---------------------------

# save(wave0, wave1, wave2, wave3, data, data_excl,  
#     file = "data/elsa/processed/elsa_proc_data.RData")

