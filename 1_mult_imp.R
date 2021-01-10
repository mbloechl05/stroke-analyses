## ======================
## Multiple imputation
## ======================

# Clean work space
rm(list = ls())

# Load preprocessed data 
load("data/processed/proc_data.RData")


# --------------------------------------
# 1) Impute missing data on covariates
# --------------------------------------

# PSM requires full covariate data; since we only have a few cases, we will 
# use multiple imputation to retain maximum power. 
# (Note, however, that we neither impute the outcome nor te treatment.) 


# 1.1) Create new data frame with relavant variables for PSM
# ------------------------------------------------------------

data_mi <- data_red %>% 
  dplyr::select(idauniq, 
                stroke, 
                w1_dhager, 
                w1_dhsex, 
                w0_ethni, 
                w0_educ, 
                w1_hypt, 
                w1_diab, 
                w0_bmival, 
                w1_heska, 
                w1_adl_sum, 
                w1_mem_sum)


# 1.2) Set up multiple imputation
# ----------------------------------

# "Straw-person" imputation to obtain default values
imp0 <- mice(data_mi, m = 1, maxit = 0)

# Extract default predictor matrix and imputation method 
impMethod <- imp0$method
impMethod[] <- ""

# Overwrite default predictor matrix
predMatrix <- make.predictorMatrix(data_mi) 
predMatrix[,] <- 0

# Create new predictor matrix
predMatrix["w1_dhager",  
           c("w1_dhsex", 
             "w0_ethni", 
             "w0_educ", 
             "w1_hypt", 
             "w1_diab",
             "w0_bmival", 
             "w1_heska", 
             "w1_adl_sum", 
             "w1_mem_sum")] <- 1

predMatrix["w0_ethni", 
           c("w1_dhager", 
             "w1_dhsex", 
             "w0_educ", 
             "w1_hypt", 
             "w1_diab",
             "w0_bmival", 
             "w1_heska", 
             "w1_adl_sum", 
             "w1_mem_sum")] <- 1

predMatrix["w1_dhsex",   
           c("w1_dhager", 
             "w0_ethni", 
             "w0_educ", 
             "w1_hypt", 
             "w1_diab",
             "w0_bmival", 
             "w1_heska", 
             "w1_adl_sum", 
             "w1_mem_sum")] <- 1

predMatrix["w0_educ",    
           c("w1_dhager", 
             "w0_ethni", 
             "w1_dhsex", 
             "w1_hypt", 
             "w1_diab", 
             "w0_bmival", 
             "w1_heska", 
             "w1_adl_sum", 
             "w1_mem_sum")] <- 1

predMatrix["w1_hypt",   
           c("w1_dhager", 
             "w0_ethni", 
             "w1_dhsex", 
             "w0_educ", 
             "w1_diab",
             "w0_bmival", 
             "w1_heska", 
             "w1_adl_sum", 
             "w1_mem_sum")] <- 1

predMatrix["w0_bmival",  
           c("w1_dhager", 
             "w0_ethni", 
             "w1_dhsex", 
             "w0_educ", 
             "w1_diab",
             "w1_hypt", 
             "w1_heska",  
             "w1_adl_sum", 
             "w1_mem_sum")] <- 1

predMatrix["w1_heska",   
           c("w1_dhager", 
             "w0_ethni",
             "w1_dhsex", 
             "w0_educ", 
             "w1_diab",
             "w1_hypt", 
             "w0_bmival", 
             "w1_adl_sum", 
             "w1_mem_sum")] <- 1

predMatrix["w1_diab",    
           c("w1_dhager", 
             "w0_ethni", 
             "w1_dhsex", 
             "w0_educ", 
             "w1_hypt", 
             "w0_bmival", 
             "w1_heska", 
             "w1_adl_sum", 
             "w1_mem_sum")] <- 1

predMatrix["w1_adl_sum",   
           c("w1_dhager", 
             "w0_ethni", 
             "w1_dhsex", 
             "w0_educ", 
             "w1_hypt", 
             "w0_bmival", 
             "w1_heska", 
             "w1_mem_sum", 
             "w1_diab")] <- 1

predMatrix["w1_mem_sum",   
           c("w1_dhager", 
             "w0_ethni" , 
             "w1_dhsex", 
             "w0_educ", 
             "w1_hypt", 
             "w0_bmival", 
             "w1_heska", 
             "w1_diab", 
             "w1_adl_sum")] <- 1

### Imputation methods
impMethod[c("w0_ethni", 
            "w1_dhsex", 
            "w0_educ", 
            "w1_hypt", 
            "w1_heska", 
            "w1_diab")] <- "logreg" # log. regression

impMethod[c("w1_dhager", 
            "w0_bmival", 
            "w1_adl_sum", 
            "w1_mem_sum")] <- "pmm" 


# 1.3) Run imputation
# ---------------------

imp <- mice(data_mi, # data set
            m = 20, # number of imputations (20)
            maxit = 10, # number of iterations per imputation (10)
            method = impMethod, # imputation method
            predictorMatrix = predMatrix # predictor matrix
)    


# 1.4) Save imputed data sets to workspace
# ------------------------------------------

implist <- mitml::mids2mitml.list(imp) 


# ----------------------
# 2) Save imputed data 
# ----------------------

save(imp, 
     implist, 
     data_mi, file = "data/processed/imp_data.RData")

