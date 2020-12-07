## =====================================================================
## Multiple imputation
## =====================================================================

# Clean work space
rm(list = ls())

# Load packages
library(dplyr)
library(psych)     # for describe()
library(tibble)
library(mice)      # for imputation
library(miceadds)
library(mitml)
library(flextable) # for flextable()
library(officer)   # for read_docx()

# Load preprocessed data 
load("data/processed/proc_data.RData")


# ----------------------------------
# 1) Check missing covariate data
# ----------------------------------

# 1.1) Prepare function to create data frame from freq table of binary vars
describe_bin <- function(var) {
  df   <- as.data.frame(table(var, useNA = "always"))
  df.t <- setNames(data.frame(t(df[,-1])),df[,1])
  colnames(df.t)[3] <- "missing"
  df.t <- rename(df.t, yes = "1", no = "0")
  return(df.t)
}


# 1.2) Descriptive stats for each variable in data frame

# ### calcualte descriptive stats 
age <- describe    (data_red$w1_dhager) 
gen <- describe_bin(data_red$w1_dhsex) # male gender?
eth <- describe_bin(data_red$w0_ethni) # white ethnicity?
edu <- describe_bin(data_red$w0_educ)
smo <- describe_bin(data_red$w1_heska)
bmi <- describe    (data_red$w0_bmival)
hyp <- describe_bin(data_red$w1_hypt)
dia <- describe_bin(data_red$w1_diab)
liv <- describe_bin(data_red$w1_scptr)
bde <- describe    (data_red$w2_dep_sum)
bls <- describe    (data_red$w2_swl_mean)

### show all descriptive stats in list
list("age"        = i.age, 
     "gender"     = i.gen, 
     "ethnicity"  = i.eth, 
     "education"  = i.edu, 
     "smoking"    = i.smo, 
     "bmi"        = i.bmi, 
     "hypert"     = i.hyp, 
     "diabetes"   = i.dia, 
     "cohab_marr" = i.liv)


# --------------------------------------
# 2) Impute missing data on covariates
# --------------------------------------

# PSM requires full covariate data; since we only have a few cases, we will 
# use multiple imputation to retain maximum power. 
# (Note, however, that we neither impute the outcome nor te treatment.) 


# 2.1) Create new data frame with relavant variables for PSM

data_mi <- data_red %>% 
  dplyr::select(idauniq, n_strokes, w1_dhager, w1_dhsex, w0_ethni, w0_educ, 
                w1_hypt, w1_diab  , w0_bmival, w1_heska, w1_scptr, w2_dep_sum)


# 2.2) Set up multiple imputation

### "Straw-men" imputation to obtain default values
imp0 <- mice(data_mi, m = 1, maxit = 0)

### Extract default predictor matrix and imputation method 
impMethod <- imp0$method
impMethod[] <- ""

### Overwrite default predictor matrix
predMatrix <- make.predictorMatrix(data_mi) 
predMatrix[,] <- 0

### Create new predictor matrix
predMatrix["w1_dhager",  
           c("w1_dhsex", "w0_ethni" , "w0_educ" , "w1_hypt", 
             "w1_diab" , "w0_bmival", "w1_heska", "w1_scptr", "w2_dep_sum")] <- 1

predMatrix["w0_ethni", 
           c("w1_dhager", "w1_dhsex", "w0_educ", "w1_hypt", 
             "w0_bmival", "w1_heska", "w1_diab", "w1_scptr", "w2_dep_sum")] <- 1

predMatrix["w1_dhsex",   
           c("w1_dhager","w0_ethni" , "w0_educ", "w1_hypt", 
             "w0_bmival", "w1_heska", "w1_diab", "w1_scptr", "w2_dep_sum")] <- 1

predMatrix["w0_educ",    
           c("w1_dhager", "w0_ethni", "w1_dhsex", "w1_hypt", 
             "w0_bmival", "w1_heska", "w1_diab" , "w1_scptr", "w2_dep_sum")] <- 1

predMatrix["w1_hypt",   
           c("w1_dhager", "w0_ethni", "w1_dhsex","w0_educ", 
             "w0_bmival", "w1_heska", "w1_diab" , "w1_scptr", "w2_dep_sum")] <- 1

predMatrix["w0_bmival",  
           c("w1_dhager", "w0_ethni", "w1_dhsex", "w0_educ", 
             "w1_hypt"  , "w1_heska", "w1_diab" , "w1_scptr", "w2_dep_sum")] <- 1

predMatrix["w1_heska",   
           c("w1_dhager", "w0_ethni" ,"w1_dhsex", "w0_educ", 
             "w1_hypt"  , "w0_bmival", "w1_diab", "w1_scptr", "w2_dep_sum")] <- 1

predMatrix["w1_diab",    
           c("w1_dhager", "w0_ethni" , "w1_dhsex", "w0_educ", 
             "w1_hypt"  , "w0_bmival", "w1_heska", "w1_scptr", "w2_dep_sum")] <- 1

predMatrix["w1_scptr",   
           c("w1_dhager", "w0_ethni" , "w1_dhsex", "w0_educ", 
             "w1_hypt"  , "w0_bmival", "w1_heska", "w1_diab", "w2_dep_sum")] <- 1

predMatrix["w2_dep_sum",   
           c("w1_dhager", "w0_ethni" , "w1_dhsex", "w0_educ", 
             "w1_hypt"  , "w0_bmival", "w1_heska", "w1_diab", "w1_scptr")] <- 1


### Imputation methods
impMethod[c("w0_ethni", "w1_dhsex", "w0_educ", "w1_hypt", 
            "w1_heska", "w1_diab" , "w1_scptr")] <- "logreg" # log. regression

impMethod[c("w1_dhager", "w0_bmival", "w2_dep_sum")] <- "pmm" 


# 2.3) Run imputation

imp <- mice(data_mi, # data set
            m = 20, # number of imputations (20)
            maxit = 10, # number of iterations per imputation (10)
            method = impMethod, # imputation method
            predictorMatrix = predMatrix # predictor matrix
)    


# 2.4) Save imputed data sets to workspace

implist <- mitml::mids2mitml.list(imp) 


# ----------------------
# 3) Save imputed data 
# ----------------------

save(imp, implist, data_mi, file = "data/processed/imp_data.RData")


# ------------------------------------------------------
# 4) Descriptives observed and imputed data (Table S1)
# ------------------------------------------------------

# If you wish to re-run the following analyses, you can also load the 
# "elsa_imp_data.RData" file here; just uncomment the following line:
# load("data/elsa/processed/elsa_imp_data.RData")

# 3.1) Stack imputed datasets in long format, exclude the original data
impdat <- complete(imp, action = "long", include = FALSE)

# 3.2) Creat helper function to calculate pooled mean, pooled sd, and se
describe_imputed <- function(var, bin) {
  if (bin == F) {
    pool      <- with(impdat, by(impdat, .imp, 
                                 function(x) c(mean(x[, var]), sd(x[, var]))))
    pool_desc <- Reduce("+",pool)/length(pool)
    df        <- data.frame(t(vapply(pool,unlist,unlist(pool[[1]]))))
    pool_se   <- describe(df$X1)
    pools     <- list(pooled_descriptives = pool_desc, pooled_se = pool_se)
  }
  else {
    pool     <- with(impdat, by(impdat, .imp, 
                                function(x) c(describe_bin(x[, var]))))
    df       <- data.frame(t(vapply(pool,unlist,unlist(pool[[1]]))))
    pool_n   <- sum(df$yes)/nrow(df)
    pool_ran <- range(df$yes)
    pool_per <- (sum(df$yes)/nrow(df))/rowSums(df)[1]*100
    pools    <- list(pooled_n = pool_n, pool_range = pool_ran, 
                     pool_perc = pool_per)
  }   
  return(pools)
}


# 3.3) compute mean and standard deviation in each imputed dataset

### calcualte descriptive stats
i.age <- describe_imputed("w1_dhager", bin = F)
i.gen <- describe_imputed("w1_dhsex" , bin = T)
i.eth <- describe_imputed("w0_ethni" , bin = T)
i.edu <- describe_imputed("w0_educ"  , bin = T)
i.smo <- describe_imputed("w1_heska" , bin = T)
i.bmi <- describe_imputed("w0_bmival", bin = F)
i.hyp <- describe_imputed("w1_hypt"  , bin = T)
i.dia <- describe_imputed("w1_diab"  , bin = T)
i.liv <- describe_imputed("w1_scptr" , bin = T)

### show all descriptive stats in list
list("age"        = i.age, 
     "gender"     = i.gen, 
     "ethnicity"  = i.eth, 
     "education"  = i.edu, 
     "smoking"    = i.smo, 
     "bmi"        = i.bmi, 
     "hypert"     = i.hyp, 
     "diabetes"   = i.dia, 
     "cohab_marr" = i.liv)


# 3.4) Table S1: Descriptives observed and imputed covariates

### Before creating the table, we need to create some helper functions
### Helper one: Number of cases in data set

#####  UPDATE!!
N <- 6351

### Helper 2: Function to calculate % missing data for a continuous variable
pm_c <- function(var) {
  percent.missing <- 100-var/N*100
  return(percent.missing)
}

### Helper 3: Function to calculate % missing data for a binary variable
pm_b <- function(var) {
  percent.missing <- var$missing/rowSums(var)*100
  return(percent.missing)
}

### Helper 4: Function to calculate % observed for binary ariables
po <- function(var) {
  percent.observed <- var$yes / (var$yes+var$no)*100
  return(percent.observed)
}


### Now, we can create a data frame for stats that go into Table S1
mi_describe_frame = structure(list(
  # set variable names
  var = c("Age (in years)", "Male gender, yes", "White ethnicity, yes", 
          "Higher education, yes", "Current smoking, yes", "BMI (in kg/m²)", 
          "Diabetes, yes", "Hypertension, yes", "Living with someone, yes", 
          "Baseline mental distress"), 
  # get means or N from observed data
  o.m = c(age$mean,gen$yes, eth$yes, edu$yes, smo$yes, bmi$mean, dia$yes, 
          hyp$yes, liv$yes, bde$mean),
  # get SDs or % from observed data
  o.sd = c(age$sd,po(gen), po(eth), po(edu), po(smo), bmi$sd, po(dia), 
           po(hyp),po(liv), bde$sd),
  # get % missing from observed data
  o.miss = c(pm_c(age$n),pm_b(gen), pm_b(eth), pm_b(edu), pm_b(smo), 
             pm_c(bmi$n), pm_b(dia), pm_b(hyp), pm_b(liv), pm_c(bde$n)),
  # get pooled mean or N for imputed data
  i.pm = c(i.age$pooled_descriptives[1], i.gen$pooled_n, i.eth$pooled_n, 
           i.edu$pooled_n, i.smo$pooled_n, i.bmi$pooled_descriptives[1], 
           i.dia$pooled_n, i.hyp$pooled_n, i.liv$pooled_n, 
           i.bde$pooled_descriptives[1]), 
  # get pooled SD or % for imputed data
  i.psd = c(i.age$pooled_descriptives[2],i.gen$pool_perc, i.eth$pool_perc, 
            i.edu$pool_perc, i.smo$pool_perc, i.bmi$pooled_descriptives[2], 
            i.dia$pool_perc, i.hyp$pool_perc, i.liv$pool_perc, 
            i.bde$pooled_descriptives[2]), 
  # get SD of means or range for imputed data
  i.se = c(i.age$pooled_se$sd, i.gen$pool_range[1], i.eth$pool_range[1], 
           i.edu$pool_range[1], i.smo$pool_range[1], i.bmi$pooled_se$sd, 
           i.dia$pool_range[1], i.hyp$pool_range[1], i.liv$pool_range[1], 
           i.bde$pooled_se$sd),
  # get second value for range
  i.se2 = c(0, i.gen$pool_range[2], i.eth$pool_range[2], i.edu$pool_range[2], 
            i.smo$pool_range[2], 0, i.dia$pool_range[2], i.hyp$pool_range[2], 
            i.liv$pool_range[2], 0)), 
  
  class = "data.frame", 
  row.names = c(NA,-10L))

### Create a flex table from descriptive data frame
ts_one <- flextable(mi_describe_frame, 
                    col_keys = c("var", "o.m", "o.miss", "i.pm", "i.se")) 
ts_one <- set_header_labels(ts_one, var = "", 
                            o.m = "Observed data", o.miss = "Observed data",
                            i.pm = "Imputed data", i.se = "Imputed data")
ts_one <- flextable::compose(
  ts_one, j = "o.m", 
  value = as_paragraph("", as_chunk(sprintf("%.01f", o.m)),
                       " (", as_chunk(sprintf("%.01f", o.sd)))
)
ts_one <- flextable::compose(
  ts_one, j = "i.pm", 
  value = as_paragraph("", as_chunk(sprintf("%.01f", i.pm)),
                       " (", as_chunk(sprintf("%.01f", i.psd)))
)
ts_one <- flextable::compose(
  ts_one, j = "i.se", 
  value = as_paragraph("", as_chunk(sprintf("%.01f", i.se)),
                       "-", as_chunk(sprintf("%.01f", i.se2)))
)
ts_one <- merge_at(ts_one, i = 1, j = 2:3, part = "header")
ts_one <- merge_at(ts_one, i = 1, j = 4:5, part = "header")
ts_one <- add_header_row(
  ts_one, values = c("","Mean (SD) or N (%)", "% missing",
                     "Pooled mean (SD) or N (%)", "SE or range"), 
  top = FALSE 
)
ts_one

### Finally, export this bombastic table to word
read_docx() %>% 
  body_add_flextable(ts_one) %>% 
  print(target = "analyses/output/tables/TableS1_ELSA.docx")

