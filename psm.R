## ===============================
## Propensity score matching
## ===============================

# Load packages
library(dplyr)
library(MatchIt) # for matchit()
library(tibble)
library(mitml)
library(cobalt)  # for bal.tab()
library(forcats) # for fct_rev()

# Load preprocessed data 
load("data/processed/proc_data.RData")

# Load imputed data
load("data/processed/imp_data.RData")
impdat <- mice::complete(imp, action = "long", include = FALSE)


# --------------------------------
# 1) Propensity score matching
# --------------------------------

# Create function: fun_match()
### This function allows to conduct PSM in >1 dataset and save the matched samples 
fun_match <- function(sample){
  # conduct the matching procedure
  matched <- matchit(n_strokes ~ w1_dhager + w1_dhsex + w0_ethni + w0_educ, # + 
                       #w1_heska + w1_diab + w0_bmival + w1_hypt +  w1_scptr,
                     data = sample,
                     method = "nearest", 
                     caliper = 0.2,
                     ratio = 20) # N of controls matched to 1 case
  # get and save matched data in wide format
  data.match.w <- match.data(matched)[1:ncol(data_mi)] 
  # add new id variable
  data.match.w <- rowid_to_column(data.match.w, var = "rowid") 
  # reformat to long format
#  data.match.l <- melt(data.match.w, id.vars = c("rowid", "n_strokes","w1_dhager",
 #                                                "w1_dhsex", "w0_ethni", "w0_educ",
  #                                               "w1_heska", "w1_diab", "w1_hypt",  
   #                                              "w0_bmival", "w1_scptr"), 
    #                   value.name = "depression_score") 
#  data.match.l$time <- ifelse(data.match.l$variable == "w3_depm", 1, 0)
 # data.match.l$time <- as.factor(data.match.l$time)
  #data.match.l$st_case <- as.factor(data.match.l$st_case)
  # combine list of data frames to list of list of data frames
  datalist <- list(matched = matched, data.match.w = data.match.w #, 
                   #data.match.l = data.match.l
                   )
  return(datalist)
}

# Now conduct PSM in all imputed data sets using fun_match()
matchlist <- lapply(implist, fun_match) 
matchlist


# ----------------------
# 2) Save matched data 
# ----------------------

# Save the matched data and all the other data for further analyses
save(matchlist, matchedlist, 
     file = "data/processed/matched_data.RData")

