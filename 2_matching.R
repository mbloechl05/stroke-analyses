## =============================
## Propensity score matching
## =============================

# Load preprocessed data 
load("data/processed/proc_data.RData")

# Load imputed data
load("data/processed/imp_data.RData")
impdat <- mice::complete(imp, action = "long", include = FALSE)


# --------------------------------
# 1) Propensity score matching
# --------------------------------

# 1.1) Create function: fun_match()
# --------------------------------
# This function allows to conduct PSM in > 1 dataset and save the matched samples 
fun_match <- function(sample){
  # conduct the matching procedure
  matched <- matchit(stroke ~ 
                       # matching variables
                       w1_dhager + 
                       w1_dhsex + 
                       w0_ethni + 
                       w0_educ + 
                       w1_heska + 
                       w1_diab + 
                       w0_bmival + 
                       w1_hypt + 
                       w1_adl_sum + 
                       w1_mem_sum,
                     # matching details
                     data = sample,
                     method = "nearest", 
                     caliper = 0.2,
                     ratio = 20 # N of controls matched to 1 case
                     ) 
  # get and save matched data in wide format
  data.match.w <- match.data(matched)[1:ncol(data_mi)] 
  # add new id variable
  data.match.w <- rowid_to_column(data.match.w, var = "rowid") 
  # combine list of data frames to list of list of data frames
  datalist <- list(matched = matched, data.match.w = data.match.w)
  return(datalist)
}


# 1.2) Matching procedure
# --------------------------

# Now conduct PSM in all imputed data sets using fun_match() and save as list
matchlist <- lapply(implist, fun_match) 
matchlist


# 1.3) Finally, also save basic matching info
# ---------------------------------------------

# Unlist list of lists
for (i in seq(matchlist)) 
  assign(paste0("matched",i), matchlist[[i]]$matched)

# Then list again lists of similar type (matched)
matchedlist <- 
  list(matched1,  matched2,  matched3,  matched4,  matched5,  matched6,  
       matched7,  matched8,  matched9,  matched10, matched11, matched12, 
       matched13, matched14, matched15, matched16, matched17, matched18, 
       matched19, matched20)


# ----------------------
# 2) Save matched data 
# ----------------------

# Save the matched data and all the other data for further analyses
save(matchlist, 
     matchedlist, 
     file = "data/processed/matched_data.RData")

