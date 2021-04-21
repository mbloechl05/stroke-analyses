# =====================
#  Packages
# =====================

# Load all packages that are necessary to run the preprocessing, imputation and 
# matching, and all analyses -- or only parts of it 

lapply(c("plyr", 
         "dplyr", 
         "tidyr", 
         "VIM", 
         "stats", 
         "psych", 
         "data.table", 
         "tibble", 
         "mice",
         "miceadds", 
         "mitml", 
         "flextable", 
         "stddiff",
         "officer", 
         "MatchIt", 
         "cobalt",
         "forcats", 
         "ggplot2", 
         "Rmisc", 
         "purrr",
         "reshape", 
         "car", 
         "lme4", 
         "lmerTest", 
         "arm"), 
       require, character.only = TRUE)

