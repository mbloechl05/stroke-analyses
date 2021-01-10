## ===============================================================
## Helper functions
## (contact maria.bloechl@gmail.com in case of questions)
## ==============================================================

# --------------------------------------------------------------
# Function to create data frame from freq table of binary vars
# --------------------------------------------------------------

descr_b <- function(var) {
  df   <- as.data.frame(table(var, useNA = "always"))
  df.t <- setNames(data.frame(t(df[,-1])),df[,1])
  colnames(df.t)[3] <- "missing"
  df.t <- dplyr::rename(df.t, yes = "1", no = "0")
  return(df.t)
}

# ------------------------------------------------------
# Function to calculate pooled mean, pooled sd, and se
# ------------------------------------------------------

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
                                function(x) c(descr_b(x[, var]))))
    df       <- data.frame(t(vapply(pool,unlist,unlist(pool[[1]]))))
    pool_n   <- sum(df$yes)/nrow(df)
    pool_ran <- range(df$yes)
    pool_per <- (sum(df$yes)/nrow(df))/rowSums(df)[1]*100
    pools    <- list(pooled_n = pool_n, pool_range = pool_ran, 
                     pool_perc = pool_per)
  }   
  return(pools)
}

# ------------------------------------------------------
# Functions to calculate % of missing values for table
# ------------------------------------------------------

# Before creating the table, we need to create some helper functions

# Function to calculate % missing data for a continuous variable
pm_c <- function(var) {
  percent.missing <- 100-var/N*100
  return(percent.missing)
}

# Function to calculate % missing data for a binary variable
pm_b <- function(var) {
  percent.missing <- var$missing/rowSums(var)*100
  return(percent.missing)
}

# Function to calculate % observed for binary ariables
po <- function(var) {
  percent.observed <- var$yes / (var$yes+var$no)*100
  return(percent.observed)
}

# -------------------------------------------------------------------------
# Function to calculate pooled descriptives (pooled mean, pooled sd, and 
# standardised differences from imputed data
# -------------------------------------------------------------------------

describeBy_imp <- function(x, data, binary, strokevar) {
  if (binary == F) {
    # pooled descriptives
    pool     <- with(data,
                     by(data, .imp, 
                        function(y) c(describeBy(y[, x], 
                                                 group = y[, strokevar]))))
    pool_df  <- data.frame(t(vapply(pool,unlist,unlist(pool[[1]]))))
    pm_str   <- mean(pool_df$X1.mean) # pooled mean stroke
    pm_con   <- mean(pool_df$X0.mean) # pooled mean no stroke
    psd_str  <- mean(pool_df$X1.sd)   # pooled sd stroke
    psd_con  <- mean(pool_df$X0.sd)   # pooled sd no stroke
    # list all results
    pools_2 <- list(mean_str = pm_str, mean_con = pm_con, sd_str = psd_str, 
                    sd_con = psd_con) 
  } else {
    # pooled descriptives
    data[,x] <- factor(data[,x], levels = c("1", "0"))
    pool     <- with(data, 
                     by(data, .imp,
                        function(y) c(table(y[, x], y[, strokevar]))))
    pool_desc <- Reduce("+",pool)/length(pool)
    #   pool_desc <- as.data.frame(pool_desc)
    pn_str <- pool_desc[3]
    pn_con <- pool_desc[1]
    pperc_str <- 100/(pool_desc[3]+pool_desc[4])*pool_desc[3]
    pperc_con <- 100/(pool_desc[1]+pool_desc[2])*pool_desc[1]
    # list all results
    pools_2 <- list(n_str = pn_str, n_con = pn_con, perc_str = pperc_str, 
                    perc_con = pperc_con) 
  }
  return(pools_2)
}



