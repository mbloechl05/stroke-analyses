## ================
##  Data analysis
## ================

# load packages
library(ggplot2)
library(Rmisc) # for summarySEwithin
library(reshape)
library(lme4)
library(lmerTest)
library(car) # for recode
library(arm) # for se.fixef

# -----------------------------------------------
# Modelling time courses with respect to stroke
# -----------------------------------------------

# Create Dummy variables for time points (waves)

data_long$dummy_mins4 <- ifelse(data_long$time == -4, 1, 0) 
data_long$dummy_mins3 <- ifelse(data_long$time == -3, 1, 0)
data_long$dummy_mins2 <- ifelse(data_long$time == -2, 1, 0) 
data_long$dummy_mins1 <- ifelse(data_long$time == -1, 1, 0) 
data_long$dummy_plus1 <- ifelse(data_long$time ==  1, 1, 0) 
data_long$dummy_plus2 <- ifelse(data_long$time ==  2, 1, 0)
data_long$dummy_plus3 <- ifelse(data_long$time ==  3, 1, 0)
data_long$dummy_plus4 <- ifelse(data_long$time ==  4, 1, 0)
data_long$dummy_plus5 <- ifelse(data_long$time ==  5, 1, 0) 

# Depressive symptoms
# --------------------

# Define random-intercept-only model (reference = stroke 0)
m_depress <- depress ~ 1 + dummy_mins4 + dummy_mins3 + dummy_mins2 + dummy_mins1 + dummy_plus1 + 
  dummy_plus2 + dummy_plus3 + dummy_plus4 + dummy_plus5 + (1|idauniq)

# Fit model
fit_depress <- lmer(m_dep, data = data_long, REML = TRUE)

# Show model results
summary(fit_depress)


# Life satisfaction
# --------------------

# Define random-intercept-only model (reference = stroke 0)
m_lifesat <- lifesat ~ 1 + dummy_mins4 + dummy_mins3 + dummy_mins2 + dummy_mins1 + dummy_plus1 + 
  dummy_plus2 + dummy_plus3 + dummy_plus4 + dummy_plus5 + (1|idauniq)

# Fit model
fit_lifesat <- lmer(m_lifesat, data = data_long, REML = TRUE)

# Show model results
summary(fit_lifesat)


# ---------------------
# Model-implied plots
# ---------------------

# Depressive symptoms
# ---------------------

eff <- as.data.frame(fixef(fit_dep))
ses <- as.data.frame(se.fixef(fit_dep))

effs_depress <- cbind(eff, ses)
names(effs_depress) <- c("eff", "se")
effs_depress <- rownames_to_column(effs_depress, var = "var")

effs_depress$time <- recode(effs_depress$var, "'(Intercept)' = 0; 
                            'dummy_mins4' = -4; 
                            'dummy_mins3' = -3; 
                            'dummy_mins2' = -2; 
                            'dummy_mins1' = -1; 
                            'dummy_plus1' =  1; 
                            'dummy_plus2' =  2; 
                            'dummy_plus3' =  3; 
                            'dummy_plus4' =  4; 
                            'dummy_plus5' =  5")

effs_depress$value    <- effs_depress$eff[1] + effs_depress$eff
effs_depress$value[1] <- effs_depress$eff[1]

# Make the graph with the 95% confidence interval
ggplot(effs_depress, aes(x = time, y = value, group = 1)) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  geom_errorbar(width = .1, aes(ymin = value - se, ymax = value + se), colour = "#C46391") +
  geom_point(shape = 21, size = 3, fill = "#C46391", colour = "#C46391") + 
  geom_line(colour = "#C46391", size = 1) +
  ylim(0.95, 2.5) + 
  scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4,5)) +
  labs(y = "Depressive Symptoms", x = "") +
  theme_classic(base_size = 15)+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 0, r = 15, b = 0, l = 0)),         
        axis.text.x  = element_text(colour = "black", size = 19, 
                                    margin = margin(t = 15, r = 0, b = 0, l = 0)), 
        axis.text.y  = element_text(colour = "black", size = 19), 
        legend.position = "none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


# -------------------
# Actual data plots
# -------------------

# Depressive symptoms
# --------------------

sum_dep <- summarySEwithin(data_long, 
                           measurevar = "depress", withinvars = "time",
                           idvar = "idauniq", na.rm = T, conf.interval = .95)

# Make the graph with the 95% confidence interval
ggplot(sum_dep, aes(x = time, y = depress, group = 1)) +
  geom_errorbar(width = .1, aes(ymin = depress - ci, ymax = depress + ci)) +
  geom_point(shape = 21, size = 3, fill = "white") + 
  geom_line() +
  ylim(1, 3) + 
  labs(y = "Depressive Symptoms", x = "")


# Life satisfaction
# -------------------

sum_swl <- summarySEwithin(data_long, 
                           measurevar = "lifesat", withinvars = "time",
                           idvar = "idauniq", na.rm = T, conf.interval = .95)

# Make the graph with the 95% confidence interval
ggplot(sum_swl, aes(x = time, y = lifesat, group = 1)) +
  geom_errorbar(width = .1, aes(ymin = lifesat - ci, ymax = lifesat + ci)) +
  geom_point(shape = 21, size = 3, fill = "white") + 
  geom_line() +
  #ylim(1, 3) + 
  labs(y = "Life satisfaction", x = "")


# Loneliness
# ------------

sum_lon <- summarySEwithin(data_long, 
                           measurevar = "loneli", withinvars = "time",
                           idvar = "idauniq", na.rm = T, conf.interval = .95)

# Make the graph with the 95% confidence interval
ggplot(sum_lon, aes(x = time, y = loneli, group = 1)) +
  geom_errorbar(width = .1, aes(ymin = loneli- ci, ymax = loneli + ci)) +
  geom_point(shape = 21, size = 3, fill = "white") + 
  geom_line() +
  #ylim(1, 3) + 
  labs(y = "Loneliness", x = "")





















#################################
### Personen mit Schlaganfall ###
#################################


table(data$w3_hediast)
data_st <- subset(data, w3_hediast == 1)

#### 1. Depressive Symptome ######

# select relevant variables
data_st_dep <- data_st[, c("idauniq", 
                           "w2_dep_sum", "w3_dep_sum", "w4_dep_sum", "w5_dep_sum",
                           "w6_dep_sum", "w7_dep_sum")]

# creat long formats 
data_st_dep_long <- melt(data_st_dep, id.vars=c("idauniq"))

sum_st_dep <- summarySEwithin(data_st_dep_long, 
                              measurevar="value", withinvars="variable",
                              idvar="idauniq", na.rm=T, conf.interval=.95)

# Make the graph with the 95% confidence interval
ggplot(sum_st_dep, aes(x=variable, y=value)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci)) +
  geom_point(shape=21, size=3, fill="white") + 
  ylim(1,3) + 
  labs(y = "Depressive Symptoms", x = "", title = "New Stroke at W3")

#### 2. Lebenszufriedenheit #####

data_st_swl <- data_st[, c("idauniq", "w2_swl_mean", "w3_swl_mean", "w4_swl_mean", "w5_swl_mean",
                           "w6_swl_mean", "w7_swl_mean")]

data_st_swl_long <- melt(data_st_swl, id.vars=c("idauniq"))

sum_st_swl <- summarySEwithin(data_st_swl_long, 
                              measurevar="value", withinvars="variable",
                              idvar="idauniq", na.rm=T, conf.interval=.95)

# Make the graph with the 95% confidence interval
ggplot(sum_st_swl, aes(x = variable, y = value)) +
  #geom_vline(xintercept = which(sum_st_swl$variable == 'w3_swl_mean'), color = "red", size = 1.5) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci)) +
  geom_point(shape=21, size=3, fill="white") + 
  ylim(4,6) + 
  labs(y = "Life Satisfaction", x = "", title = "New Stroke at W3")

