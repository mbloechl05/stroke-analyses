# Model Infurna et al. 
# ---------------------

# load packages
library(ggplot2)
library(dplyr)
library(Rmisc) # for summarySEwithin()
library(reshape)
library(lme4)
library(lmerTest)
library(car) # for recode()
library(arm) # for se.fixef()

# load data
setwd("/Users/maria/Documents/learn/0_PhD/Projects/stroke_wellbeing")
load("data/processed/proc_data.RData")
load("data/processed/matched_data.RData")

# ---------------------------------
# Depressive symptoms: Main model 
# ---------------------------------

data_long <- subset(data_long, n_strokes.x == 1)

data_long <- data_long %>% # slope before stroke
  mutate(slope_1 = dplyr::recode(time, 
                                 `-4`= 0,
                                 `-3`= 1, 
                                 `-2`= 2, 
                                 `-1`= 3, 
                                 `0`= 3,
                                 `1`= 3, 
                                 `2`= 3, 
                                 `3`= 3, 
                                 `4`= 3, 
                                 `5`= 3))

data_long <- data_long %>% # jump intercept at stroke
  mutate(jump = dplyr::recode(time, 
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

data_long <- data_long %>% # slope after stroke
  mutate(slope_2 = dplyr::recode(time, 
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


# Define random-intercept-only model (reference = stroke 0)
m_depress <- depress ~ 1 + slope_1 + jump + slope_2 + (1 + slope_1 + jump + slope_2 | idauniq)

# Fit model
fit_depress <- lmer(m_depress, data = data_long, REML = TRUE)

# Show model results
summary(fit_depress)

e1 <- allEffects(fit_depress_r2) # plot model
plot(e1)


# try different optimizers
# ----------------------------------

# model 1
fit_depress_1 <- lmer(m_depress, data = data_long, control = lmerControl(optimizer = "bobyqa"))
summary(fit_depress_1)


# ================= MODEL 1 ================================

# model 2
fit_depress_2 <- lmer(m_depress, data = data_long, control = lmerControl(optimizer = "Nelder_Mead"))
summary(fit_depress_2)

# plot model
e1 <- allEffects(fit_depress_2) 
plot(e1)

# extract AIC
extractAIC(fit_depress_2)

# =========================================================


# model 3
fit_depress_3 <- lmer(m_depress, data = data_long, control = lmerControl(optimizer = "nlminbwrap"))
summary(fit_depress_3)

# model 4 (default)
fit_depress_4 <- lmer(m_depress, data = data_long, control = lmerControl(optimizer = "nloptwrap"))
summary(fit_depress_4)

## --> all except the default (model 4) indicate singularity!
?isSingular


# Check singularity 
# ---------------------

# model 1
tt <- getME(fit_depress_1, "theta") # extract random-effects parameter estimates: these are 
# parameterized as the relative Cholesky factors of each random effect term --> not sure what these terms represent
ll <- getME(fit_depress_1, "lower") # extract lower bounds on model parameters (random effects parameters only
min(tt[ll == 0]) # tests minimum tt for which ll = 0
tt

# model 2
tt <- getME(fit_depress_2, "theta") 
ll <- getME(fit_depress_2, "lower") 
min(tt[ll == 0]) 
tt

# model 3
tt <- getME(fit_depress_3, "theta") 
ll <- getME(fit_depress_3, "lower") 
min(tt[ll == 0]) 
tt

# model 4
tt <- getME(fit_depress_4, "theta") 
ll <- getME(fit_depress_4, "lower") 
min(tt[ll == 0]) 
tt


# ------------------------------------
# Depressive symptoms: Revised model
# -------------------------------------

# removed random effect of slope 1 since it is theoretically least interesting, had little variance and issues of 
# singularity in one optimizer

# Define model (reference = stroke 0)
m_depress_r <- depress ~ 1 + slope_1 + jump + slope_2 + (1 + jump + slope_2 | idauniq)


# try different optimizers
# ----------------------------------

# model 1
fit_depress_r1 <- lmer(m_depress_r, data = data_long, control = lmerControl(optimizer = "bobyqa"))
summary(fit_depress_r1)

# ==================== MODEL 2 ============================

# model 2
fit_depress_r2 <- lmer(m_depress_r, data = data_long, control = lmerControl(optimizer = "Nelder_Mead"))
summary(fit_depress_r2)

# plot model
e1 <- allEffects(fit_depress_r2) 
plot(e1)

# extract AIC
extractAIC(fit_depress_r2)

# =========================================================

# model 3
fit_depress_r3 <- lmer(m_depress_r, data = data_long, control = lmerControl(optimizer = "nlminbwrap"))
summary(fit_depress_r3)

# model 4 (default)
fit_depress_r4 <- lmer(m_depress_r, data = data_long, control = lmerControl(optimizer = "nloptwrap"))
summary(fit_depress_r4)



# Different design matrix
# ---------------------------

data_long <- data_long %>% # slope before stroke
  mutate(slope_1 = dplyr::recode(time, 
                                 `-4`= 0,
                                 `-3`= 1, 
                                 `-2`= 2, 
                                 `-1`= 3, 
                                 `0`= 3,
                                 `1`= 3, 
                                 `2`= 3, 
                                 `3`= 3, 
                                 `4`= 3, 
                                 `5`= 3))

data_long <- data_long %>% # jump intercept at stroke
  mutate(jump = dplyr::recode(time, 
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

data_long <- data_long %>% # slope after stroke
  mutate(slope_2 = dplyr::recode(time, 
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

m_depress_r <- depress ~ 1 + slope_1 + jump + slope_2 + (1 + jump + slope_2 | idauniq)

fit_depress_r2 <- lmer(m_depress_r, data = data_long, control = lmerControl(optimizer = "Nelder_Mead"))
summary(fit_depress_r2)

e1 <- allEffects(fit_depress_r2) # plot model
plot(e1)


# nlme
# ---------------------------


# ----------------------------
# Life satisfaction: Models
# ----------------------------

# Define model (reference = stroke 0)
m_lifesat <- lifesat ~ 1 + slope_1 + jump + slope_2 + (1 + jump + slope_2 | idauniq)

# model 1
fit_lifesat_r1 <- lmer(m_lifesat, data = data_long, control = lmerControl(optimizer = "bobyqa"))
summary(fit_lifesat_r1)

# model 2
fit_lifesat_r2 <- lmer(m_lifesat, data = data_long, control = lmerControl(optimizer = "Nelder_Mead"))
summary(fit_lifesat_r2)

# model 3
fit_lifesat_r3 <- lmer(m_lifesat, data = data_long, control = lmerControl(optimizer = "nlminbwrap"))
summary(fit_lifesat_r3)

# model 4 (default)
fit_lifesat_r4 <- lmer(m_lifesat, data = data_long, control = lmerControl(optimizer = "nloptwrap"))
summary(fit_lifesat_r4)



