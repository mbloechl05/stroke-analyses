# ====================
#  Symptom analyses
# ====================

# !!! First parts of script 4_long_analysis.R have to be run before 
# running this script!!!

# Source helper functions and packages
source("analyses/helpers_describe.R")
# source("analyses/00_prepare.R")

# -----------------------------
# 1.) Symptom-based analyses
# -----------------------------

# For general purposes, make a list of relevant depressive symptoms
syms <- c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")


# Time -3
# -----------

# Create list of dataframes that only have time = -3 data
slist_m3 <- lapply(finallist, subset, time == -3 & 
                           (stroke.x == 1 | stroke.x == 0))

# How many people have available and missing symptom data at this time point?
describeBy(slist_m3[[1]]$dep, slist_m3[[1]]$stroke.x)
describeBy(slist_m3[[1]]$sle, slist_m3[[1]]$stroke.x)
describeBy(slist_m3[[1]]$hap, slist_m3[[1]]$stroke.x)
describeBy(slist_m3[[1]]$lon, slist_m3[[1]]$stroke.x)
describeBy(slist_m3[[1]]$enj, slist_m3[[1]]$stroke.x)
describeBy(slist_m3[[1]]$sad, slist_m3[[1]]$stroke.x)
describeBy(slist_m3[[1]]$eff, slist_m3[[1]]$stroke.x)
describeBy(slist_m3[[1]]$goi, slist_m3[[1]]$stroke.x)

# Pooled comparison stats single symptoms
dep_m3 <- sym_stat(slist_m3, "dep") # depressed mood
sle_m3 <- sym_stat(slist_m3, "sle") # sleep
hap_m3 <- sym_stat(slist_m3, "hap") # happy
lon_m3 <- sym_stat(slist_m3, "lon") # lonely
enj_m3 <- sym_stat(slist_m3, "enj") # enjoy
sad_m3 <- sym_stat(slist_m3, "sad") # sad
eff_m3 <- sym_stat(slist_m3, "eff") # effort
goi_m3 <- sym_stat(slist_m3, "goi") # going

# extract p-values by hand and calculate adjusted p-values (fdr / benjamini-hochberg)
p_m3 <- c(0.055595, 0.51663, 0.40366, 0.77166, 0.13694, 0.82035, 0.02279, 0.5918)
p.adjust(p_m3, method = "BH")


# Time -1
# -----------

# Create list of dataframes that only have time = -1 data
slist_m1 <- lapply(finallist, subset, time == -1 & 
                           (stroke.x == 1 | stroke.x == 0))

# How many people have available and missing symptom data at this time point?
describeBy(slist_m1[[1]]$dep, slist_m1[[1]]$stroke.x)
describeBy(slist_m1[[1]]$sle, slist_m1[[1]]$stroke.x)
describeBy(slist_m1[[1]]$hap, slist_m1[[1]]$stroke.x)
describeBy(slist_m1[[1]]$lon, slist_m1[[1]]$stroke.x)
describeBy(slist_m1[[1]]$enj, slist_m1[[1]]$stroke.x)
describeBy(slist_m1[[1]]$sad, slist_m1[[1]]$stroke.x)
describeBy(slist_m1[[1]]$eff, slist_m1[[1]]$stroke.x)
describeBy(slist_m1[[1]]$goi, slist_m1[[1]]$stroke.x)

# Pooled comparison stats single symptoms
dep_m1 <- sym_stat(slist_m1, "dep") # depressed mood
sle_m1 <- sym_stat(slist_m1, "sle") # sleep
hap_m1 <- sym_stat(slist_m1, "hap") # happy
lon_m1 <- sym_stat(slist_m1, "lon") # lonely
enj_m1 <- sym_stat(slist_m1, "enj") # enjoy
sad_m1 <- sym_stat(slist_m1, "sad") # sad
eff_m1 <- sym_stat(slist_m1, "eff") # effort
goi_m1 <- sym_stat(slist_m1, "goi") # going

# extract p-values by hand and calculate adjusted p-values (fdr / benjamini-hochberg)
p_m1 <- c(0.03932, 0.28902, 0.1427, 0.77852, 0.27295, 0.77027, 0.01676, 0.01604)
p.adjust(p_m1, method = "BH")


# Time 0
# -----------

# Create list of dataframes that only have time = 0 data
slist_0 <- lapply(finallist, subset, time == 0 & 
                          (stroke.x == 1 | stroke.x == 0))

# How many people have available and missing symptom data at this time point?
describeBy(slist_0[[1]]$dep, slist_0[[1]]$stroke.x)
describeBy(slist_0[[1]]$sle, slist_0[[1]]$stroke.x)
describeBy(slist_0[[1]]$hap, slist_0[[1]]$stroke.x)
describeBy(slist_0[[1]]$lon, slist_0[[1]]$stroke.x)
describeBy(slist_0[[1]]$enj, slist_0[[1]]$stroke.x)
describeBy(slist_0[[1]]$sad, slist_0[[1]]$stroke.x)
describeBy(slist_0[[1]]$eff, slist_0[[1]]$stroke.x)
describeBy(slist_0[[1]]$goi, slist_0[[1]]$stroke.x)

# Pooled comparison stats single symptoms
dep_0 <- sym_stat(slist_0, "dep") # depressed mood
sle_0 <- sym_stat(slist_0, "sle") # sleep
hap_0 <- sym_stat(slist_0, "hap") # happy
lon_0 <- sym_stat(slist_0, "lon") # lonely
enj_0 <- sym_stat(slist_0, "enj") # enjoy
sad_0 <- sym_stat(slist_0, "sad") # sad
eff_0 <- sym_stat(slist_0, "eff") # effort
goi_0 <- sym_stat(slist_0, "goi") # going

# extract p-values by hand and calculate adjusted p-values (fdr / benjamini-hochberg)
p_0 <- c(0.02289, 0.0268, 0.00262, 0.11102, 0.00014, 0.88187, 4.057535e-06, 9.827510e-07)
format(p.adjust(p_0, method = "BH"), scientific = F)


# Time 3
# -----------

# Create list of dataframes that only have time = 3 data
slist_p3 <- lapply(finallist, subset, time == 3 & 
                           (stroke.x == 1 | stroke.x == 0))

# How many people have available and missing symptom data at this time point?
describeBy(slist_p3[[1]]$dep, slist_p3[[1]]$stroke.x)
describeBy(slist_p3[[1]]$sle, slist_p3[[1]]$stroke.x)
describeBy(slist_p3[[1]]$hap, slist_p3[[1]]$stroke.x)
describeBy(slist_p3[[1]]$lon, slist_p3[[1]]$stroke.x)
describeBy(slist_p3[[1]]$enj, slist_p3[[1]]$stroke.x)
describeBy(slist_p3[[1]]$sad, slist_p3[[1]]$stroke.x)
describeBy(slist_p3[[1]]$eff, slist_p3[[1]]$stroke.x)
describeBy(slist_p3[[1]]$goi, slist_p3[[1]]$stroke.x)

# Pooled comparison stats single symptoms
dep_p3 <- sym_stat(slist_p3, "dep") # depressed mood
sle_p3 <- sym_stat(slist_p3, "sle") # sleep
hap_p3 <- sym_stat(slist_p3, "hap") # happy
lon_p3 <- sym_stat(slist_p3, "lon") # lonely
enj_p3 <- sym_stat(slist_p3, "enj") # enjoy
sad_p3 <- sym_stat(slist_p3, "sad") # sad
eff_p3 <- sym_stat(slist_p3, "eff") # effort
goi_p3 <- sym_stat(slist_p3, "goi") # going

# extract p-values by hand and calculate adjusted p-values (fdr / benjamini-hochberg)
p_p3 <- c(0.06066, 0.13276, 0.36591, 0.54424, 0.12057, 0.21749, 0.00194, 0.00697)
format(p.adjust(p_p3, method = "BH"), scientific = F)


# -----------------------
# 2.) Symptom plots
# ---------------------

marias_theme <- list(
        theme_bw(),
        theme(panel.border     = element_blank(), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.title       = element_text(size = 15), 
              axis.line        = element_line(colour = "black"), 
              axis.text        = element_text(size = 15, colour = "black"), 
              legend.text      = element_text(size = 15), 
              legend.position  = "none"), 
        labs(x = "Frequency" , 
             y = ""),
        scale_y_discrete(limits = c("hap", "enj", "sle", "goi", "eff", "sad", 
                                    "dep", "lon"), 
                         labels = c("happy", "enjoy life", "sleep", "going",
                                    "effort", "sad", "depressed", "lonely")),
        scale_x_continuous(limits = c(0.1,1),  
                           breaks = c(0.2,0.4,0.6,0.8,1.0)),
        scale_colour_manual(values = c("#348D9A",'#573E72'), 
                            name = "", 
                            labels = c("Stroke","Controls")))

# Time -3
# ----------

# combine pooled percentage values to data frame
sym_m3_s <- c(dep_m3[[1]], sle_m3[[1]], hap_m3[[1]], lon_m3[[1]], 
              enj_m3[[1]], sad_m3[[1]], eff_m3[[1]], goi_m3[[1]])
sym_m3_c <- c(dep_m3[[2]], sle_m3[[2]], hap_m3[[2]], lon_m3[[2]], 
              enj_m3[[2]], sad_m3[[2]], eff_m3[[2]], goi_m3[[2]])
sym_m3 <- data.frame(sym_m3_s, sym_m3_c)

# new variable indicating symptoms
sym_m3$symptom <- c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")

# reshape to long format for plotting
sym_m3_long <- reshape(sym_m3,
                       direction = "long", 
                       varying = c("sym_m3_s", "sym_m3_c"),
                       v.names = "score", 
                       timevar = "stroke") # 1 = stroke, 2 = controls
sym_m3_long$stroke <- as.factor(sym_m3_long$stroke)

# create data frame that identifies sign differences
sig_diff_m3 <- subset(sym_m3_long, symptom == "eff")

# Plot data
ggplot(data = sym_m3_long, aes(y = symptom, x = score)) + 
        geom_line(aes(group = symptom), 
                  color = "grey90", alpha = 0.5, size = 2) +
        geom_point(aes(colour = stroke), size = 2, alpha = 0.4) + 
        geom_line(data = sig_diff_m3, aes(group = symptom), 
                  color = "grey90", size = 4) + 
        geom_point(data = sig_diff_m3, aes(colour = stroke), 
                   size = 4) +
        marias_theme
ggsave("figures/figure_2a.pdf", width = 6, height = 4, units = "in")


# Time -1
# ----------

# combine pooled percentage values to data frame
sym_m1_s <- c(dep_m1[[1]], sle_m1[[1]], hap_m1[[1]], lon_m1[[1]], 
              enj_m1[[1]], sad_m1[[1]], eff_m1[[1]], goi_m1[[1]])
sym_m1_c <- c(dep_m1[[2]], sle_m1[[2]], hap_m1[[2]], lon_m1[[2]], 
              enj_m1[[2]], sad_m1[[2]], eff_m1[[2]], goi_m1[[2]])
sym_m1 <- data.frame(sym_m1_s, sym_m1_c)

# new variable indicating symptoms
sym_m1$symptom <- c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")

# reshape to long format for plotting
sym_m1_long <- reshape(sym_m1,
                       direction = "long", 
                       varying = c("sym_m1_s", "sym_m1_c"),
                       v.names = "score", 
                       timevar = "stroke") # 1 = stroke, 2 = controls
sym_m1_long$stroke <- as.factor(sym_m1_long$stroke)

# create data frame that identifies sign differences
sig_diff_m1 <- subset(sym_m1_long, 
                      symptom == "dep"| symptom == "eff"| symptom == "goi")
                              
# Plot data
ggplot(data = sym_m1_long, aes(y = symptom, x = score)) + 
        geom_line(aes(group = symptom), 
                  color = "grey90", alpha = 0.5, size = 2) +
        geom_point(aes(colour = stroke), size = 2, alpha = 0.4) + 
        geom_line(data = sig_diff_m1, aes(group = symptom), 
                  color = "grey90", size = 4) + 
        geom_point(data = sig_diff_m1, aes(colour = stroke), 
                   size = 4) +
        marias_theme
ggsave("figures/figure_2b.pdf", width = 6, height = 4, units = "in")


# Time 0
# ----------

# combine pooled percentage values to data frame
sym_0_s <- c(dep_0[[1]], sle_0[[1]], hap_0[[1]], lon_0[[1]], 
             enj_0[[1]], sad_0[[1]], eff_0[[1]], goi_0[[1]])
sym_0_c <- c(dep_0[[2]], sle_0[[2]], hap_0[[2]], lon_0[[2]], 
             enj_0[[2]], sad_0[[2]], eff_0[[2]], goi_0[[2]])
sym_0 <- data.frame(sym_0_s, sym_0_c)

# new variable indicating symptoms
sym_0$symptom <- c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")

# reshape to long format for plotting
sym_0_long <- reshape(sym_0,
                      direction = "long", 
                      varying = c("sym_0_s", "sym_0_c"),
                      v.names = "score", 
                      timevar = "stroke") # 1 = stroke, 2 = controls
sym_0_long$stroke <- as.factor(sym_0_long$stroke)

# create data frame that identifies revenue differences over 20%
sig_diff_0 <- subset(sym_0_long,
                      symptom == "dep"| symptom == "sle"| symptom == "hap" |
                             symptom == "enj" | symptom == "eff" | 
                             symptom == "goi")

# Plot data
ggplot(data = sym_0_long, aes(y = symptom, x = score)) + 
        geom_line(aes(group = symptom), 
                  color = "grey90", alpha = 0.5, size = 2) +
        geom_point(aes(colour = stroke), size = 2, alpha = 0.4) + 
        geom_line(data = sig_diff_0, aes(group = symptom), 
                  color = "grey90", size = 4) + 
        geom_point(data = sig_diff_0, aes(colour = stroke), 
                   size = 4) +
        marias_theme
ggsave("figures/figure_2c.pdf", width = 6, height = 4, units = "in")


# Time +3
# ----------

# combine pooled percentage values to data frame
sym_p3_s <- c(dep_p3[[1]], sle_p3[[1]], hap_p3[[1]], lon_p3[[1]], 
              enj_p3[[1]], sad_p3[[1]], eff_p3[[1]], goi_p3[[1]])
sym_p3_c <- c(dep_p3[[2]], sle_p3[[2]], hap_p3[[2]], lon_p3[[2]], 
              enj_p3[[2]], sad_p3[[2]], eff_p3[[2]], goi_p3[[2]])
sym_p3 <- data.frame(sym_p3_s, sym_p3_c)

# new variable indicating symptoms
sym_p3$symptom <- c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")

# reshape to long format for plotting
sym_p3_long <- reshape(sym_p3,
                       direction = "long", 
                       varying = c("sym_p3_s", "sym_p3_c"),
                       v.names = "score", 
                       timevar = "stroke") # 1 = stroke, 2 = controls
sym_p3_long$stroke <- as.factor(sym_p3_long$stroke)

# create data frame that identifies revenue differences over 20%
sig_diff_p3 <- subset(sym_p3_long, symptom == "eff"| symptom == "goi")

# Plot data
ggplot(data = sym_p3_long, aes(y = symptom, x = score)) + 
        geom_line(aes(group = symptom), 
                  color = "grey90", alpha = 0.5, size = 2) +
        geom_point(aes(colour = stroke), size = 2, alpha = 0.4) + 
        geom_line(data = sig_diff_p3, aes(group = symptom), 
                  color = "grey90", size = 4) + 
        geom_point(data = sig_diff_p3, aes(colour = stroke), 
                   size = 4) +
        marias_theme
ggsave("figures/figure_2d.pdf", width = 6, height = 4, units = "in")



