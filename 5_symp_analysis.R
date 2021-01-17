# ====================
#  Symptom analyses
# ====================

# Source helper functions
source("analyses/helpers_describe.R")


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

# Pooled comparison stats single symptoms
dep_m3 <- sym_stat(slist_m3, "dep") # depressed mood
sle_m3 <- sym_stat(slist_m3, "sle") # sleep
hap_m3 <- sym_stat(slist_m3, "hap") # happy
lon_m3 <- sym_stat(slist_m3, "lon") # lonely
enj_m3 <- sym_stat(slist_m3, "enj") # enjoy
sad_m3 <- sym_stat(slist_m3, "sad") # sad
eff_m3 <- sym_stat(slist_m3, "eff") # effort
goi_m3 <- sym_stat(slist_m3, "goi") # going


# Time -1
# -----------

# Create list of dataframes that only have time = -1 data
slist_m1 <- lapply(finallist, subset, time == -1 & 
                           (stroke.x == 1 | stroke.x == 0))

# Pooled comparison stats single symptoms
dep_m1 <- sym_stat(slist_m1, "dep") # depressed mood
sle_m1 <- sym_stat(slist_m1, "sle") # sleep
hap_m1 <- sym_stat(slist_m1, "hap") # happy
lon_m1 <- sym_stat(slist_m1, "lon") # lonely
enj_m1 <- sym_stat(slist_m1, "enj") # enjoy
sad_m1 <- sym_stat(slist_m1, "sad") # sad
eff_m1 <- sym_stat(slist_m1, "eff") # effort
goi_m1 <- sym_stat(slist_m1, "goi") # going


# Time 0
# -----------

# Create list of dataframes that only have time = 0 data
slist_0 <- lapply(finallist, subset, time == 0 & 
                          (stroke.x == 1 | stroke.x == 0))

# Pooled comparison stats single symptoms
dep_0 <- sym_stat(slist_0, "dep") # depressed mood
sle_0 <- sym_stat(slist_0, "sle") # sleep
hap_0 <- sym_stat(slist_0, "hap") # happy
lon_0 <- sym_stat(slist_0, "lon") # lonely
enj_0 <- sym_stat(slist_0, "enj") # enjoy
sad_0 <- sym_stat(slist_0, "sad") # sad
eff_0 <- sym_stat(slist_0, "eff") # effort
goi_0 <- sym_stat(slist_0, "goi") # going

# Time 3
# -----------

# Create list of dataframes that only have time = 3 data
slist_p3 <- lapply(finallist, subset, time == 3 & 
                           (stroke.x == 1 | stroke.x == 0))

# Pooled comparison stats single symptoms
dep_p3 <- sym_stat(slist_p3, "dep") # depressed mood
sle_p3 <- sym_stat(slist_p3, "sle") # sleep
hap_p3 <- sym_stat(slist_p3, "hap") # happy
lon_p3 <- sym_stat(slist_p3, "lon") # lonely
enj_p3 <- sym_stat(slist_p3, "enj") # enjoy
sad_p3 <- sym_stat(slist_p3, "sad") # sad
eff_p3 <- sym_stat(slist_p3, "eff") # effort
goi_p3 <- sym_stat(slist_p3, "goi") # going


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

# Plot data
ggplot(data = sym_m3_long, aes(y = symptom, x = score)) + 
        geom_line(aes(group = symptom), 
                  color = "grey90", alpha = 0.5, size = 2) +
        geom_point(aes(colour = stroke), size = 2, alpha = 0.6) + 
        marias_theme

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

# create data frame that identifies revenue differences over 20%
sig_diff_m1 <- subset(sym_m1_long, 
                      symptom == "dep"| symptom == "eff"| symptom == "goi")
                              
# Plot data
ggplot(data = sym_m1_long, aes(y = symptom, x = score)) + 
        geom_line(aes(group = symptom), 
                  color = "grey90", alpha = 0.5, size = 2) +
        geom_point(aes(colour = stroke), size = 2, alpha = 0.6) + 
        geom_line(data = sig_diff_m1, aes(group = symptom), 
                  color = "grey90", size = 4) + 
        geom_point(data = sig_diff_m1, aes(colour = stroke), 
                   size = 4) +
        marias_theme

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
        geom_point(aes(colour = stroke), size = 2, alpha = 0.6) + 
        geom_line(data = sig_diff_0, aes(group = symptom), 
                  color = "grey90", size = 4) + 
        geom_point(data = sig_diff_0, aes(colour = stroke), 
                   size = 4) +
        marias_theme

# # Time +1
# # ----------
# 
# # combine pooled percentage values to data frame
# sym_p1_s <- c(dep_p1[[1]], sle_p1[[1]], hap_p1[[1]], lon_p1[[1]], 
#               enj_p1[[1]], sad_p1[[1]], eff_p1[[1]], goi_p1[[1]])
# sym_p1_c <- c(dep_p1[[2]], sle_p1[[2]], hap_p1[[2]], lon_p1[[2]], 
#               enj_p1[[2]], sad_p1[[2]], eff_p1[[2]], goi_p1[[2]])
# sym_p1 <- data.frame(sym_p1_s, sym_p1_c)
# 
# # new variable indicating symptoms
# sym_p1$symptom <- c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")
# 
# # reshape to long format for plotting
# sym_p1_long <- reshape(sym_p1,
#                        direction = "long", 
#                        varying = c("sym_p1_s", "sym_p1_c"),
#                        v.names = "score", 
#                        timevar = "stroke") # 1 = stroke, 2 = controls
# sym_p1_long$stroke <- as.factor(sym_p1_long$stroke)
# 
# # Plot data
# ggplot(data = sym_p1_long, aes(y = symptom, x = score)) + 
#         geom_line(aes(group = symptom)) +
#         geom_point(aes(colour = stroke)) + 
#         marias_theme

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
sig_diff_p3 <- subset(sym_p3_long, 
                      symptom == "dep"| symptom == "eff"| symptom == "goi")

# Plot data
ggplot(data = sym_p3_long, aes(y = symptom, x = score)) + 
        geom_line(aes(group = symptom), 
                  color = "grey90", alpha = 0.5, size = 2) +
        geom_point(aes(colour = stroke), size = 2, alpha = 0.6) + 
        geom_line(data = sig_diff_p3, aes(group = symptom), 
                  color = "grey90", size = 4) + 
        geom_point(data = sig_diff_p3, aes(colour = stroke), 
                   size = 4) +
        marias_theme


# -----------
# Networks
# -----------

# Time -3
# ----------

# create appropriate mitml list
corr_list_m3 <- lapply(finallist_model, subset, time == -3)

# calculate correlations
corr_data_m3 <- 
        micombine.cor(corr_list_m3, 
                      variables = c("stroke.y", "adl", "mem", "dep", "sle", 
                                    "hap", "lon", "enj", "sad", "eff", "goi"), 
                      conf.level = 0.95,
                      method = "spearman")

corr_mat_m3 <- attr(corr_data_m3, "r_matrix")

# Time -1
# ----------

# create appropriate mitml list
corr_list_m1 <- lapply(finallist_model, subset, time == -1)

# calculate correlations
corr_data_m1 <- 
        micombine.cor(corr_list_m1, 
                      variables = c("stroke.y", "adl", "mem", "dep", "sle", 
                                    "hap", "lon", "enj", "sad", "eff", "goi"), 
                      conf.level = 0.95,
                      method = "spearman")

corr_mat_m1 <- attr(corr_data_m1, "r_matrix")


# Time 0
# ----------

# create appropriate mitml list
corr_list_0 <- lapply(finallist_model, subset, time == 0)

# calculate correlations
corr_data_0 <- 
        micombine.cor(corr_list_0, 
                      variables = c("stroke.y", "adl", "mem", "dep", "sle", 
                                    "hap", "lon", "enj", "sad", "eff", "goi"), 
                      conf.level = 0.95,
                      method = "spearman")

corr_mat_0 <- attr(corr_data_0, "r_matrix")


# Time +3
# ----------

# create appropriate mitml list
corr_list_p3 <- lapply(finallist_model, subset, time == 3)

# calculate correlations
corr_data_p3 <- 
        micombine.cor(corr_list_p3, 
                      variables = c("stroke.y", "adl", "mem", "dep", "sle", 
                                    "hap", "lon", "enj", "sad", "eff", "goi"), 
                      conf.level = 0.95,
                      method = "spearman")

corr_mat_p3 <- attr(corr_data_p3, "r_matrix")


# Plot networks
# ----------------

# get average layout
qgraph_m3 <- qgraph(corr_mat_m3)
qgraph_m1 <- qgraph(corr_mat_m1)
qgraph_0  <- qgraph(corr_mat_0)
qgraph_p3 <- qgraph(corr_mat_p3)

# create average layout
netlayout <- averageLayout(qgraph_m3, qgraph_m1, qgraph_0, qgraph_p3)

# groups
stroke <- c(1)
depressive_sym <- c(4:11)
other <- c(2:3)
groups <- list(stroke, depressive_sym, other)

# plot networks
qgraph(corr_mat_m3, 
       layout = netlayout, 
       threshold = 0.05, 
       negDashed = F,
       groups = groups,
       labels = c("Stroke", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
       color = c("white", "#92c2cc", "#303435"), 
       border.color = c("white", "white", "white", "white", "white", "white", 
                        "white", "white", "white", "white", "white"),
       border.width = c(5,2,2,2,2,2,2,2,2,2,2),
       label.color = c("black", "white", "white", "black", "black", "black",
                       "black", "black", "black", "black", "black"),
       posCol = "grey60", negCol = "grey60", fade = F, esize = 5,
       vsize = c(7, 5,5,5,5,5,5,5,5,5,5),    
       maximum = 1, label.prop = 1.3)

qgraph(corr_mat_m1, 
       layout = netlayout, 
       threshold = 0.05, 
       negDashed = F,
       groups = groups,
       labels = c("Stroke", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
       color = c("white", "#92c2cc", "#303435"), 
       border.color = c("white", "white", "white", "white", "white", "white", 
                        "white", "white", "white", "white", "white"),
       border.width = c(5,2,2,2,2,2,2,2,2,2,2),
       label.color = c("black", "white", "white", "black", "black", "black",
                       "black", "black", "black", "black", "black"),
       posCol = "grey60", negCol = "grey60", fade = F, esize = 5,
       vsize = c(7, 5,5,5,5,5,5,5,5,5,5),     
       maximum = 1, label.prop = 1.3)

qgraph(corr_mat_0, 
       layout = netlayout, 
       threshold = 0.05, 
       negDashed = F,
       groups = groups,
       labels = c("Stroke", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
       color = c("white", "#92c2cc", "#303435"), 
       border.color = c("white", "white", "white", "white", "white", "white", 
                        "white", "white", "white", "white", "white"),
       border.width = c(5,2,2,2,2,2,2,2,2,2,2),
       label.color = c("black", "white", "white", "black", "black", "black",
                       "black", "black", "black", "black", "black"),
       posCol = "grey60", negCol = "grey60", fade = F, esize = 5,
       vsize = c(7, 5,5,5,5,5,5,5,5,5,5),     
       maximum = 1, label.prop = 1.3)

qgraph(corr_mat_p3, 
       layout = netlayout, 
       threshold = 0.05, 
       negDashed = F,
       groups = groups,
       labels = c("Stroke", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
       color = c("white", "#92c2cc", "#303435"), 
       border.color = c("white", "white", "white", "white", "white", "white", 
                        "white", "white", "white", "white", "white"),
       border.width = c(5,2,2,2,2,2,2,2,2,2,2),
       label.color = c("black", "white", "white", "black", "black", "black",
                       "black", "black", "black", "black", "black"),
       posCol = "grey60", negCol = "grey60", fade = F, esize = 5,
       vsize = c(6, 4,4,4,4,4,4,4,4,4,4),    
       maximum = 1, label.prop = 1.3)





# Now have to calculate

# with single symptom data, time -2
# --------------------------------------------

data_minus2_s <- subset(data_final_long9, stroke.x == 1 & time == -1)
data_minus2_s <- data_minus2_s[,c("adl", "mem",
                                  "dep", "sle", "hap",
                                  "lon", "enj",
                                  "sad", "eff", "goi",
                                  "stroke.x")]
data_minus2_c <- subset(data_final_long9, stroke.x == 0 & time == -1)
data_minus2_c <- data_minus2_c[,c("adl", "mem",
                                  "dep", "sle", "hap",
                                  "lon", "enj",
                                  "sad", "eff", "goi",
                                  "stroke.x")]

data_minus2 <- rbind(data_minus2_s, data_minus2_c)

data_minus2$stroke.x <- as.numeric(data_minus2$stroke.x)
data_minus2$stroke.x <- data_minus2$stroke.x - 1

net_minus2 <- estimateNetwork(data_minus2,
                              default = "cor")


# with single symptom data, time 0
# --------------------------------------------

data_zero_s <- subset(data_final_long8, stroke.x == 1 & time == 0)
data_zero_s <- data_zero_s[,c("adl", "mem",
                              "dep", "sle", "hap",
                              "lon", "enj",
                              "sad", "eff", "goi",
                              "stroke.x")]
data_zero_c <- subset(data_final_long8, stroke.x == 0 & time == 0)
data_zero_c <- data_zero_c[,c("adl", "mem",
                              "dep", "sle", "hap",
                              "lon", "enj",
                              "sad", "eff", "goi",
                              "stroke.x")]

data_zero <- rbind(data_zero_s, data_zero_c)

data_zero$stroke.x <- as.numeric(data_zero$stroke.x)
data_zero$stroke.x <- data_zero$stroke.x - 1

net_zero <- estimateNetwork(data_zero,
                            default = "cor")

# with single symptom data, time +2
# --------------------------------------------

data_plus2_s <- subset(data_final_long8, stroke.x == 1 & time == 3)
data_plus2_s <- data_plus2_s[,c("adl", "mem",
                                  "dep", "sle", "hap",
                                  "lon", "enj",
                                  "sad", "eff", "goi",
                                  "stroke.x")]
data_plus2_c <- subset(data_final_long8, stroke.x == 0 & time == 3)
data_plus2_c <- data_plus2_c[,c("adl", "mem",
                                  "dep", "sle", "hap",
                                  "lon", "enj",
                                  "sad", "eff", "goi",
                                  "stroke.x")]

data_plus2 <- rbind(data_plus2_s, data_plus2_c)

data_plus2$stroke.x <- as.numeric(data_plus2$stroke.x)
data_plus2$stroke.x <- data_plus2$stroke.x - 1

net_plus2 <- estimateNetwork(data_plus2,
                             default = "cor")


# Network plots
# ------------------

net_layout <- averageLayout(net_zero, net_plus2, net_minus2)

plot(net_minus2,
     layout = net_layout,
     threshold = 0.05,
     negDashed = F,
     posCol = "grey60", negCol = "grey60", fade = F, esize = 5,
     vsize = 10, border.color = "white", color = "plum", maximum = 1)

plot(net_zero,
     layout = net_layout,
     threshold = 0.05,
     negDashed = F,
     posCol = "grey60", negCol = "grey60", fade = F, esize = 5,
     vsize = 10, border.color = "white", color = "plum", maximum = 1)

plot(net_plus2,
     layout = net_layout,
     threshold = 0.05,
     negDashed = F,
     posCol = "grey60", negCol = "grey60", fade = F, esize = 5,
     vsize = 10, border.color = "white", color = "plum", maximum = 1)

# 
# # ---------------------------
# # Single symptom analyses
# # ---------------------------
# 
# 
# describeBy(data_minus2, data_minus2$stroke.x)
# 
# describeBy(data_zero, data_zero$stroke.x)
# 
# describeBy(data_plus2, data_plus2$stroke.x)
# 
# 
# # Time point minus two
# # ---------------------------
# 
# # reshape to long format for plotting
# data_minus2_long <- reshape(data_minus2[3:11],
#                           direction = "long", 
#                           varying = list(names(data_minus2)[3:10]),
#                           v.names = "score", 
#                           timevar = "sympt")
# head(data_minus2_long)
# 
# # need descriptive stats for plotting--> sacve those in object "stats"
# stats = describeBy(data_minus2_long$score, 
#                    group = list(data_minus2_long$stroke.x, data_minus2_long$sympt), 
#                    mat = TRUE, 
#                    skew = FALSE, 
#                    ranges = FALSE) 
# 
# # rename factorsin stats for plotting again
# names(stats)[names(stats) == "group1"] <- "stroke" 
# names(stats)[names(stats) == "group2"] <- "symptom" 
# 
# # # re-order levels in stats to have desired ordering in plot
# # data_exp_long$Alter_Gr   <- factor(data_exp_long$Alter_Gr, 
# #                                    levels = c("Juenger", "Aelter"))
# 
# ggplot(data = stats, aes(y = mean, x = symptom, fill = stroke)) + 
#         geom_bar(position = position_dodge(),
#                  stat = "identity", 
#                  #colour = "black", 
#                  size = .3, 
#                  show.legend = T) + 
#         theme_bw() +
#         theme(panel.border     = element_blank(), 
#               panel.grid.major = element_blank(), 
#               panel.grid.minor = element_blank(),
#               axis.title       = element_text(size = 15), 
#               axis.line        = element_line(colour = "black"), 
#               axis.text        = element_text(size = 15, colour = "black"), 
#               legend.text      = element_text(size = 15)) + 
#         labs(x = "" , 
#              y = "Percent") +
#         scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
#                          labels=c("dep", "sle", "hap", "lon", "enj", "sad", 
#                                   "eff", "goi")) +
#         scale_y_continuous(expand = c(0,0), 
#                            limits = c(0,1),  
#                            breaks = c(0.2,0.4,0.6,0.8,1.0)) + 
#         scale_fill_manual(values=c('grey90','#2d4059'), 
#                           name = "", 
#                           labels = c("Controls","Stroke")) 
# 
# 
# # Time point zero 
# # ---------------------------
# 
# # reshape to long format for plotting
# data_zero_long <- reshape(data_zero[3:11],
#                            direction = "long", 
#                            varying = list(names(data_zero)[3:10]),
#                            v.names = "score", 
#                            timevar = "sympt")
# head(data_zero_long)
# 
# # need descriptive stats for plotting--> sacve those in object "stats"
# stats = describeBy(data_zero_long$score, 
#                    group = list(data_zero_long$stroke.x, data_zero_long$sympt), 
#                    mat = TRUE, 
#                    skew = FALSE, 
#                    ranges = FALSE) 
# 
# # rename factorsin stats for plotting again
# names(stats)[names(stats) == "group1"] <- "stroke" 
# names(stats)[names(stats) == "group2"] <- "symptom" 
# 
# # # re-order levels in stats to have desired ordering in plot
# # data_exp_long$Alter_Gr   <- factor(data_exp_long$Alter_Gr, 
# #                                    levels = c("Juenger", "Aelter"))
# 
# ggplot(data = stats, aes(y = mean, x = symptom, fill = stroke)) + 
#         geom_bar(position = position_dodge(),
#                  stat = "identity", 
#                  colour = "black", size = .3, 
#                  show.legend = T) + 
#         theme_bw() +
#         theme(panel.border     = element_blank(), 
#               panel.grid.major = element_blank(), 
#               panel.grid.minor = element_blank(),
#               axis.title       = element_text(size = 15), 
#               axis.line        = element_line(colour = "black"), 
#               axis.text        = element_text(size = 15, colour = "black"), 
#               legend.text      = element_text(size = 15)) + 
#         labs(x = "" , 
#              y = "Percent") +
#         scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
#                          labels=c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")) +
#         scale_y_continuous(expand = c(0,0), 
#                            limits = c(0,1),  
#                            breaks = c(0.2,0.4,0.6,0.8,1.0)) + 
#         scale_fill_manual(values=c('grey90','#2d4059'), 
#                           name = "", 
#                           labels = c("Controls","Stroke")) 
# 
# 
# # Time point plus 2
# # ---------------------------
# 
# # reshape to long format for plotting
# data_plus2_long <- reshape(data_plus2[3:11],
#         direction = "long", 
#         varying = list(names(data_plus2)[3:10]),
#         v.names = "score", 
#         timevar = "sympt")
# head(data_plus2_long)
# 
# # need descriptive stats for plotting--> sacve those in object "stats"
# stats = describeBy(data_plus2_long$score, 
#                    group = list(data_plus2_long$stroke.x, data_plus2_long$sympt), 
#                    mat = TRUE, 
#                    skew = FALSE, 
#                    ranges = FALSE) 
# 
# # rename factorsin stats for plotting again
# names(stats)[names(stats) == "group1"] <- "stroke" 
# names(stats)[names(stats) == "group2"] <- "symptom" 
# 
# # # re-order levels in stats to have desired ordering in plot
# # data_exp_long$Alter_Gr   <- factor(data_exp_long$Alter_Gr, 
# #                                    levels = c("Juenger", "Aelter"))
# 
# ggplot(data = stats, aes(y = mean, x = symptom, fill = stroke)) + 
#         geom_bar(position = position_dodge(),
#                  stat = "identity", 
#                  colour = "black", size = .3, 
#                  show.legend = T) + 
#         theme_bw() +
#         theme(panel.border     = element_blank(), 
#               panel.grid.major = element_blank(), 
#               panel.grid.minor = element_blank(),
#               axis.title       = element_text(size = 15), 
#               axis.line        = element_line(colour = "black"), 
#               axis.text        = element_text(size = 15, colour = "black"), 
#               legend.text      = element_text(size = 15)) + 
#         labs(x = "" , 
#              y = "Percent") +
#         scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
#                          labels=c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")) +
#         scale_y_continuous(expand = c(0,0), 
#                            limits = c(0,1),  
#                            breaks = c(0.2,0.4,0.6,0.8,1.0)) + 
#         scale_fill_manual(values=c('grey90','#2d4059'), 
#                           name = "", 
#                           labels = c("Controls","Stroke")) 
# 
# 
# t.test(data_plus2$dep ~ data_plus2$stroke.x)
# t.test(data_plus2$sle ~ data_plus2$stroke.x)
# t.test(data_plus2$hap ~ data_plus2$stroke.x)
# t.test(data_plus2$lon ~ data_plus2$stroke.x)
# t.test(data_plus2$enj ~ data_plus2$stroke.x, var.equal = T)
# t.test(data_plus2$sad ~ data_plus2$stroke.x)
# t.test(data_plus2$eff ~ data_plus2$stroke.x)
# t.test(data_plus2$goi ~ data_plus2$stroke.x)
# 
# cor.test(data_plus2$dep, data_plus2$stroke.x)
# cor.test(data_plus2$sle, data_plus2$stroke.x)
# cor.test(data_plus2$hap, data_plus2$stroke.x)
# cor.test(data_plus2$lon, data_plus2$stroke.x)
# cor.test(data_plus2$enj, data_plus2$stroke.x)
# cor.test(data_plus2$sad, data_plus2$stroke.x)
# cor.test(data_plus2$eff, data_plus2$stroke.x)
# cor.test(data_plus2$goi, data_plus2$stroke.x)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # -----------
# # Stroke 
# # -----------
# 
# # 1) Pre-stroke
# # ------------------
# 
# data_stroke_minus1 <- subset(data_final_long6, n_strokes.x == 1 & time == -2)
# data_stroke_minus1 <- data_stroke_minus1[,c("adl", "dep", "eff", "sle",
#                                             "hap", "lon", "enj", "sad", "goi")]
# describe(data_stroke_minus1)
# cor(data_stroke_minus1, use = "pairwise.complete.obs")
# 
# net_stroke_minus1 <- estimateNetwork(data_stroke_minus1, default = "mgm") 
# plot(net_stroke_minus1, layout = "circle")
# sum(abs(net_stroke_minus1$graph))/2 #5.36
# 
# # 2) Post-Stroke 1
# # ------------------
# 
# data_stroke_zero <- subset(data_final_long6, n_strokes.x == 1 & time == 0)
# data_stroke_zero <- data_stroke_zero[,c("adl", "dep", "eff", "sle",
#                                         "hap", "lon", "enj", "sad", "goi")]
# describe(data_stroke_zero)
# cormat_stroke_zero <- cor(data_stroke_zero, use = "pairwise.complete.obs")
# 
# net_stroke_zero <- estimateNetwork(data_stroke_zero, default = "mgm") 
# plot(net_stroke_zero, layout = "circle")
# sum(abs(net_stroke_zero$graph))/2 #6.97
# 
# qgraph(cormat_stroke_zero, layout = "circle", minimum = 0.2)
# 
# 
# # 3) Poststroke 2
# # ------------------
# 
# data_stroke_plus1 <- subset(data_final_long6, n_strokes.x == 1 & time == 1)
# data_stroke_plus1 <- data_stroke_plus1[,c("adl", "dep", "eff", "sle", 
#                                           "hap", "lon", "enj", "sad", "goi")]
# describe(data_stroke_plus1)
# cor(data_stroke_plus1, use = "pairwise.complete.obs")
# 
# net_stroke_plus1 <- estimateNetwork(data_stroke_plus1, default = "mgm") 
# plot(net_stroke_plus1, layout = "circle")
# sum(abs(net_stroke_plus1$graph))/2 #6.39
# 
# 
# # -----------
# # Controls
# # -----------
# 
# # 1) Pre-stroke
# # ------------------
# 
# data_contr_minus1 <- subset(data_final_long6, n_strokes.x == 0 & time == -1)
# data_contr_minus1 <- sample_n(data_contr_minus1, 350)
# data_contr_minus1 <- data_contr_minus1[,c("dep", "eff", "sle",
#                                           "hap", "lon", "enj", "sad", "goi")]
# cor(data_contr_minus1, use = "pairwise.complete.obs")
# hist(data_contr_minus1$adl)
# describe(data_contr_minus1$adl)
# 
# net_contr_minus1 <- estimateNetwork(data_contr_minus1, default = "mgm") 
# plot(net_contr_minus1, layout = "circle")
# 
# # 2) Post-Stroke 1
# # ------------------
# 
# data_contr_zero <- subset(data_final_long6, n_strokes.x == 0 & time == 0)
# data_contr_zero <- sample_n(data_contr_zero, 600)
# data_contr_zero <- data_contr_zero[,c("adl", "dep", "eff", "sle",
#                                       "hap", "lon", "enj", "sad", "goi")]
# describe(data_contr_zero)
# cormat_contr_zero <- cor(data_contr_zero, use = "pairwise.complete.obs")
# 
# net_contr_zero <- estimateNetwork(data_contr_zero, default = "mgm") 
# plot(net_contr_zero, layout = "circle")
# sum(abs(net_contr_zero$graph))/2 #6.27
# 
# qgraph(cormat_contr_zero, layout = "circle", minimum = 0.2)
# 
# 
# # 
# # # Network comparison
# # nct_pre_post <- NCT(net_contr_zero, net_stroke_zero, it = 1000, 
# #                     binary.data = T, paired = F)
# 
# # 3) Poststroke 2
# # ------------------
# 
# data_contr_plus1 <- subset(data_final_long6, n_strokes.x == 0 & time == 1)
# data_contr_plus1 <- data_contr_plus1[,c("adl", "dep", "eff", "hap", "lon", 
#                                           "enj", "sad", "goi")]
# net_contr_plus1 <- estimateNetwork(data_contr_plus1, default = "mgm") 
# plot(net_contr_plus1)
# 
# 
# 
# # Compare correlation ADL and depressive mood across groups and time
# 
# cor.test(data_contr_zero$dep,    data_contr_zero$adl,    method = "pearson")
# cor.test(data_stroke_minus1$dep, data_stroke_minus1$adl, method = "pearson")
# cor.test(data_stroke_zero$dep,   data_stroke_zero$adl,   method = "pearson")
# cor.test(data_stroke_plus1$dep,  data_stroke_plus1$adl,  method = "pearson")
# 
# paired.r(0.2407981, 0.2042797, n = 326, n2 = 314)
# 
# cor.test(data_contr_zero$adl,    data_contr_zero$eff,    method = "spearman")
# cor.test(data_stroke_minus1$adl, data_stroke_minus1$eff, method = "spearman")
# cor.test(data_stroke_zero$adl,   data_stroke_zero$eff,   method = "spearman")
# cor.test(data_stroke_plus1$adl,  data_stroke_plus1$eff,  method = "spearman")
# 
# cor.test(data_contr_zero$adl,    data_contr_zero$hap,    method = "pearson")
# cor.test(data_stroke_minus1$adl, data_stroke_minus1$hap, method = "pearson")
# cor.test(data_stroke_zero$adl,   data_stroke_zero$hap,   method = "pearson")
# cor.test(data_stroke_plus1$adl,  data_stroke_plus1$hap,  method = "pearson")
# 
# paired.r(-0.1844093 , -0.1855722, n = 315, n2 = 326)
