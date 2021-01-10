## ====================
##  Symptom analyses
## ====================

# need to apply all analyses to list of data and then average according 
# to rubin's rules (but only for controls)

# -----------
# Networks
# -----------

# with single symptom data, time -2
# --------------------------------------------

data_minus2_s <- subset(data_final_long8, stroke.x == 1 & time == -1)
data_minus2_s <- data_minus2_s[,c("adl", "mem", 
                                  "dep", "sle", "hap",
                                  "lon", "enj", 
                                  "sad", "eff", "goi",
                                  "stroke.x")]
data_minus2_c <- subset(data_final_long8, stroke.x == 0 & time == -1)
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


# ---------------------------
# Single symptom analyses
# ---------------------------


describeBy(data_minus2, data_minus2$stroke.x)

describeBy(data_zero, data_zero$stroke.x)

describeBy(data_plus2, data_plus2$stroke.x)


# Time point minus two
# ---------------------------

# reshape to long format for plotting
data_minus2_long <- reshape(data_minus2[3:11],
                          direction = "long", 
                          varying = list(names(data_minus2)[3:10]),
                          v.names = "score", 
                          timevar = "sympt")
head(data_minus2_long)

# need descriptive stats for plotting--> sacve those in object "stats"
stats = describeBy(data_minus2_long$score, 
                   group = list(data_minus2_long$stroke.x, data_minus2_long$sympt), 
                   mat = TRUE, 
                   skew = FALSE, 
                   ranges = FALSE) 

# rename factorsin stats for plotting again
names(stats)[names(stats) == "group1"] <- "stroke" 
names(stats)[names(stats) == "group2"] <- "symptom" 

# # re-order levels in stats to have desired ordering in plot
# data_exp_long$Alter_Gr   <- factor(data_exp_long$Alter_Gr, 
#                                    levels = c("Juenger", "Aelter"))

ggplot(data = stats, aes(y = mean, x = symptom, fill = stroke)) + 
        geom_bar(position = position_dodge(),
                 stat = "identity", 
                 colour = "black", size = .3, 
                 show.legend = T) + 
        theme_bw() +
        theme(panel.border     = element_blank(), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.title       = element_text(size = 15), 
              axis.line        = element_line(colour = "black"), 
              axis.text        = element_text(size = 15, colour = "black"), 
              legend.text      = element_text(size = 15)) + 
        labs(x = "" , 
             y = "Percent") +
        scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                         labels=c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")) +
        scale_y_continuous(expand = c(0,0), 
                           limits = c(0,1),  
                           breaks = c(0.2,0.4,0.6,0.8,1.0)) + 
        scale_fill_manual(values=c('grey90','#2d4059'), 
                          name = "", 
                          labels = c("Controls","Stroke")) 


# Time point zero 
# ---------------------------

# reshape to long format for plotting
data_zero_long <- reshape(data_zero[3:11],
                           direction = "long", 
                           varying = list(names(data_zero)[3:10]),
                           v.names = "score", 
                           timevar = "sympt")
head(data_zero_long)

# need descriptive stats for plotting--> sacve those in object "stats"
stats = describeBy(data_zero_long$score, 
                   group = list(data_zero_long$stroke.x, data_zero_long$sympt), 
                   mat = TRUE, 
                   skew = FALSE, 
                   ranges = FALSE) 

# rename factorsin stats for plotting again
names(stats)[names(stats) == "group1"] <- "stroke" 
names(stats)[names(stats) == "group2"] <- "symptom" 

# # re-order levels in stats to have desired ordering in plot
# data_exp_long$Alter_Gr   <- factor(data_exp_long$Alter_Gr, 
#                                    levels = c("Juenger", "Aelter"))

ggplot(data = stats, aes(y = mean, x = symptom, fill = stroke)) + 
        geom_bar(position = position_dodge(),
                 stat = "identity", 
                 colour = "black", size = .3, 
                 show.legend = T) + 
        theme_bw() +
        theme(panel.border     = element_blank(), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.title       = element_text(size = 15), 
              axis.line        = element_line(colour = "black"), 
              axis.text        = element_text(size = 15, colour = "black"), 
              legend.text      = element_text(size = 15)) + 
        labs(x = "" , 
             y = "Percent") +
        scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                         labels=c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")) +
        scale_y_continuous(expand = c(0,0), 
                           limits = c(0,1),  
                           breaks = c(0.2,0.4,0.6,0.8,1.0)) + 
        scale_fill_manual(values=c('grey90','#2d4059'), 
                          name = "", 
                          labels = c("Controls","Stroke")) 


# Time point plus 2
# ---------------------------

# reshape to long format for plotting
data_plus2_long <- reshape(data_plus2[3:11],
        direction = "long", 
        varying = list(names(data_plus2)[3:10]),
        v.names = "score", 
        timevar = "sympt")
head(data_plus2_long)

# need descriptive stats for plotting--> sacve those in object "stats"
stats = describeBy(data_plus2_long$score, 
                   group = list(data_plus2_long$stroke.x, data_plus2_long$sympt), 
                   mat = TRUE, 
                   skew = FALSE, 
                   ranges = FALSE) 

# rename factorsin stats for plotting again
names(stats)[names(stats) == "group1"] <- "stroke" 
names(stats)[names(stats) == "group2"] <- "symptom" 

# # re-order levels in stats to have desired ordering in plot
# data_exp_long$Alter_Gr   <- factor(data_exp_long$Alter_Gr, 
#                                    levels = c("Juenger", "Aelter"))

ggplot(data = stats, aes(y = mean, x = symptom, fill = stroke)) + 
        geom_bar(position = position_dodge(),
                 stat = "identity", 
                 colour = "black", size = .3, 
                 show.legend = T) + 
        theme_bw() +
        theme(panel.border     = element_blank(), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.title       = element_text(size = 15), 
              axis.line        = element_line(colour = "black"), 
              axis.text        = element_text(size = 15, colour = "black"), 
              legend.text      = element_text(size = 15)) + 
        labs(x = "" , 
             y = "Percent") +
        scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8"), 
                         labels=c("dep", "sle", "hap", "lon", "enj", "sad", "eff", "goi")) +
        scale_y_continuous(expand = c(0,0), 
                           limits = c(0,1),  
                           breaks = c(0.2,0.4,0.6,0.8,1.0)) + 
        scale_fill_manual(values=c('grey90','#2d4059'), 
                          name = "", 
                          labels = c("Controls","Stroke")) 


t.test(data_plus2$dep ~ data_plus2$stroke.x)
t.test(data_plus2$sle ~ data_plus2$stroke.x)
t.test(data_plus2$hap ~ data_plus2$stroke.x)
t.test(data_plus2$lon ~ data_plus2$stroke.x)
t.test(data_plus2$enj ~ data_plus2$stroke.x, var.equal = T)
t.test(data_plus2$sad ~ data_plus2$stroke.x)
t.test(data_plus2$eff ~ data_plus2$stroke.x)
t.test(data_plus2$goi ~ data_plus2$stroke.x)

cor.test(data_plus2$dep, data_plus2$stroke.x)
cor.test(data_plus2$sle, data_plus2$stroke.x)
cor.test(data_plus2$hap, data_plus2$stroke.x)
cor.test(data_plus2$lon, data_plus2$stroke.x)
cor.test(data_plus2$enj, data_plus2$stroke.x)
cor.test(data_plus2$sad, data_plus2$stroke.x)
cor.test(data_plus2$eff, data_plus2$stroke.x)
cor.test(data_plus2$goi, data_plus2$stroke.x)











# -----------
# Stroke 
# -----------

# 1) Pre-stroke
# ------------------

data_stroke_minus1 <- subset(data_final_long6, n_strokes.x == 1 & time == -2)
data_stroke_minus1 <- data_stroke_minus1[,c("adl", "dep", "eff", "sle",
                                            "hap", "lon", "enj", "sad", "goi")]
describe(data_stroke_minus1)
cor(data_stroke_minus1, use = "pairwise.complete.obs")

net_stroke_minus1 <- estimateNetwork(data_stroke_minus1, default = "mgm") 
plot(net_stroke_minus1, layout = "circle")
sum(abs(net_stroke_minus1$graph))/2 #5.36

# 2) Post-Stroke 1
# ------------------

data_stroke_zero <- subset(data_final_long6, n_strokes.x == 1 & time == 0)
data_stroke_zero <- data_stroke_zero[,c("adl", "dep", "eff", "sle",
                                        "hap", "lon", "enj", "sad", "goi")]
describe(data_stroke_zero)
cormat_stroke_zero <- cor(data_stroke_zero, use = "pairwise.complete.obs")

net_stroke_zero <- estimateNetwork(data_stroke_zero, default = "mgm") 
plot(net_stroke_zero, layout = "circle")
sum(abs(net_stroke_zero$graph))/2 #6.97

qgraph(cormat_stroke_zero, layout = "circle", minimum = 0.2)


# 3) Poststroke 2
# ------------------

data_stroke_plus1 <- subset(data_final_long6, n_strokes.x == 1 & time == 1)
data_stroke_plus1 <- data_stroke_plus1[,c("adl", "dep", "eff", "sle", 
                                          "hap", "lon", "enj", "sad", "goi")]
describe(data_stroke_plus1)
cor(data_stroke_plus1, use = "pairwise.complete.obs")

net_stroke_plus1 <- estimateNetwork(data_stroke_plus1, default = "mgm") 
plot(net_stroke_plus1, layout = "circle")
sum(abs(net_stroke_plus1$graph))/2 #6.39

# -----------
# Controls
# -----------

# 1) Pre-stroke
# ------------------

data_contr_minus1 <- subset(data_final_long6, n_strokes.x == 0 & time == -1)
data_contr_minus1 <- sample_n(data_contr_minus1, 350)
data_contr_minus1 <- data_contr_minus1[,c("dep", "eff", "sle",
                                          "hap", "lon", "enj", "sad", "goi")]
cor(data_contr_minus1, use = "pairwise.complete.obs")
hist(data_contr_minus1$adl)
describe(data_contr_minus1$adl)

net_contr_minus1 <- estimateNetwork(data_contr_minus1, default = "mgm") 
plot(net_contr_minus1, layout = "circle")

# 2) Post-Stroke 1
# ------------------

data_contr_zero <- subset(data_final_long6, n_strokes.x == 0 & time == 0)
data_contr_zero <- sample_n(data_contr_zero, 600)
data_contr_zero <- data_contr_zero[,c("adl", "dep", "eff", "sle",
                                      "hap", "lon", "enj", "sad", "goi")]
describe(data_contr_zero)
cormat_contr_zero <- cor(data_contr_zero, use = "pairwise.complete.obs")

net_contr_zero <- estimateNetwork(data_contr_zero, default = "mgm") 
plot(net_contr_zero, layout = "circle")
sum(abs(net_contr_zero$graph))/2 #6.27

qgraph(cormat_contr_zero, layout = "circle", minimum = 0.2)


# 
# # Network comparison
# nct_pre_post <- NCT(net_contr_zero, net_stroke_zero, it = 1000, 
#                     binary.data = T, paired = F)

# 3) Poststroke 2
# ------------------

data_contr_plus1 <- subset(data_final_long6, n_strokes.x == 0 & time == 1)
data_contr_plus1 <- data_contr_plus1[,c("adl", "dep", "eff", "hap", "lon", 
                                          "enj", "sad", "goi")]
net_contr_plus1 <- estimateNetwork(data_contr_plus1, default = "mgm") 
plot(net_contr_plus1)



# Compare correlation ADL and depressive mood across groups and time

cor.test(data_contr_zero$dep,    data_contr_zero$adl,    method = "pearson")
cor.test(data_stroke_minus1$dep, data_stroke_minus1$adl, method = "pearson")
cor.test(data_stroke_zero$dep,   data_stroke_zero$adl,   method = "pearson")
cor.test(data_stroke_plus1$dep,  data_stroke_plus1$adl,  method = "pearson")

paired.r(0.2407981, 0.2042797, n = 326, n2 = 314)

cor.test(data_contr_zero$adl,    data_contr_zero$eff,    method = "spearman")
cor.test(data_stroke_minus1$adl, data_stroke_minus1$eff, method = "spearman")
cor.test(data_stroke_zero$adl,   data_stroke_zero$eff,   method = "spearman")
cor.test(data_stroke_plus1$adl,  data_stroke_plus1$eff,  method = "spearman")

cor.test(data_contr_zero$adl,    data_contr_zero$hap,    method = "pearson")
cor.test(data_stroke_minus1$adl, data_stroke_minus1$hap, method = "pearson")
cor.test(data_stroke_zero$adl,   data_stroke_zero$hap,   method = "pearson")
cor.test(data_stroke_plus1$adl,  data_stroke_plus1$hap,  method = "pearson")

paired.r(-0.1844093 , -0.1855722, n = 315, n2 = 326)
