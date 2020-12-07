#install.packages("bootnet")
library(bootnet)
library(qgraph)

# randomly selected data_final_long6 for anaylses!

# -----------
# Stroke 
# -----------

# 1) Pre-stroke
# ------------------

data_stroke_minus1 <- subset(data_final_long6, n_strokes.x == 1 & time == -1)
data_stroke_minus1 <- data_stroke_minus1[,c("adl", "dep", "eff", "hap", "lon", 
                                            "enj", "sad", "goi")]
cor(data_stroke_minus1, use = "pairwise.complete.obs")

hist(data_stroke_minus1$adl)
describe(data_stroke_minus1$adl)

net_stroke_minus1 <- estimateNetwork(data_stroke_minus1, default = "mgm") 
plot(net_stroke_minus1, layout = "circle")

# 2) Post-Stroke 1
# ------------------

data_stroke_zero <- subset(data_final_long6, n_strokes.x == 1 & time == 0)
data_stroke_zero <- data_stroke_zero[,c("adl", "dep", "eff", "hap", "lon", 
                                            "enj", "sad", "goi")]
cor(data_stroke_zero, use = "pairwise.complete.obs")
hist(data_stroke_zero$adl)
describe(data_stroke_zero$adl)

net_stroke_zero <- estimateNetwork(data_stroke_zero, default = "mgm") 
plot(net_stroke_zero, layout = "circle")


# 3) Poststroke 2
# ------------------

data_stroke_plus1 <- subset(data_final_long6, n_strokes.x == 1 & time == 1)
data_stroke_plus1 <- data_stroke_plus1[,c("adl", "dep", "eff", "hap", "lon", 
                                        "enj", "sad", "goi")]
cor(data_stroke_plus1, use = "pairwise.complete.obs")

net_stroke_plus1 <- estimateNetwork(data_stroke_plus1, default = "mgm") 
plot(net_stroke_plus1, layout = "circle")


# -----------
# Controls
# -----------

# 1) Pre-stroke
# ------------------

data_contr_minus1 <- subset(data_final_long6, n_strokes.x == 0 & time == -1)
data_contr_minus1 <- sample_n(data_contr_minus1, 375)
data_contr_minus1 <- data_contr_minus1[,c("adl", "dep", "eff", "hap", "lon", 
                                            "enj", "sad", "goi")]
cor(data_contr_minus1, use = "pairwise.complete.obs")
hist(data_contr_minus1$adl)
describe(data_contr_minus1$adl)

net_contr_minus1 <- estimateNetwork(data_contr_minus1, default = "mgm") 
plot(net_contr_minus1, layout = "circle")

# 2) Post-Stroke 1
# ------------------

data_contr_zero <- subset(data_final_long6, n_strokes.x == 0 & time == 0)
data_contr_zero <- data_contr_zero[,c("adl", "dep", "eff", "hap", "lon", 
                                        "enj", "sad", "goi")]
net_contr_zero <- estimateNetwork(data_contr_zero, default = "mgm") 
plot(net_contr_zero)


# 3) Poststroke 2
# ------------------

data_contr_plus1 <- subset(data_final_long6, n_strokes.x == 0 & time == 1)
data_contr_plus1 <- data_contr_plus1[,c("adl", "dep", "eff", "hap", "lon", 
                                          "enj", "sad", "goi")]
net_contr_plus1 <- estimateNetwork(data_contr_plus1, default = "mgm") 
plot(net_contr_plus1)

