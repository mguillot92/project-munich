library(lubridate)
library(ggplot2)
library(dplyr)
library(mlVAR)
library(foreign)
library(dplyr)

setwd("D:/Descargas")
dataset2 <- read.table("DD_S1.csv", header=TRUE, sep=";", dec=".")
dataset3 <- read.table("DD_S2_N.csv", header=TRUE, sep=";", dec=".")
datasetESM <- read.table("ESM_S2_LELODEF.csv", header=TRUE, sep=";", dec=",")
colnames(datasetESM) [1] <- "participant"
dataset3complete <- datasetESM[complete.cases(datasetESM), ]
summary(datasetESM)
dataset2_num <- as.data.frame(apply(dataset2, 2, as.numeric))
dataset3_num <- as.data.frame(apply(dataset3, 2, as.numeric))
datasetESM <- as.data.frame(apply(datasetESM, 2, as.numeric))
rlang::last_error()
length(unique(dataset2$participant))
str(dataset2)
str(dataset3_num)

ggplot(dataset2, aes(x=event, y=valence)) + geom_point() + geom_line() + facet_wrap(~participant)

m2 <- mlVAR(dataset2_num, vars=c("content", "happy", "sad", "depr", "nerv", "angry", "fear", "guilt",
                             "shame", "irrit", "supp", "reap", "rum",
                             "ERQ_R","ERQ_S", "RRS_B", "RRS_R"),
                             idvar="participante", lags = 1, temporal ="orthogonal", estimator="lmer", verbose=TRUE)
round(cor(dataset2_num, use = "p"), 2)
m3 <- mlVAR(dataset3_num, vars=c("enthus", "happy", "calm", "guilty", "sleep",
                                "slug", "sad",  "energ", "nerv", "satis", "afraid",
                                "active", "alert", "proud","angry", "joy", "tired", "bored",
                                "relax", "shame", "disg", "embar", "upset"),
            idvar="participante", lags = 1, temporal ="orthogonal", estimator="lmer", verbose=TRUE)

m4 <- mlVAR(datasetESM, vars=c( "reap", "supp","rum_pa", "rum_fu", "sad", "angry", "anx" ,   "depr", "stress", "happy", "relax", "excit"),
            idvar="participant", lags = 1, temporal ="orthogonal", estimator="lmer", verbose=TRUE)



summary(m2)
summary(m3)
summary(m4)


mlVARcompare(m2,m3)

# Inspect true parameter correlation matrix:
m2$model$Omega$cor$mean
plot(m3)
plot(m4, "temporal", title = "Within-person temporal (lag-1) relations", layout = "circle", nonsig = "hide")
plot(m4, "contemporaneous", title = "Within-person contemporaneous relations",
     layout = "circle", nonsig = "hide")
plot(m3, "between", title = "Between-person relations", layout = "circle", nonsig = "hide")
