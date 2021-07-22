setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# libraries
library(neuralnet)
library(ggplot2)
library(tidyverse)
library(NeuralNetTools)
library(caret)
# data preparation
wine <- read_csv("winequalityN.csv")
colnames(wine) <- gsub(" ", "_", colnames(wine))
wine <- wine %>% drop_na()
wine$quality <- as.character(wine$quality)

# importing each class
dat <- transmute(wine, alcohol, density, volatile_acidity, chlorides, quality)
dat$qual3 <- dat$quality == "3"
dat$qual4 <- dat$quality == "4"
dat$qual5 <- dat$quality == "5"
dat$qual6 <- dat$quality == "6"
dat$qual7 <- dat$quality == "7"
dat$qual8 <- dat$quality == "8"


train_dat <- dat[1:4000,]
test_dat <- dat[4001:nrow(dat),]

model = as.formula("quality ~ alcohol + density + volatile_acidity + chlorides")
wine.net <- neuralnet(model,
                      data=train_dat, 
                      hidden = c(10,10),
                      rep = 5,
                      act.fct = "logistic",
                      err.fct = "ce",
                      linear.output = F, 
                      lifesign = "full", 
                      stepmax = 1000,
                      threshold = 13)

plot(wine.net)

wine.prediction <- neuralnet::compute(wine.net, test_dat)
idx <- apply(wine.prediction$net.result, 1, which.max)

predicted <- as.factor(c('3', '4', '5', '6', '7', '8'))[idx]
confusionMatrix(predicted, as.factor(test_dat$quality))
