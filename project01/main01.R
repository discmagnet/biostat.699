# PROJECT 01 - Elderly Driver Ingress/Egress Study
setwd("~/WORKING_DIRECTORIES/biostat.699/project01")

library(readr)
dataset01 <- read.csv("dataset01.csv")
dataset02 <- read.csv("dataset02.csv")
dataset02 <- dataset02[,1:38] # has three unnecessary columns when loaded for some reason
library(ggplot2)

# 