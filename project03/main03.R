# Project 03
setwd("~/WORKING_DIRECTORIES/biostat.699/project03")
library(readr)
library(dplyr)
path <- read_csv("PATH699.csv")

# Split data into the three waves
wave01 <- path[1:32320,c(1:114)]
wave02 <- path[32321:60682,c(1:13,115:215)]
wave03 <- path[60683:88830,c(1:13,216:417)]

# Create label for waves of participation for each subject
lab01 <- data.frame(wave01$PERSONID,rep(1,nrow(wave01)))
colnames(lab01) <- c("PERSONID","wave")
lab02 <- data.frame(wave02$PERSONID,rep(4,nrow(wave02)))
colnames(lab02) <- c("PERSONID","wave")
lab03 <- data.frame(wave03$PERSONID,rep(7,nrow(wave03)))
colnames(lab03) <- c("PERSONID","wave")
labels <- rbind(lab01,lab02,lab03)
labels <- labels %>% group_by(PERSONID) %>% summarise(label = sum(wave))
labels$waves <- "All 3 Waves"
labels$waves[labels$label==1] <- "Wave 1 Only"
labels$waves[labels$label==4] <- "Wave 2 Only"
labels$waves[labels$label==7] <- "Wave 3 Only"
labels$waves[labels$label==5] <- "Waves 1 & 2"
labels$waves[labels$label==8] <- "Waves 1 & 3"
labels$waves[labels$label==11] <- "Waves 2 & 3"
wave01 <- left_join(wave01, labels, by = "PERSONID") # 32,320 total subjects
wave02 <- left_join(wave02, labels, by = "PERSONID") # 28,362 total subjects
wave03 <- left_join(wave03, labels, by = "PERSONID") # 28,148 total subjects

# Number of subjects in each wave (36,142 total subjects)
sum(labels$label==1)  # 5,016 subjects in Wave 1 Only
sum(labels$label==4)  # 201 subjects in Wave 2 Only
sum(labels$label==5)  # 2,777 subjects in Waves 1 & 2
sum(labels$label==7)  # 1,907 subjects in Wave 3 Only
sum(labels$label==8)  # 857 subjects in Waves 1 & 3
sum(labels$label==11) # 1,714 subjects in Waves 2 & 3
sum(labels$label==12) # 23,670 subjects in All 3 Waves