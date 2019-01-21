# PROJECT 01 - Elderly Driver Ingress/Egress Study
setwd("~/WORKING_DIRECTORIES/biostat.699/project01")

library(readr)
library(dplyr)
dataset01 <- read.csv("dataset01.csv") # load cleaned version of thedata.csv
dataset02 <- read.csv("dataset02.csv") # load cleaned version of Copy+of+Elderly+Driver+Data.xlsx
dataset02 <- dataset02[,1:38] # has three unnecessary columns when loaded for some reason
colnames(dataset02)[1] <- "Subject" # first column name gets messed up for some reason
dataset03 <- left_join(dataset02,dataset01[,c(1,4:29)],by="Subject") # merge datasets

library(ggplot2)

# Look at average ingress and egress times for different vehicles

TIME_BY_VD <- dataset03 %>% group_by(vehicle_door) %>% summarise(avg_ing = mean(ingress, na.rm = T),
                                                   avg_eg = mean(egress, na.rm = T))

facet_labels <- c(
  '0' = "Parking Lot",
  '1' = "Open"
)

plot01 <- ggplot(data = dataset03,
                 aes(y = ingress,
                     x = vehicle,
                     group = vehicle)) +
  geom_boxplot() +
  facet_grid(.~open, labeller = as_labeller(facet_labels)) +
  xlab("Vehicle") +
  ylab("Ingress Time (seconds)") +
  theme_bw()
plot01

plot02 <- ggplot(data = dataset03,
                 aes(y = egress,
                     x = vehicle,
                     group = vehicle)) +
  geom_boxplot() +
  facet_grid(.~open, labeller = as_labeller(facet_labels)) +
  xlab("Vehicle") +
  ylab("Egress Time (seconds)") +
  theme_bw()
plot02

# Look at a spaghetti plot of the ingress and egress times for each subject

plot03 <- ggplot(data = dataset03,
                 aes(x = vehicle_door,
                     y = ingress,
                     group = Subject,
                     color = Group)) +
  geom_line() +
  xlab("Vehicle-Door Combination") +
  ylab("Ingress Time (seconds)") +
  theme_bw()
plot03

plot04 <- ggplot(data = dataset03,
                 aes(x = vehicle_door,
                     y = egress,
                     group = Subject,
                     color = Group)) +
  geom_line() +
  xlab("Vehicle-Door Combination") +
  ylab("Egress Time (seconds)") +
  theme_bw()
plot04

# Look at correlation structures
library(corrplot)
body.m <- cor(dataset03[,c(3,5:18)])
corrplot(body.m, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
apply(as.data.frame(body.m), 1, FUN=min)
apply(as.data.frame(body.m), 1, FUN=mean)
# weight variable to repesent body measures

strength.m <- cor(dataset03[,c(49,51,55,57,59)], use = "complete.obs")
corrplot(strength.m, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
apply(as.data.frame(strength.m), 1, FUN=min)
apply(as.data.frame(strength.m), 1, FUN=mean)
# HipABd variable to represent strength measures

dataset03 <- mutate(dataset03, OLB_good = (OLB_R + OLB_L) > 40)
dataset03 <- mutate(dataset03, OLB_poor = (OLB_R + OLB_L) < 15)
dataset03 <- mutate(dataset03, OLB_okay = ((OLB_R + OLB_L) >= 15) & ((OLB_R + OLB_L) <= 40))
dataset03 <- mutate(dataset03, OLB_score = 200*OLB_poor + 100*OLB_okay + 50*OLB_good)
mobility.m <- cor(dataset03[,c(43,45,47)], use = "complete.obs")
View(mobility.m)
corrplot(mobility.m, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# OLB_L variable to represent mobility measures

cognitive.m <- cor(dataset03[,c(40,53,54,61:64)], use = "complete.obs")
corrplot(cognitive.m, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
apply(as.data.frame(cognitive.m), 1, FUN=min)
apply(as.data.frame(cognitive.m), 1, FUN=mean)
# weak correlations between cognitive measures

# create linear model to look at associations