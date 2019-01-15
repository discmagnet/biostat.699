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
                 aes(y = ingress, x = vehicle, group = vehicle)) +
  geom_boxplot() +
  facet_grid(.~open, labeller = as_labeller(facet_labels)) +
  xlab("Vehicle") +
  ylab("Ingress Time (seconds)") +
  theme_bw()
plot01

plot02 <- ggplot(data = dataset03,
                 aes(y = egress, x = vehicle, group = vehicle)) +
  geom_boxplot() +
  facet_grid(.~open, labeller = as_labeller(facet_labels)) +
  xlab("Vehicle") +
  ylab("Egress Time (seconds)") +
  theme_bw()
plot02

# Look at a spaghetti plot of the ingress and egress times for each subject

plot03 <- ggplot(data = dataset03,
                 aes(x = vehicle_door, y = ingress, group = Subject, color = Group)) +
  geom_line() +
  xlab("Vehicle-Door Combination") +
  ylab("Ingress Time (seconds)") +
  theme_bw()
plot03

plot04 <- ggplot(data = dataset03,
                 aes(x = vehicle_door, y = egress, group = Subject, color = Group)) +
  geom_line() +
  xlab("Vehicle-Door Combination") +
  ylab("Egress Time (seconds)") +
  theme_bw()
plot04