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

veh17 <- subset(dataset03, vehicle == 1 | vehicle == 7)

plot01 <- ggplot(data = dataset03,
                 aes(y = ingress,
                     group = factor(vehicle),
                     fill = factor(vehicle))) +
  geom_boxplot() +
  facet_grid(.~open, labeller = as_labeller(facet_labels)) +
  xlab("Vehicle") +
  ylab("Ingress Time (seconds)") +
  theme_gray() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_fill_discrete(labels = c("Vehicle #1","Vehicle #2","Vehicle #3",
                                 "Vehicle #4","Vehicle #5","Vehicle #6",
                                 "Vehicle #7"), name = "")
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
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Spaghetti Plot of Ingress Times by Mobility Impairment Group")
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

# Histogram of Summed One Leg Balancing Times
hist(dataset03$OLB_R + dataset03$OLB_L, 
     main = "Distribution of Sum Times", 
     xlab = "Sum of Max Times from Each Leg", 
     ylab = "Frequency", cex.lab=1.5,
     cex.axis=1.5, cex.main=2, cex.sub=1.5)

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
dataset03 <- mutate(dataset03, OLB_poor = (OLB_R + OLB_L) < 10)
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
dataset03 <- mutate(dataset03,
                    age_c = Age - mean(Age),
                    weight_c = weight - mean(weight),
                    HipABd_c = HipABd - mean(HipABd),
                    TrailsB_c = TrailsB - mean(TrailsB),
                    dim8c = dim08 - mean(dim08),
                    dim9c = dim09 - mean(dim09),
                    log_ing = log(ingress),
                    log_eg = log(egress))
dataset04 <- subset(dataset03, in_strategy != "")
model01 <- lm(data = dataset04,
              egress ~ age_c + factor(Gender) + factor(vehicle) + open + weight_c + 
                HipABd_c + OLB_poor + factor(Group) + factor(e_strategy))
summary(model01)

library(lme4)
model02 <- lmer(data = dataset04,
                ingress ~ age_c + factor(Gender) + open + (1+open|vehicle) + weight_c + 
                  HipABd_c + OLB_poor + factor(Group) + factor(in_strategy))
summary(model02)
ranef(model02)

model03 <- lmer(data = dataset04,
                egress ~ age_c + factor(Gender) + open + (1|vehicle) + (1|Subject) + weight_c + 
                  HipABd_c + OLB_poor + factor(Group) + factor(e_strategy))
summary(model03)
library(lmerTest)
model04 <- lmer(data = dataset04,
                egress ~ age_c + factor(Gender) + open + (1|Subject) + weight_c + 
                HipABd_c + OLB_L + factor(Group) + factor(e_strategy) + TrailsB_c +
                dim8c + dim9c)
summary(model04)
glance(model04)

model05 <- lmer(data = dataset04,
                ingress ~ weight_c + (1|Subject))
r.squaredGLMM(model05)
summary(model05)

library(broom)
lik_02 <- glance(model02)$logLik
lik_03 <- glance(model03)$logLik
LRT_stat <- -2*(lik_03-lik_02)
pchisq(LRT_stat,1,lower.tail = FALSE)

# Look at correlation structures of vehicle dimensions
library(corrplot)
vehdim.m <- cor(dataset03[,c(22:32)])
car.dim <- c("Dim01","Dim02","Dim03","Dim04","Dim05","Dim06","Dim07","Dim08",
             "Dim09","Dim10","Dim11")
rownames(body.m) <- car.dim
colnames(body.m) <- car.dim
corrplot(body.m, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Radar chart for car dimensions
library(fmsb)
car_dim <- dataset03 %>%
  group_by(vehicle) %>%
  summarize(Dim01 = mean(dim01),
            Dim02 = mean(dim02),
            Dim03 = mean(dim03),
            Dim04 = mean(dim04),
            Dim05 = mean(dim05),
            Dim06 = mean(dim06),
            Dim07 = mean(dim07),
            Dim08 = mean(dim08),
            Dim09 = mean(dim09),
            Dim10 = mean(dim10),
            Dim11 = mean(dim11))
max_dim <- c(max(car_dim$Dim01),
             max(car_dim$Dim02),
             max(car_dim$Dim03),
             max(car_dim$Dim04),
             max(car_dim$Dim05),
             max(car_dim$Dim06),
             max(car_dim$Dim07),
             max(car_dim$Dim08),
             max(car_dim$Dim09),
             max(car_dim$Dim10),
             max(car_dim$Dim11))
min_dim <- c(min(car_dim$Dim01),
             min(car_dim$Dim02),
             min(car_dim$Dim03),
             min(car_dim$Dim04),
             min(car_dim$Dim05),
             min(car_dim$Dim06),
             min(car_dim$Dim07),
             min(car_dim$Dim08),
             min(car_dim$Dim09),
             min(car_dim$Dim10),
             min(car_dim$Dim11))
car_dim <- rbind(max_dim,min_dim,car_dim[,2:12])
colors_border <- c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
colors_in <- c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
radarchart(car_dim[c(1,2,4,9),],
           axistype = 0, vlcex = 0.8,
           cglcol = "gray", cglty = 1, axislabcol = "black", cglwd = 0.8,
           pcol = colors_border, pfcol = colors_in, plwd = 1, plty = 1)
theme(text = element_text(size = 28))
legend(x=1, y=1.3, legend = c("Vehicle #2","Vehicle #7"), bty = "n", pch=20 ,
       col=colors_in , text.col = "black", cex=1.2, pt.cex=3)
