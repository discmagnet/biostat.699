# Project 03
setwd("~/WORKING_DIRECTORIES/biostat.699/project03")
library(data.table)
library(dplyr)
library(survey)
path <- fread("PATH699_correction.csv")

# Split data into the three waves
wave01 <- subset(path,wave==1)
wave02 <- subset(path,wave==2)
wave03 <- subset(path,wave==3)

# Merge waves together
merge01 <- wave01[,1:13]
names(merge01)[9:13] <- c("W1smoke_status","W1menth","W1ecig","W1flav","W1quit")
merge02 <- wave02[,c(1,9,13)]
names(merge02)[2:3] <- c("W2smoke_status","W2quit")
merge03 <- wave03[,c(1,9,13,216:417)]
names(merge03)[2:3] <- c("W3smoke_status","W3quit")

waves0102 <- merge(merge01,merge02,by="PERSONID")
all_waves <- merge(waves0102,merge03,by="PERSONID")
weights <- data.frame(all_waves[,18:118])

# Define smoker
all_waves$SMOKER[all_waves$W1smoke_status == 2] <- 1
# Define quitter
all_waves$QUITTER[all_waves$W1smoke_status == 2 & all_waves$W3quit == 1] <- 1 
# say they quit in third wave
all_waves$QUITTER[all_waves$W1smoke_status == 2 & (is.na(all_waves$W3quit) & all_waves$W3smoke_status == 1)] <- 1 
# they are a former smoker
all_waves$QUITTER[all_waves$W1smoke_status == 2 & (is.na(all_waves$W3quit) & all_waves$W3smoke_status == 2)] <- 0 
# they are a current smoker
all_waves$QUITTER[all_waves$W1smoke_status == 2 & (all_waves$W3quit == 0 & all_waves$W3smoke_status == 2)] <- 0 
# they relapse in the third wave
all_waves$QUITTER[all_waves$W1smoke_status == 2 & (all_waves$W3quit == 0 & all_waves$W3smoke_status == 1 & all_waves$W2quit == 1)] <- 1 
# they quit in the second wave
all_waves$QUITTER[all_waves$W1smoke_status == 2 & (all_waves$W3quit == 0 & all_waves$W3smoke_status == 1 & all_waves$W2quit == 0)] <- 0 
# they quit in the second wave
all_waves$QUITTER[all_waves$W1smoke_status == 2 & (all_waves$W3quit == 0 & all_waves$W3smoke_status == 1 & is.na(all_waves$W2quit) & all_waves$W2smoke_status == 1)] <- 1 
# they did not quit in the second wave either
all_waves$QUITTER[all_waves$W1smoke_status == 2 & (all_waves$W3quit == 0 & all_waves$W3smoke_status == 1 & is.na(all_waves$W2quit) & all_waves$W2smoke_status == 2)] <- 0 
# and they are a former smoker in the third wave
View(select(subset(all_waves,W1smoke_status == 2),c(1,9:17,220,221)))
sum(all_waves$QUITTER, na.rm = T)

all_waves$FLAV[all_waves$W1smoke_status == 2 & all_waves$W1ecig == 1 & all_waves$W1flav == 1] <- 2
all_waves$FLAV[all_waves$W1smoke_status == 2 & all_waves$W1ecig == 1 & (all_waves$W1flav == 0 | is.na(all_waves$W1flav))] <- 1
all_waves$FLAV[all_waves$W1smoke_status == 2 & (all_waves$W1ecig == 0 | is.na(all_waves$W1ecig)) & (all_waves$W1flav == 0 | is.na(all_waves$W1flav))] <- 0
all_waves$FLAV <- relevel(factor(all_waves$FLAV), ref = "2")

all_waves$RE <- relevel(factor(all_waves$RE), ref = "2")

# Create designs
y <- svrepdesign(id = ~PERSONID,
                 weights = ~R03_A_AWGT,
                 repweights = weights,
                 type = "Fay",
                 rho = 0.3,
                 data = all_waves)

all_smokers <- subset(all_waves, SMOKER == 1)
smoke_weights <- data.frame(all_smokers[,18:118])

z <- svrepdesign(id = ~PERSONID,
                 weights = ~R03_A_AWGT,
                 repweights = smoke_weights,
                 type = "Fay",
                 rho = 0.3,
                 data = all_smokers)

# Logistic Regression - Unadjusted Menthol Smoking
mod01 <- svyglm(factor(QUITTER) ~ factor(W1menth),
                design = y,
                family = quasibinomial)
summary(mod01)

# Logistic Regression - Adjusted Menthol Smoking
mod02 <- svyglm(factor(QUITTER) ~ factor(W1menth) + factor(male) + factor(agegrp) + 
                  factor(RE) + factor(edu) + factor(income) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod02)

# Logistic Regression - Menthol/Gender Interaction
mod03 <- svyglm(factor(QUITTER) ~ factor(W1menth)*factor(male) + factor(agegrp) + 
                  factor(RE) + factor(edu) + factor(income) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod03)

# Logistic Regression - Menthol/Age Interaction
mod04 <- svyglm(factor(QUITTER) ~ factor(W1menth)*factor(agegrp) + factor(male) + 
                  factor(RE) + factor(edu) + factor(income) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod04)

# Logistic Regression - Menthol/Race Interaction
mod05 <- svyglm(factor(QUITTER) ~ factor(W1menth)*factor(RE) + factor(agegrp) + 
                  factor(male) + factor(edu) + factor(income) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod05)

# Logistic Regression - Menthol/Education Interaction
mod06 <- svyglm(factor(QUITTER) ~ factor(W1menth)*factor(edu) + factor(agegrp) + 
                  factor(RE) + factor(male) + factor(income) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod06)

# Logistic Regression - Menthol/Income Interaction
mod07 <- svyglm(factor(QUITTER) ~ factor(W1menth)*factor(income) + factor(agegrp) + 
                  factor(RE) + factor(edu) + factor(male) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod07)

# Logistic Regression - Menthol/Region Interaction
mod08 <- svyglm(factor(QUITTER) ~ factor(W1menth)*factor(region) + factor(agegrp) + 
                  factor(RE) + factor(edu) + factor(income) + factor(male),
                design = y,
                family = quasibinomial)
summary(mod08)

# Logistic Regression - Unadjusted Flavored E-Cig Smoking
mod09 <- svyglm(factor(QUITTER) ~ factor(FLAV),
                design = y,
                family = quasibinomial)
summary(mod09)

# Logistic Regression - Adjusted Flavored E-Cig Smoking
mod10 <- svyglm(factor(QUITTER) ~ factor(FLAV) + factor(male) + factor(agegrp) + 
                  factor(RE) + factor(edu) + factor(income) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod10)

# Logistic Regression - Flavored E-Cig/Gender Interaction
mod11 <- svyglm(factor(QUITTER) ~ factor(FLAV)*factor(male) + factor(agegrp) + 
                  factor(RE) + factor(edu) + factor(income) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod11)

# Logistic Regression - Flavored E-Cig/Age Interaction
mod12 <- svyglm(factor(QUITTER) ~ factor(FLAV)*factor(agegrp) + factor(male) + 
                  factor(RE) + factor(edu) + factor(income) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod12)

# Logistic Regression - Flavored E-Cig/Race Interaction
mod13 <- svyglm(factor(QUITTER) ~ factor(FLAV)*factor(RE) + factor(agegrp) + 
                  factor(male) + factor(edu) + factor(income) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod13)

# Logistic Regression - Flavored E-Cig/Education Interaction
mod14 <- svyglm(factor(QUITTER) ~ factor(FLAV)*factor(edu) + factor(agegrp) + 
                  factor(RE) + factor(male) + factor(income) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod14)

# Logistic Regression - Flavored E-Cig/Income Interaction
mod15 <- svyglm(factor(QUITTER) ~ factor(FLAV)*factor(income) + factor(agegrp) + 
                  factor(RE) + factor(edu) + factor(male) + factor(region),
                design = y,
                family = quasibinomial)
summary(mod15)

# Logistic Regression - Flavored E-Cig/Region Interaction
mod16 <- svyglm(factor(QUITTER) ~ factor(FLAV)*factor(region) + factor(agegrp) + 
                  factor(RE) + factor(edu) + factor(income) + factor(male),
                design = y,
                family = quasibinomial)
summary(mod16)

# Likelihood Ratio Tests for Menthol Interactions

# Gender
mod02$deviance-mod03$deviance
1-pchisq(mod02$deviance-mod03$deviance,mod02$df.residual-mod03$df.residual)
# Age
mod02$deviance-mod04$deviance
1-pchisq(mod02$deviance-mod04$deviance,mod02$df.residual-mod04$df.residual)
# Race
mod02$deviance-mod05$deviance
1-pchisq(mod02$deviance-mod05$deviance,mod02$df.residual-mod05$df.residual)
# Education
mod02$deviance-mod06$deviance
1-pchisq(mod02$deviance-mod06$deviance,mod02$df.residual-mod06$df.residual)
# Income
mod02$deviance-mod07$deviance
1-pchisq(mod02$deviance-mod07$deviance,mod02$df.residual-mod07$df.residual)
# Region
mod02$deviance-mod08$deviance
1-pchisq(mod02$deviance-mod08$deviance,mod02$df.residual-mod08$df.residual)

# Forest Plot
savedM <- tidy(mod02)
coefficients <- savedM$estimate[2:19]
se <- savedF$std.error[2:19]
lb <- coefficients - 1.96*se
ub <- coefficients + 1.96*se
coefs <- data.frame(coefficients,se,lb,ub)

coefs$var[1] <- "Menthol Smoker"
coefs$var[2] <- "Male"
coefs$var[3] <- "Age Group, 25-34"
coefs$var[4] <- "Age Group, 35-54"
coefs$var[5] <- "Age Group, 55+"
coefs$var[6]<- "Hispanic"
coefs$var[7] <- "Non-Hispanic Black"
coefs$var[8] <- "Non-Hispanic Other"
coefs$var[9] <- "High School/GED"
coefs$var[10] <- "Some College"
coefs$var[11] <- "College or More"
coefs$var[12] <- "Income, $10,000 to $24,999"
coefs$var[13] <- "Income, $25,000 to $49,999"
coefs$var[14] <- "Income, $50,000 to $99,999"
coefs$var[15] <- "Income, $100,000 or More"
coefs$var[16] <- "Region, Midwest"
coefs$var[17] <- "Region, South"
coefs$var[18] <- "Region, West"

coefs$ordervalue[1] <- 18
coefs$ordervalue[2] <- 17
coefs$ordervalue[3] <- 16
coefs$ordervalue[4] <- 15
coefs$ordervalue[5] <- 14
coefs$ordervalue[6] <- 13
coefs$ordervalue[7] <- 12
coefs$ordervalue[8] <- 11
coefs$ordervalue[9] <- 10
coefs$ordervalue[10] <- 9
coefs$ordervalue[11] <- 8
coefs$ordervalue[12] <- 7
coefs$ordervalue[13] <- 6
coefs$ordervalue[14] <- 5
coefs$ordervalue[15] <- 4
coefs$ordervalue[16] <- 3
coefs$ordervalue[17] <- 2
coefs$ordervalue[18] <- 1


coefs$var <- factor(coefs$var, levels = coefs$var[order(coefs$ordervalue)])

ggplot() + 
  geom_vline(data= coefs, xintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbarh(data= coefs, aes(y= var, xmin=lb, xmax=ub), 
                 height = 0.2, colour="blue") +
  geom_point(data=coefs, mapping = aes(y=var,x=coefficients), fill = "red")+
  ggtitle("Menthol Cigarette Smoking and Log Odds of Quitting\n")+ xlab("Log Odds of Quitting") + ylab("Variable")+
  theme(plot.title = element_text(size = 13),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size=10,face="bold"))
theme_bw()


# Likelihood Ratio Tests for Flavored E-Cig Interactions

# Gender
mod10$deviance-mod11$deviance
1-pchisq(mod10$deviance-mod11$deviance,mod10$df.residual-mod11$df.residual)
# Age
mod10$deviance-mod12$deviance
1-pchisq(mod10$deviance-mod12$deviance,mod10$df.residual-mod12$df.residual)
# Race
mod10$deviance-mod13$deviance
1-pchisq(mod10$deviance-mod13$deviance,mod10$df.residual-mod13$df.residual)
# Education
mod10$deviance-mod14$deviance
1-pchisq(mod10$deviance-mod14$deviance,mod10$df.residual-mod14$df.residual)
# Income
mod10$deviance-mod15$deviance
1-pchisq(mod10$deviance-mod15$deviance,mod10$df.residual-mod15$df.residual)
# Region
mod10$deviance-mod16$deviance
1-pchisq(mod10$deviance-mod16$deviance,mod10$df.residual-mod16$df.residual)

# Forest Plot
savedF <- tidy(mod10)
coefficients <- savedF$estimate[2:20]
se <- savedF$std.error[2:20]
lb <- coefficients - 1.96*se
ub <- coefficients + 1.96*se
coefs <- data.frame(coefficients,se,lb,ub)

coefs$var[1] <- "Not an Ecig User"
coefs$var[2] <- "Ecig without Flavoring"
coefs$var[3] <- "Male"
coefs$var[4] <- "Age Group, 25-34"
coefs$var[5] <- "Age Group, 35-54"
coefs$var[6] <- "Age Group, 55+"
coefs$var[7]<- "Hispanic"
coefs$var[8] <- "Non-Hispanic Black"
coefs$var[9] <- "Non-Hispanic Other"
coefs$var[10] <- "High School/GED"
coefs$var[11] <- "Some College"
coefs$var[12] <- "College or More"
coefs$var[13] <- "Income, $10,000 to $24,999"
coefs$var[14] <- "Income, $25,000 to $49,999"
coefs$var[15] <- "Income, $50,000 to $99,999"
coefs$var[16] <- "Income, $100,000 or More"
coefs$var[17] <- "Region, Midwest"
coefs$var[18] <- "Region, South"
coefs$var[19] <- "Region, West"

coefs$ordervalue[1] <- 19
coefs$ordervalue[2] <- 18
coefs$ordervalue[3] <- 17
coefs$ordervalue[4] <- 16
coefs$ordervalue[5] <- 15
coefs$ordervalue[6] <- 14
coefs$ordervalue[7] <- 13
coefs$ordervalue[8] <- 12
coefs$ordervalue[9] <- 11
coefs$ordervalue[10] <- 10
coefs$ordervalue[11] <- 9
coefs$ordervalue[12] <- 8
coefs$ordervalue[13] <- 7
coefs$ordervalue[14] <- 6
coefs$ordervalue[15] <- 5
coefs$ordervalue[16] <- 4
coefs$ordervalue[17] <- 3
coefs$ordervalue[18] <- 2
coefs$ordervalue[19] <- 1

coefs$var <- factor(coefs$var, levels = coefs$var[order(coefs$ordervalue)])

library(ggplot2)
ggplot() + 
  geom_vline(data= coefs, xintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbarh(data= coefs, aes(y= var, xmin=lb, xmax=ub), 
                 height = 0.2, colour="blue") +
  geom_point(data=coefs, mapping = aes(y=var,x=coefficients), fill = "red")+
  ggtitle("E-cigarette Smoking and Log Odds of Quitting\n")+ xlab("Log Odds of Quitting") + ylab("Variable")+
  theme(plot.title = element_text(size = 13),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size=10,face="bold"))
theme_bw()