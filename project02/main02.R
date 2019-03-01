setwd("~/WORKING_DIRECTORIES/biostat.699/project02")
fld <- read.csv("~/WORKING_DIRECTORIES/biostat.699/project02/FLdata.csv")

fld$drug01id <- NULL
fld$drug02id <- NULL

for(i in 1:nrow(fld)){
  fld$drug01id[i][fld$d1_2[i]==1] <- paste(fld$drug01id[i], 2,sep = " ")
  fld$drug01id[i][fld$d1_5[i]==1] <- paste(fld$drug01id[i], 5,sep = " ")
  fld$drug01id[i][fld$d1_14[i]==1] <- paste(fld$drug01id[i], 14,sep = " ")
  fld$drug01id[i][fld$d1_17[i]==1] <- paste(fld$drug01id[i], 17,sep = " ")
  fld$drug01id[i][fld$d1_20[i]==1] <- paste(fld$drug01id[i], 20,sep = " ")
  fld$drug01id[i][fld$d1_21[i]==1] <- paste(fld$drug01id[i], 21,sep = " ")
  fld$drug01id[i][fld$d1_22[i]==1] <- paste(fld$drug01id[i], 22,sep = " ")
  fld$drug01id[i][fld$d1_23[i]==1] <- paste(fld$drug01id[i], 23,sep = " ")
  fld$drug01id[i][fld$d1_24[i]==1] <- paste(fld$drug01id[i], 24,sep = " ")
  fld$drug01id[i][fld$d1_29[i]==1] <- paste(fld$drug01id[i], 29,sep = " ")
  fld$drug01id[i][fld$d1_32[i]==1] <- paste(fld$drug01id[i], 32,sep = " ")
  fld$drug01id[i][fld$d1_33[i]==1] <- paste(fld$drug01id[i], 33,sep = " ")
  fld$drug01id[i][fld$d1_35[i]==1] <- paste(fld$drug01id[i], 35,sep = " ")
  fld$drug01id[i][fld$d1_39[i]==1] <- paste(fld$drug01id[i], 39,sep = " ")
  fld$drug01id[i][fld$d1_40[i]==1] <- paste(fld$drug01id[i], 40,sep = " ")
  fld$drug01id[i][fld$d1_43[i]==1] <- paste(fld$drug01id[i], 43,sep = " ")
  fld$drug01id[i][fld$d1_46[i]==1] <- paste(fld$drug01id[i], 46,sep = " ")
  fld$drug01id[i][fld$d1_48[i]==1] <- paste(fld$drug01id[i], 48,sep = " ")
  fld$drug01id[i][fld$d1_49[i]==1] <- paste(fld$drug01id[i], 49,sep = " ")
  fld$drug01id[i][fld$d1_51[i]==1] <- paste(fld$drug01id[i], 51,sep = " ")
  fld$drug01id[i][fld$d1_53[i]==1] <- paste(fld$drug01id[i], 53,sep = " ")
  fld$drug01id[i][fld$d1_54[i]==1] <- paste(fld$drug01id[i], 54,sep = " ")
  fld$drug01id[i][fld$d1_56[i]==1] <- paste(fld$drug01id[i], 56,sep = " ")
  fld$drug01id[i][fld$d1_58[i]==1] <- paste(fld$drug01id[i], 58,sep = " ")
  fld$drug01id[i][fld$d1_60[i]==1] <- paste(fld$drug01id[i], 60,sep = " ")
  fld$drug01id[i][fld$d1_63[i]==1] <- paste(fld$drug01id[i], 63,sep = " ")
  fld$drug01id[i][fld$d1_64[i]==1] <- paste(fld$drug01id[i], 64,sep = " ")
  fld$drug01id[i][fld$d1_68[i]==1] <- paste(fld$drug01id[i], 68,sep = " ")
  fld$drug01id[i][fld$d1_69[i]==1] <- paste(fld$drug01id[i], 69,sep = " ")
  fld$drug01id[i][fld$d1_73[i]==1] <- paste(fld$drug01id[i], 73,sep = " ")
  fld$drug01id[i][fld$d1_74[i]==1] <- paste(fld$drug01id[i], 74,sep = " ")
  fld$drug01id[i][fld$d1_75[i]==1] <- paste(fld$drug01id[i], 75,sep = " ")
  fld$drug01id[i][fld$d1_76[i]==1] <- paste(fld$drug01id[i], 76,sep = " ")
  fld$drug01id[i][fld$d1_81[i]==1] <- paste(fld$drug01id[i], 81,sep = " ")
  fld$drug01id[i][fld$d1_83[i]==1] <- paste(fld$drug01id[i], 83,sep = " ")
  fld$drug01id[i][fld$d1_85[i]==1] <- paste(fld$drug01id[i], 85,sep = " ")
  fld$drug01id[i][fld$d1_86[i]==1] <- paste(fld$drug01id[i], 86,sep = " ")
  fld$drug01id[i][fld$d1_87[i]==1] <- paste(fld$drug01id[i], 87,sep = " ")
  fld$drug01id[i][fld$d1_88[i]==1] <- paste(fld$drug01id[i], 88,sep = " ")
  fld$drug01id[i][fld$d1_90[i]==1] <- paste(fld$drug01id[i], 90,sep = " ")
  fld$drug01id[i][fld$d1_91[i]==1] <- paste(fld$drug01id[i], 91,sep = " ")
  fld$drug01id[i][fld$d1_92[i]==1] <- paste(fld$drug01id[i], 92,sep = " ")
  fld$drug01id[i][fld$d1_93[i]==1] <- paste(fld$drug01id[i], 93,sep = " ")
  fld$drug01id[i][fld$d1_97[i]==1] <- paste(fld$drug01id[i], 97,sep = " ")
  fld$drug01id[i][fld$d1_101[i]==1] <- paste(fld$drug01id[i], 101,sep = " ")
  fld$drug01id[i][fld$d1_102[i]==1] <- paste(fld$drug01id[i], 102,sep = " ")
  fld$drug01id[i][fld$d1_105[i]==1] <- paste(fld$drug01id[i], 105,sep = " ")
}

for(i in 1:nrow(fld)){
  fld$drug02id[i][fld$d2_2[i]==1] <- paste(fld$drug02id[i], 2,sep = " ")
  fld$drug02id[i][fld$d2_5[i]==1] <- paste(fld$drug02id[i], 5,sep = " ")
  fld$drug02id[i][fld$d2_14[i]==1] <- paste(fld$drug02id[i], 14,sep = " ")
  fld$drug02id[i][fld$d2_17[i]==1] <- paste(fld$drug02id[i], 17,sep = " ")
  fld$drug02id[i][fld$d2_20[i]==1] <- paste(fld$drug02id[i], 20,sep = " ")
  fld$drug02id[i][fld$d2_21[i]==1] <- paste(fld$drug02id[i], 21,sep = " ")
  fld$drug02id[i][fld$d2_22[i]==1] <- paste(fld$drug02id[i], 22,sep = " ")
  fld$drug02id[i][fld$d2_23[i]==1] <- paste(fld$drug02id[i], 23,sep = " ")
  fld$drug02id[i][fld$d2_24[i]==1] <- paste(fld$drug02id[i], 24,sep = " ")
  fld$drug02id[i][fld$d2_29[i]==1] <- paste(fld$drug02id[i], 29,sep = " ")
  fld$drug02id[i][fld$d2_32[i]==1] <- paste(fld$drug02id[i], 32,sep = " ")
  fld$drug02id[i][fld$d2_33[i]==1] <- paste(fld$drug02id[i], 33,sep = " ")
  fld$drug02id[i][fld$d2_35[i]==1] <- paste(fld$drug02id[i], 35,sep = " ")
  fld$drug02id[i][fld$d2_39[i]==1] <- paste(fld$drug02id[i], 39,sep = " ")
  fld$drug02id[i][fld$d2_40[i]==1] <- paste(fld$drug02id[i], 40,sep = " ")
  fld$drug02id[i][fld$d2_43[i]==1] <- paste(fld$drug02id[i], 43,sep = " ")
  fld$drug02id[i][fld$d2_46[i]==1] <- paste(fld$drug02id[i], 46,sep = " ")
  fld$drug02id[i][fld$d2_48[i]==1] <- paste(fld$drug02id[i], 48,sep = " ")
  fld$drug02id[i][fld$d2_49[i]==1] <- paste(fld$drug02id[i], 49,sep = " ")
  fld$drug02id[i][fld$d2_51[i]==1] <- paste(fld$drug02id[i], 51,sep = " ")
  fld$drug02id[i][fld$d2_53[i]==1] <- paste(fld$drug02id[i], 53,sep = " ")
  fld$drug02id[i][fld$d2_54[i]==1] <- paste(fld$drug02id[i], 54,sep = " ")
  fld$drug02id[i][fld$d2_56[i]==1] <- paste(fld$drug02id[i], 56,sep = " ")
  fld$drug02id[i][fld$d2_58[i]==1] <- paste(fld$drug02id[i], 58,sep = " ")
  fld$drug02id[i][fld$d2_60[i]==1] <- paste(fld$drug02id[i], 60,sep = " ")
  fld$drug02id[i][fld$d2_63[i]==1] <- paste(fld$drug02id[i], 63,sep = " ")
  fld$drug02id[i][fld$d2_64[i]==1] <- paste(fld$drug02id[i], 64,sep = " ")
  fld$drug02id[i][fld$d2_68[i]==1] <- paste(fld$drug02id[i], 68,sep = " ")
  fld$drug02id[i][fld$d2_69[i]==1] <- paste(fld$drug02id[i], 69,sep = " ")
  fld$drug02id[i][fld$d2_73[i]==1] <- paste(fld$drug02id[i], 73,sep = " ")
  fld$drug02id[i][fld$d2_74[i]==1] <- paste(fld$drug02id[i], 74,sep = " ")
  fld$drug02id[i][fld$d2_75[i]==1] <- paste(fld$drug02id[i], 75,sep = " ")
  fld$drug02id[i][fld$d2_76[i]==1] <- paste(fld$drug02id[i], 76,sep = " ")
  fld$drug02id[i][fld$d2_81[i]==1] <- paste(fld$drug02id[i], 81,sep = " ")
  fld$drug02id[i][fld$d2_83[i]==1] <- paste(fld$drug02id[i], 83,sep = " ")
  fld$drug02id[i][fld$d2_85[i]==1] <- paste(fld$drug02id[i], 85,sep = " ")
  fld$drug02id[i][fld$d2_86[i]==1] <- paste(fld$drug02id[i], 86,sep = " ")
  fld$drug02id[i][fld$d2_87[i]==1] <- paste(fld$drug02id[i], 87,sep = " ")
  fld$drug02id[i][fld$d2_88[i]==1] <- paste(fld$drug02id[i], 88,sep = " ")
  fld$drug02id[i][fld$d2_90[i]==1] <- paste(fld$drug02id[i], 90,sep = " ")
  fld$drug02id[i][fld$d2_91[i]==1] <- paste(fld$drug02id[i], 91,sep = " ")
  fld$drug02id[i][fld$d2_92[i]==1] <- paste(fld$drug02id[i], 92,sep = " ")
  fld$drug02id[i][fld$d2_93[i]==1] <- paste(fld$drug02id[i], 93,sep = " ")
  fld$drug02id[i][fld$d2_97[i]==1] <- paste(fld$drug02id[i], 97,sep = " ")
  fld$drug02id[i][fld$d2_101[i]==1] <- paste(fld$drug02id[i], 101,sep = " ")
  fld$drug02id[i][fld$d2_102[i]==1] <- paste(fld$drug02id[i], 102,sep = " ")
  fld$drug02id[i][fld$d2_105[i]==1] <- paste(fld$drug02id[i], 105,sep = " ")
}

fld$BIRTHDATE <- as.Date(fld$dob, format = "%m/%d/%Y")
fld$DIAGNOSIS <- as.Date(fld$datedx, format = "%m/%d/%Y")
fld$DEATHDATE <- as.Date(fld$datedeath, format = "%m/%d/%Y")
fld$LABDATE <- as.Date(fld$lab_date, format = "%m/%d/%Y")
fld$TREAT01 <- as.Date(fld$date1, format = "%m/%d/%Y")
fld$TREAT02 <- as.Date(fld$date2, format = "%m/%d/%Y")
fld$TREAT03 <- as.Date(fld$date3, format = "%m/%d/%Y")
fld$TREAT04 <- as.Date(fld$date4, format = "%m/%d/%Y")
fld$TREAT05 <- as.Date(fld$date5, format = "%m/%d/%Y")
fld$TREAT06 <- as.Date(fld$date6, format = "%m/%d/%Y")
fld$TREAT07 <- as.Date(fld$date7, format = "%m/%d/%Y")
fld$TREAT08 <- as.Date(fld$date8, format = "%m/%d/%Y")
fld$TREAT09 <- as.Date(fld$date9, format = "%m/%d/%Y")
fld$TREAT10 <- as.Date(fld$date10, format = "%m/%d/%Y")
fld$RESPDATE01 <- as.Date(fld$resp_dt1, format = "%m/%d/%Y")
fld$RESPDATE02 <- as.Date(fld$resp_dt2, format = "%m/%d/%Y")
fld$RESPDATE03 <- as.Date(fld$resp_dt3, format = "%m/%d/%Y")
fld$RESPDATE04 <- as.Date(fld$resp_dt4, format = "%m/%d/%Y")
fld$RESPDATE05 <- as.Date(fld$resp_dt5, format = "%m/%d/%Y")
fld$RESPDATE06 <- as.Date(fld$resp_dt6, format = "%m/%d/%Y")
fld$RESPDATE07 <- as.Date(fld$resp_dt7, format = "%m/%d/%Y")
fld$RESPDATE08 <- as.Date(fld$resp_dt8, format = "%m/%d/%Y")
fld$RESPDATE09 <- as.Date(fld$resp_dt9, format = "%m/%d/%Y")
fld$RESPDATE10 <- as.Date(fld$resp_dt10, format = "%m/%d/%Y")
fld$PROGDATE01 <- as.Date(fld$prog_dt1, format = "%m/%d/%Y")
fld$PROGDATE02 <- as.Date(fld$prog_dt2, format = "%m/%d/%Y")
fld$PROGDATE03 <- as.Date(fld$prog_dt3, format = "%m/%d/%Y")
fld$PROGDATE04 <- as.Date(fld$prog_dt4, format = "%m/%d/%Y")
fld$PROGDATE05 <- as.Date(fld$prog_dt5, format = "%m/%d/%Y")
fld$PROGDATE06 <- as.Date(fld$prog_dt6, format = "%m/%d/%Y")
fld$PROGDATE07 <- as.Date(fld$prog_dt7, format = "%m/%d/%Y")
fld$PROGDATE08 <- as.Date(fld$prog_dt8, format = "%m/%d/%Y")
fld$PROGDATE09 <- as.Date(fld$prog_dt9, format = "%m/%d/%Y")
fld$PROGDATE10 <- as.Date(fld$prog_dt10, format = "%m/%d/%Y")
fld$CENSUSDATE <- as.Date(fld$census_dt, format = "%m/%d/%Y")
fld$LASTCTDATE <- as.Date(fld$last_contact_dt, format = "%m/%d/%Y")

# remove patients who transformed to DLBCL before coming to U of M
fld <- subset(fld, is.na(disease_um43) == 1)

# analyze time from diagnosis to starting treatment at U of M
fld$DIAGNOSIS <- unclass(difftime(as.Date(fld$datedx, format = "%m/%d/%Y"),as.Date("1990-01-01"), units = "days"))/365.25 + 1990
hist(fld$DIAGNOSIS, breaks = c(1980:2020), main = "Histogram of Diagnosis Dates", xlab = "Year of Diagnosis")

# analyze time from diagnosis to lab date at U of M
fld$dx2lab <- unclass(difftime(as.Date(fld$lab_date, format = "%m/%d/%Y"),as.Date(fld$datedx, format = "%m/%d/%Y"), units = "days"))/365.25
hist(fld$dx2lab, breaks = c((-5*2):(30*2))/2, main = "Histogram of Time Between Diagnosis and Lab Date", xlab = "Years Between Diagnosis and Lab Date")

fld$dx2trt <- unclass(difftime(as.Date(fld$date1, format = "%m/%d/%Y"),as.Date(fld$datedx, format = "%m/%d/%Y"), units = "days"))/365.25

# remove patients who came to U of M at least 6 months after being dignosed
fld <- subset(fld, (dx2lab <= 0.5)|(dx2trt <= 1/12)*is.na(dx2lab))

# remove patients who are missing a first treatment date (this will be my time zero)
fld <- subset(fld, date1 != "")
hist(fld$DIAGNOSIS, breaks = c(1995:2020), main = "Histogram of Diagnosis Dates", xlab = "Year of Diagnosis")

# remove patients who are missing an identifier for the first treatment
fld <- subset(fld, is.na(drug01id) != 1)

library(mice)
# Use md.pattern to find frequencies of each drug treatment
drug_nos <- c("2","5","14","17","20","21","22","23","24","29","32",
              "33","35","39","40","43","46","48","49","51","53","54",
              "56","58","60","63","64","68","69","73","74","75","76",
              "81","83","85","86","87","88","90","91","92","93","97",
              "101","102","105")
tx1_drugs <- fld[145:191]
colnames(tx1_drugs) <- drug_nos
md.pattern(tx1_drugs)
tx2_drugs <- fld[202:248]
colnames(tx2_drugs) <- drug_nos
md.pattern(tx2_drugs)
tx3_drugs <- fld[259:304]
colnames(tx3_drugs) <- drug_nos[c(1:8,10:47)]
md.pattern(tx3_drugs)
tx4_drugs <- fld[315:361]
colnames(tx4_drugs) <- drug_nos
md.pattern(tx4_drugs)
tx5_drugs <- fld[372:418]
colnames(tx5_drugs) <- drug_nos
md.pattern(tx5_drugs)
tx6_drugs <- fld[429:475]
colnames(tx6_drugs) <- drug_nos
md.pattern(tx6_drugs)
tx7_drugs <- fld[486:532]
colnames(tx7_drugs) <- drug_nos
md.pattern(tx7_drugs)
tx8_drugs <- fld[543:589]
colnames(tx8_drugs) <- drug_nos
md.pattern(tx8_drugs)
tx9_drugs <- fld[600:646]
colnames(tx9_drugs) <- drug_nos
md.pattern(tx9_drugs)
tx10_drugs <- fld[657:703]
colnames(tx10_drugs) <- drug_nos
md.pattern(tx10_drugs)

# remove the people who were on observation in the first regimen and never got another treatment
obs <- subset(fld,drug01id == "NA 102")
obs <- subset(obs,!is.na(drug02id))
obs$first_treated <- obs$TREAT02
obs$observed <- 1
obs$RandB <- obs$drug02id == "NA 32 75"
obs$Ritux <- obs$drug02id == "NA 75"
obs$RCHOP <- obs$drug02id == "NA 5 75"
obs$Radia <- obs$drug02id == "NA 91"
not_obs <- subset(fld,drug01id != "NA 102")
not_obs$first_treated <- not_obs$TREAT01
not_obs$observed <- 0
not_obs$RandB <- not_obs$drug01id == "NA 32 75"
not_obs$Ritux <- not_obs$drug01id == "NA 75"
not_obs$RCHOP <- not_obs$drug01id == "NA 5 75"
not_obs$Radia <- not_obs$drug01id == "NA 91"
data <- rbind(not_obs,obs)
data$FIRSTTRT <- as.Date(data$first_treated, format = "%m/%d/%Y")
data$age_first <- unclass(difftime(as.Date(data$first_treated, format = "%m/%d/%Y"),as.Date(data$dob, format = "%m/%d/%Y"), units = "days"))/365.25

# check the types of treatments people receive who were placed under observation in first regimen
obs2_drugs <- obs[202:248]
colnames(obs2_drugs) <- drug_nos
md.pattern(obs2_drugs)

# creating the cleaned survival dataset
# to measure overall survival
data$maxtime <- max(data$CENSUSDATE,data$LASTCTDATE)

died <- subset(data, is.na(DEATHDATE) == 0)
died$death <- 1
alive <- subset(data, is.na(DEATHDATE) == 1)
alive$death <- 0

died$time <-unclass(difftime(as.Date(died$datedeath, format = "%m/%d/%Y"),as.Date(died$first_treated, format = "%m/%d/%Y"), units = "days"))/365.25
alive$time <-unclass(difftime(alive$maxtime,as.Date(alive$first_treated, format = "%m/%d/%Y"), units = "days"))/365.25

data2 <- rbind(died,alive)

library(survival)
library(survminer)

fit <- survfit(Surv(time,death)~1, data = data2)
fit2 <- survfit(Surv(time,death)~RandB, data = data2)
model1 <- coxph(Surv(time,death)~age_first+male+RandB, data = data2)
summary(model1)
ggsurvplot(fit, data = data2,
           legend = "none",
           conf.int = T,
           risk.table = T,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_bw(),
           title = "Overall Survival from Time of First Treatment",
           xlab = "Time Since First Treatment (years)",
           xlim = c(0, 10),
           ylab = "Survival Probability",
           censor.shape = "|",
           censor.size = 3,
           break.time.by = 2)
# 5 year survival
max(fit$surv[fit$time >= 5])

ggsurvplot(fit2, data = data2,
           # surv.median.line = "v",
           legend.title = "Treatment",
           legend.labs = c("Other", "R & B"),
           risk.table = T,
           # pval = T,
           conf.int = T,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_bw(),
           title = "Overall Survival from Time of First Treatment",
           xlab = "Time Since First Treatment (years)",
           xlim = c(0,10),
           ylab = "Survival Probability",
           censor.shape = "|",
           censor.size = 3,
           break.time.by = 2)

# first get the people who progressed after first treatment
pro <- subset(data2, prog_dt1 != "")
pro1 <- subset(pro, PROGDATE01 > FIRSTTRT)
pro4 <- subset(pro, PROGDATE01 < FIRSTTRT)
not_pro5 <- subset(pro4, prog_dt2 == "")
not_pro5 <- subset(not_pro5, resp_dt2 != "")
not_pro5$progressed <- 0
not_pro5$time_pfs <- unclass(difftime(as.Date(not_pro5$resp_dt2, format = "%m/%d/%Y"),as.Date(not_pro5$first_treated, format = "%m/%d/%Y"), units = "days"))/365.25
pro4 <- subset(pro4, prog_dt2 != "")
pro4$progressed <- 1
pro4$time_pfs <- unclass(difftime(as.Date(pro4$prog_dt2, format = "%m/%d/%Y"),as.Date(pro4$first_treated, format = "%m/%d/%Y"), units = "days"))/365.25
not_pro <- subset(data2, prog_dt1 == "")
pro1$progressed <- 1
pro1$time_pfs <- unclass(difftime(as.Date(pro1$prog_dt1, format = "%m/%d/%Y"),as.Date(pro1$first_treated, format = "%m/%d/%Y"), units = "days"))/365.25
pro2 <- subset(not_pro, prog_dt2 != "")
pro2$progressed <- 1
pro2$time_pfs <- unclass(difftime(as.Date(pro2$prog_dt2, format = "%m/%d/%Y"),as.Date(pro2$first_treated, format = "%m/%d/%Y"), units = "days"))/365.25
not_pro <- subset(not_pro, prog_dt2 == "")
not_pro2 <- subset(not_pro, (date2 == "")&(resp_dt1 != "")&(resp1 != 4))
not_pro2 <- subset(not_pro2, RESPDATE01 > FIRSTTRT)
not_pro2$progressed <- 0
not_pro2$time_pfs <- unclass(difftime(as.Date(not_pro2$resp_dt1, format = "%m/%d/%Y"),as.Date(not_pro2$first_treated, format = "%m/%d/%Y"), units = "days"))/365.25
not_pro3 <- subset(not_pro, (date2 != "")|(resp_dt1 == "")|(resp1 == 4))
pro3 <- subset(not_pro3, resp1 == 4)
pro3$progressed <- 1
pro3$time_pfs <- unclass(difftime(as.Date(pro3$resp_dt1, format = "%m/%d/%Y"),as.Date(pro3$first_treated, format = "%m/%d/%Y"), units = "days"))/365.25
not_pro3 <- subset(not_pro3, resp1 != 4)
not_pro4 <- subset(not_pro3, ((resp_dt1 != "")|(date2 != ""))&(resp_dt2 != ""))
not_pro4$progressed <- 0
not_pro4$time_pfs <- unclass(difftime(as.Date(not_pro4$resp_dt2, format = "%m/%d/%Y"),as.Date(not_pro4$first_treated, format = "%m/%d/%Y"), units = "days"))/365.25

data3 <- rbind(pro1,pro2,pro3,pro4,not_pro2,not_pro4,not_pro5)

fit3 <- survfit(Surv(time_pfs,progressed)~1, data = data3)
fit4 <- survfit(Surv(time_pfs,progressed)~RandB, data = data3)
model2 <- coxph(Surv(time_pfs,progressed)~age_first+male+RandB, data = data3)
summary(model2)
ggsurvplot(fit3, data = data3,
           legend = "none",
           conf.int = T,
           risk.table = T,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_bw(),
           title = "Progression Free Survival from Time of First Treatment",
           xlab = "Time Since First Treatment (years)",
           xlim = c(0, 5),
           ylab = "Survival Probability",
           censor.shape = "|",
           censor.size = 3,
           break.time.by = 1)
# median survival time
min(fit3$time[fit3$surv <= .5])

ggsurvplot(fit4, data = data3,
           surv.median.line = "v",
           legend.title = "Treatment",
           legend.labs = c("Other", "R & B"),
           risk.table = T,
           # pval = T,
           conf.int = T,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_bw(),
           title = "Progression Free Survival from Time of First Treatment",
           xlab = "Time Since First Treatment (years)",
           xlim = c(0,5),
           ylab = "Survival Probability",
           censor.shape = "|",
           censor.size = 3,
           break.time.by = 1)

data2$transformed <- 1*(data2$dis_last_contact==4)
data2$time_trans <- unclass(difftime(as.Date(data2$last_contact_dt, format = "%m/%d/%Y"),as.Date(data2$first_treated, format = "%m/%d/%Y"), units = "days"))/365.25

fit5 <- survfit(Surv(time_trans,transformed)~1, data = data2)
fit6 <- survfit(Surv(time_trans,transformed)~RandB, data = data2)
model3 <- coxph(Surv(time_trans,transformed)~age_first+male+RandB, data = data2)
summary(model3)
ggsurvplot(fit5, data = data2,
           legend = "none",
           conf.int = T,
           risk.table = T,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_bw(),
           title = "Transformation Free Survival from Time of First Treatment",
           xlab = "Time Since First Treatment (years)",
           xlim = c(0, 10),
           ylab = "Survival Probability",
           censor.shape = "|",
           censor.size = 3,
           break.time.by = 2)
# 5 year survival
max(fit5$surv[fit5$time >= 5])

ggsurvplot(fit6, data = data2,
           # surv.median.line = "v",
           legend.title = "Treatment",
           legend.labs = c("Other", "R & B"),
           risk.table = T,
           # pval = T,
           conf.int = T,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_bw(),
           title = "Transformation Free Survival from Time of First Treatment",
           xlab = "Time Since First Treatment (years)",
           xlim = c(0,5),
           ylab = "Survival Probability",
           censor.shape = "|",
           censor.size = 3,
           break.time.by = 1)