# Supplemental R Code for Project 04 (Design Question 02)
setwd("~/WORKING_DIRECTORIES/biostat.699/project04")

# Packages
library(survival)
library(survminer)
library(broom)
library(ggplot2)

# Set number of simulations
sims <- 10000

# Set exponential distribution
lambda <- 49.06266 # in order to achieve median at 28 days

# Set sample size
N <- c(1:60)*5 # Sample sizes from 5 to 300 in 5 unit increments

# Set effect sizes
p_smalleff <- 0.4 # 40% get aGVHD
p_middleff <- 0.35 # 35% get aGVHD
p_largeeff <- 0.3 # 30% get aGVHD

# Initialize power storage variables
power_small <- vector()
power_middl <- vector()
power_large <- vector()

for(j in 1:length(N)){
  
  # Initialize significance storage variables
  sig_small <- vector()
  sig_middl <- vector()
  sig_large <- vector()
  
  for(i in 1:sims){
    # create survival dataset for small effect of potato starch
    samp <- runif(N[j],0,1)
    aGVHD <- samp < p_smalleff
    time <- rep(110,N[j])
    time[aGVHD] <- -lambda*log(1-runif(sum(aGVHD),0,1)*(1-exp(-100/lambda)))
    smallfit <- survfit(Surv(time,aGVHD)~1)
    small_upper <- summary(smallfit,times=100)$upper
    small_lower <- summary(smallfit,times=100)$lower
    sig_small[i] <- small_lower > 0.5 | small_upper < 0.5
    # create survival dataset for medium effect of potato starch
    samp <- runif(N[j],0,1)
    aGVHD <- samp < p_middleff
    time <- rep(110,N[j])
    time[aGVHD] <- -lambda*log(1-runif(sum(aGVHD),0,1)*(1-exp(-100/lambda)))
    middlfit <- survfit(Surv(time,aGVHD)~1)
    middl_upper <- summary(middlfit,times=100)$upper
    middl_lower <- summary(middlfit,times=100)$lower
    sig_middl[i] <- middl_lower > 0.5 | middl_upper < 0.5
    # create survival dataset for large effect of potato starch
    samp <- runif(N[j],0,1)
    aGVHD <- samp < p_largeeff
    time <- rep(110,N[j])
    time[aGVHD] <- -lambda*log(1-runif(sum(aGVHD),0,1)*(1-exp(-100/lambda)))
    largefit <- survfit(Surv(time,aGVHD)~1)
    large_upper <- summary(largefit,times=100)$upper
    large_lower <- summary(largefit,times=100)$lower
    sig_large[i] <- large_lower > 0.5 | large_upper < 0.5
  }
  power_small[j] <- mean(sig_small)
  power_middl[j] <- mean(sig_middl)
  power_large[j] <- mean(sig_large)
}

# Plot the power curve
power_data <- data.frame(rep(N,3),c(power_small,power_middl,power_large),rep(c(1:3),each=length(N)))
names(power_data) <- c("Size","Power","Effect")
ggplot(data = power_data, aes(x=Size, y=Power, color=factor(Effect))) +
  geom_point() +
  geom_smooth(se=FALSE) +
  labs(title = "Power Curve for Three Potato Starch Effect Sizes",
       x = "Sample Size",
       y = "Power") +
  scale_color_discrete(name = "Effect Size", labels = c("40% aGVHD","35% aGVHD","30% aGVHD")) +
  geom_hline(yintercept = 0.8) + 
  theme_bw()

# https://resourcetepee.com/free-statistical-calculators/single-arm-survival-sample-size/
# With alpha = 0.05 (two-sided) and power = 0.80
# for detecting a difference between 50% and 40% we need a sample size of approximately 167
# for detecting a difference between 50% and 35% we need a sample size of approximately 70
# for detecting a difference between 50% and 30% we need a sample size of approximately 36