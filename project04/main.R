# BIOSTAT 699 Project 04 - Simulation Study
setwd("~/WORKING_DIRECTORIES/biostat.699/project04")

# Set sample size
N <- 100
p <- 0.4
samp <- runif(N,0,1)
aGVHD <- samp < p
time <- rep(110,N)
time[aGVHD] <- -lambda*log(1-runif(sum(aGVHD),0,1)*(1-exp(-100/lambda)))
data <- data.frame(time,aGVHD)
fit <- survfit(Surv(time,aGVHD)~1)
ggsurvplot(fit, data = data,
           legend = "none",
           xlim = c(0,100))
summary(fit$time[fit$n.event==1&fit$time<100])
min(fit$upper[fit$time<100])
min(fit$surv[fit$time<100])
min(fit$lower[fit$time<100])

# Determine the best scale and shape parameter with the Weibull distribution
shape <- c(500:1500)/1000
scale <- 28/(-log(1-0.5))^(1/shape)

upper_q <- vector()
lower_q <- vector()
over100 <- vector()
for(i in 1:length(shape)){
  time[aGVHD] <- rweibull(sum(aGVHD),shape = shape[i],scale = scale[i])
  upper_q[i] <- quantile(time[aGVHD])[4]
  lower_q[i] <- quantile(time[aGVHD])[2]
  over100[i] <- sum(time[aGVHD]<100)/sum(aGVHD)
}

upper_error <- upper_q-56
lower_error <- lower_q-7
total_error <- upper_error^2+lower_error^2
shape[total_error==min(total_error)]
scale[total_error==min(total_error)]

up_new <- upper_q*over100^2
lo_new <- lower_q*over100
library(ggplot2)
dist <- data.frame(rep(shape,2),rep(scale,2),c(upper_q,lower_q),rep(c(0,1),each=1001))
colnames(dist) <- c("Shape","Scale","Value","Quartile")
plot01 <- ggplot(data = dist, aes(x=Shape,y=Value)) +
  geom_point(aes(color = factor(Quartile))) +
  scale_color_discrete(name="Quartile",labels=c("Upper","Lower")) +
  geom_hline(yintercept = c(7,56)) +
  geom_vline(xintercept = 1)
plot01

# Determine which initial proportions give us aGVHD rates of exactly 50%, 40%, 35%, and 30%
N <- 100000
# Set aGVHD threshold
p <- c(30:60)/100
surv <- vector()
for(i in 1:length(p)){
  samp <- runif(N,0,1)
  aGVHD <- samp < p[i]
  # Assign survival times
  time <- rep(110,N)
  time[aGVHD] <- -lambda*log(1-runif(sum(aGVHD),0,1)*(1-exp(-100/lambda)))
  summary(time[aGVHD])
  # Simulate 
  data <- data.frame(time,aGVHD)
  fit <- survfit(Surv(time,aGVHD)~1)
  # ggsurvplot(fit, data = data,
  #            legend = "none",
  #            xlim = c(0,100))
  summary(fit$time[fit$n.event==1&fit$time<100])
  min(fit$upper[fit$time<100])
  min(fit$lower[fit$time<100])
  surv[i] <- min(fit$surv[fit$time<100])
}

pest <- lm(p ~ surv)
survnew <- c(0.5,0.6,0.65,0.7)
pnew <- pest$coefficients[1] + pest$coefficients[2]*survnew

