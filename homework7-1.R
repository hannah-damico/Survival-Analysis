

# read in the data BE SURE TO CHANGE DIRECTORY BASED ON WHERE YOU PLACE THE FILE.
# NOTE THE DOUBLE SLASHES

epese_data <- read.csv("./epese_bmi2-1.csv")

head (epese_data)
summary(epese_data[, 2:25])                  # summary statistics on age time homework 2 data set.


# reverse code dead to deal with censor
epese_data$censor = -1 * (epese_data$dead - 1)

library(survival)

#look at the  survival by  smoking group



surv_probs <-
  survfit(Surv(age_at_death, dead) ~ as.factor(smoke_at_baseline), data = epese_data)

# check the log rank statistic
survdiff(Surv(age_at_death, dead) ~ as.factor(smoke_at_baseline), data = epese_data)

# plot

plot (
  surv_probs,
  main = "K-M estimates for smoking baseline, EPESE",
  conf.int = FALSE,
  xlab = "years",
  ylab = "survival function"
)
summary(surv_probs)



#question 1: Does smoking matter PHreg?

fit1.survival <-
  coxph(
    Surv(age_at_death, dead) ~ smoke_at_baseline,
    method = "breslow",
    na.action = na.exclude,
    data = epese_data
  )
summary (fit1.survival)

plot(survfit(fit1.survival, fun = "cloglog"))



# question 2: check if proportionality is met

# smoke1 <- subset(epese_data, smoke_at_baseline == 1)
# smoke2 <- subset(epese_data, smoke_at_baseline == 2)
# fit1 <- summary(survfit(Surv(age_at_death, dead) ~ smoke_at_baseline,
#                       na.action=na.exclude,  data=smoke1))
# fit2<- summary(survfit(Surv(age_at_death, dead) ~ smoke_at_baseline,
na.action = na.exclude,  data = smoke2))

# check of log-log survival plot for smoking.


surv_probs <-
  survfit(Surv(age_at_death, dead) ~ as.factor(smoke_at_baseline), data = epese_data)
surv_probs2 <-
  (surv_probs)    #Can also use summary(surv_probs). Removes censored values but also removes final values
surv_probs2$lls_surv <- log(-log(surv_probs2$surv))
surv_probs2$time <- log(surv_probs2$time)
log_prob <-
  as.data.frame(cbind(surv_probs2$lls_surv, surv_probs2$time))
names(log_prob) <- c('surv', 'time')
log_prob$smoke = NA
x <- max(as.numeric(rownames(log_prob[log_prob$time == 0, ])))
log_prob$smoke[1:x - 1] = 1
log_prob$smoke[x:nrow(log_prob)] = 2

plot (
  log_prob$time[log_prob$smoke == 1],
  log_prob$surv[log_prob$smoke == 1],
  type = 'p',
  col = c(4),
  xlim = c(0, 4),
  ylim = c(-6.5, 1),
  main = "Log of -log of estimated surv func: Smoking at Baseline",
  xlab = "log(age at death) -70",
  ylab = "log(-log(survival function))"
)
par(new = T)
plot (
  log_prob$time[log_prob$smoke == 2],
  log_prob$surv[log_prob$smoke == 2],
  type = 'p',
  col = c(2),
  xlim = c(0, 4),
  ylim = c(-6.5, 1),
  ylab = '',
  xlab = ''
)
lines(
  log_prob$time[log_prob$smoke == 1],
  log_prob$surv[log_prob$smoke == 1],
  type = 'l',
  col = c(4),
  xlim = c(0, 4),
  ylim = c(-6.5, 1),
  ylab = '',
  xlab = ''
)
lines(
  log_prob$time[log_prob$smoke == 2],
  log_prob$surv[log_prob$smoke == 2],
  type = 'l',
  col = c(2),
  xlim = c(0, 4),
  ylim = c(-6.5, 1),
  ylab = '',
  xlab = ''
)




# Question 3: race effect



fit3.survival <- coxph(
  Surv(age_at_death, dead) ~ white,
  method = "breslow",
  na.action = na.exclude,
  data = epese_data
)
summary (fit3.survival)


#plot survival curves

surv_probs <-
  survfit(Surv(age_at_death, dead) ~ as.factor(white), data = epese_data)
surv_probs2 <-
  (surv_probs)    #Can also use summary(surv_probs). Removes censored values but also removes final values
prob <- as.data.frame(cbind(surv_probs2$surv, surv_probs2$time))
names(prob) <- c('surv', 'time')
prob$white = NA
prob

prob$white[1:36] = 0
prob$white[37:70] = 1

plot (
  prob$time[prob$white == 1],
  prob$surv[prob$white == 1],
  type = 'p',
  col = c(4),
  xlim = c(0, 38),
  ylim = c(0, 1),
  main = " survival func: race",
  xlab = "age at death -70",
  ylab = "survival probability"
)
par(new = T)
plot (
  prob$time[prob$white == 0],
  prob$surv[prob$white == 0],
  type = 'p',
  col = c(2),
  xlim = c(0, 38),
  ylim = c(0, 1),
  ylab = '',
  xlab = ''
)
lines(
  prob$time[prob$white == 1],
  prob$surv[prob$white == 1],
  type = 'l',
  col = c(4),
  xlim = c(0, 38),
  ylim = c(0, 1),
  ylab = '',
  xlab = ''
)
lines(
  prob$time[prob$white == 0],
  prob$surv[prob$white == 0],
  type = 'l',
  col = c(2),
  xlim = c(0, 38),
  ylim = c(0, 1),
  ylab = '',
  xlab = ''
)



# check of log-log survival plot for race.



surv_probs <-
  survfit(Surv(age_at_death, dead) ~ as.factor(white), data = epese_data)
surv_probs2 <-
  (surv_probs)    #Can also use summary(surv_probs). Removes censored values but also removes final values
surv_probs2$lls_surv <- log(-log(surv_probs2$surv))
surv_probs2$time <- log(surv_probs2$time)
log_prob <-
  as.data.frame(cbind(surv_probs2$lls_surv, surv_probs2$time))
names(log_prob) <- c('surv', 'time')
log_prob$white = NA
x <- max(as.numeric(rownames(log_prob[log_prob$time == 0, ])))
log_prob$white[1:x - 1] = 1
log_prob$white[x:nrow(log_prob)] = 0

plot (
  log_prob$time[log_prob$white == 1],
  log_prob$surv[log_prob$white == 1],
  type = 'p',
  col = c(4),
  xlim = c(0, 4),
  ylim = c(-6.5, 1),
  main = "Log of -log of estimated surv func: race",
  xlab = "log(age at death) -70",
  ylab = "log(-log(survival function))"
)
par(new = T)
plot (
  log_prob$time[log_prob$white == 0],
  log_prob$surv[log_prob$white == 0],
  type = 'p',
  col = c(2),
  xlim = c(0, 4),
  ylim = c(-6.5, 1),
  ylab = '',
  xlab = ''
)
lines(
  log_prob$time[log_prob$white == 1],
  log_prob$surv[log_prob$white == 1],
  type = 'l',
  col = c(4),
  xlim = c(0, 4),
  ylim = c(-6.5, 1),
  ylab = '',
  xlab = ''
)
lines(
  log_prob$time[log_prob$white == 0],
  log_prob$surv[log_prob$white == 0],
  type = 'l',
  col = c(2),
  xlim = c(0, 4),
  ylim = c(-6.5, 1),
  ylab = '',
  xlab = ''
)



# question 4 - PH model for all predictors;

fit4.survival <-
  coxph(
    Surv(age_at_death, dead) ~ sex + years_ed  + heart_attack +  stroke +  cancer +
      high_bp  + self_rated_health  + smoke_at_baseline +
      num_rx  + katz  + cog_imp  + depressed_yn  + white + as.factor(bmi_cat),
    method = "breslow",
    na.action = na.exclude,
    data = epese_data
  )
summary (fit4.survival)











# model without BMI group to check if 4 dummies are significant

fit4.survival_nobmi <-
  coxph(
    Surv(age_at_death, dead) ~ sex + years_ed + heart_attack + stroke + cancer +
      high_bp + self_rated_health + smoke_at_baseline +
      num_rx + katz + cog_imp + depressed_yn + white ,
    method = "breslow",
    na.action = na.exclude,
    data = epese_data
  )


# difference in fit without bmi group
anova (fit4.survival, fit4.survival_nobmi)

# smoking by race interaction

fit2.survival <-
  coxph(
    Surv(age_at_death, dead) ~ sex + years_ed + heart_attack + stroke + cancer +
      high_bp + self_rated_health + smoke_at_baseline * white +
      num_rx + katz + cog_imp + depressed_yn  +
      bmi_gp1 + bmi_gp3 + bmi_gp4 + bmi_gp5,
    method = "breslow",
    na.action = na.exclude,
    data = epese_data
  )
summary (fit2.survival)




# question 5 - SEE SAS OUTPUT


# question 6 - YEARS to DEATH vs. AGE AT DEATH


fit6.survival <-
  coxph(
    Surv(years_to_death, dead) ~ sex + years_ed  + heart_attack +  stroke +  cancer +
      high_bp  + self_rated_health  + smoke_at_baseline +
      num_rx  + katz  + cog_imp  + depressed_yn  + white + as.factor(bmi_cat) + age,
    method = "breslow",
    na.action = na.exclude,
    data = epese_data
  )
summary (fit6.survival)
