# answers homework 9.

# building in the EPESE.

library (survival)
epese <-read.csv("P:/cfp/Documents/Documents/BIOMETRY/longitudinal data analysis 2022/data/epese_bmi2.csv")

attach(epese)

nrow(epese)
head(epese)

head(epese)
mean(age_at_death)

#correction - age_at_death_70 needs to have 70 subtracted. correction below no correct
epese$age_at_death_70<-epese$age_at_death-70


summary(epese$age_at_death)
summary(epese$age_at_death_70)

# look at the basic model for smoking.
##################   Question 1 & 2 - time to death  #######################


#####  KM stuff.
survdiff(Surv(age_at_death, dead==1)~smoke_at_baseline,  data=epese)

# for plotting
KM0_time <- survfit(Surv(age_at_death_70, dead==1)~smoke_at_baseline,  type="kaplan-meier", conf.type="none", data=epese)
summary (KM0_time)

# look at survival
plot(KM0_time, ylab="Survival Probability", xlab="age")

#check proportionality assumption
plot(KM0_time, lty=2:3, fun="cloglog", 
     xlab="logMonths", ylab="log(-(log(S))", main='Log-log(S) for smoking') 


###### notice above i used the age-70 variable to assist in plotting.



#Cox Model - w/out  left censoring.
Cox_left_censor1 <- coxph (Surv(age_at_death, dead==1)~as.factor(smoke_at_baseline)  , method="breslow", na.action=na.exclude, data=epese)
summary (Cox_left_censor1)
zph.fit1 <- cox.zph(Cox_left_censor1, transform = 'log')
print(zph.fit1)
plot(zph.fit1[1])
abline(h=0, lty=3)

diff<-epese$age_at_death-epese$age
summary(diff)


# if you think that smoking did not meet the proportionality assumption, you could add a 'time'*smoking interaction.

Cox_left_censor1 <- coxph (Surv(age_at_death_70, dead==1)~as.factor(smoke_at_baseline)*age_at_death_70  , 
                           method="breslow", na.action=na.exclude, data=epese)
summary (Cox_left_censor1)








# not asked in the question, but look at time on study.
#again, need to add a small number for time to event=0
# 1 KM model

KM0_time2 <- survfit(Surv(years_to_death+.01, dead==1)~smoke_at_baseline,  type="kaplan-meier", conf.type="none", data=epese)
summary (KM0_time2)

# 2 PH model
Cox_time1 <- coxph (Surv(years_to_death+0.01, dead==1)~as.factor(smoke_at_baseline)  , method="breslow", na.action=na.exclude, data=epese)
summary (Cox_time1)
zph.fit1 <- cox.zph(Cox_time1, transform = 'log')
print(zph.fit1)
plot(zph.fit1[1])
abline(h=0, lty=3)


# question 2 - incorporating left censoring.

# note.  i need to add a small number to age_at_death to allow for people who die, within the year of interview.
# see diff above.

#Cox Model - w left censoring.
Cox_time2 <- coxph (Surv(age,age_at_death+0.01, dead==1)~as.factor(smoke_at_baseline)  , method="breslow", na.action=na.exclude, data=epese)
summary (Cox_time2)
zph.fit2 <- cox.zph(Cox_time2, transform = 'log')
print(zph.fit2)

plot(zph.fit2[1])
abline(h=0, lty=3)


# Q3 looking at smoking.    PROPORTIONALITY (already done throughout, but here it is again.)
#Cox Model 
Cox_age2 <- coxph (Surv((age),age_at_death+0.01, dead==1)~as.factor(smoke_at_baseline)  , method="breslow", na.action=na.exclude, data=epese)
summary (Cox_age2)

# proportionality for smoking
# for plotting

KM0_smoke <- survfit(Surv(age_at_death, dead==1)~as.factor(smoke_at_baseline), type="kaplan-meier", 
                         conf.type="none", data=epese)
summary (KM0_smoke)


# look at survival
plot(KM0_smoke, xlab="Time", ylab="Survival Probability", main='K-M survival Smoking')

#check proportionality assumption
plot(KM0_smoke, lty=2:3, fun="cloglog", 
     xlab="logMonths", ylab="log(-(log(S))") 






# if you think that smoking did not meet the proportionality assumption, you could add a 'time'*smoking interaction.

Cox_left_censor1 <- coxph (Surv(age_at_death_70, dead==1)~as.factor(smoke_at_baseline)*age_at_death_70  , 
                           method="breslow", na.action=na.exclude, data=epese)
summary (Cox_left_censor1)










summary(age)
summary(age_at_death)



##################  Question 4:  CHECK FOR RACE ###############################




# for plotting
KM0_race <- survfit(Surv(age_at_death_70, dead==1)~white, type="kaplan-meier", conf.type="none", data=epese)
summary (KM0_race)


# look at survival
plot(KM0_race, xlab="Time", ylab="Survival Probability", main='K-M survival Race')

#check proportionality assumption
plot(KM0_race, lty=2:3, fun="cloglog", 
     xlab="logMonths", ylab="log(-(log(S))") 


summary(age)
summary(age_at_death)

#Cox Model 
Cox_race <- coxph (Surv(age, age_at_death+.01, dead==1)~as.factor(white)  , method="breslow", na.action=na.exclude, data=epese)
summary (Cox_race)
zph.fit.race <- cox.zph(Cox_race, transform = 'log')
print(zph.fit.race)

plot(zph.fit.race[1])
abline(h=0, lty=3)


# question 5: use left and right censoring. Does BMI matter  

# look for missing values
library(pastecs)
stat.desc(epese,desc=F)

# BMI in groups
Cox_all_nobmi <- coxph (Surv(age, (age_at_death+.01 ), dead==1)~as.factor(sex) +  years_ed + heart_attack + stroke + cancer + high_bp+ 
                    self_rated_health +smoke_at_baseline +num_rx + katz +cog_imp + depressed_yn  +  white,            
                  method="breslow", na.action=na.exclude, data=epese)
summary (Cox_all_nobmi)

Cox_all <- coxph (Surv(age, (age_at_death+.01 ), dead==1)~as.factor(sex) +  years_ed + heart_attack + stroke + cancer + high_bp+ 
                    self_rated_health +smoke_at_baseline +num_rx + katz +cog_imp + depressed_yn  + as.factor(bmi_gp) + white,            
                  method="breslow", na.action=na.exclude, data=epese)
summary (Cox_all)
anova (Cox_all,Cox_all_nobmi)

# therefore bmi category doesnt matter


#bmi linear

Cox_bmi_check2<- coxph (Surv(age, (age_at_death+0.01), dead==1)~as.factor(sex) +  years_ed + heart_attack + stroke + cancer + high_bp+ 
                          self_rated_health +smoke_at_baseline +num_rx + katz +cog_imp + depressed_yn   + white + bmi ,            
                        method="breslow", na.action=na.exclude, data=epese)
summary (Cox_bmi_check2)




Cox_bmi_check3<- coxph (Surv(age, (age_at_death+0.01), dead==1)~as.factor(sex) +  years_ed + heart_attack + stroke + cancer + high_bp+ 
                          self_rated_health +smoke_at_baseline +num_rx + katz +cog_imp + depressed_yn   + white + bmi + bmi_sq,            
                        method="breslow", na.action=na.exclude, data=epese)
summary (Cox_bmi_check3)

# check if bmi & bmiSQ matter on 2 df, relative to model with no bmi
anova(Cox_bmi_check3,Cox_all_nobmi)



anova(Cox_Cox_bmi_check2, Cox_bmi_check3)


# Question 7 - trading 'time to death' in for 'age at death'

Cox_time_to_full <- coxph (Surv(years_to_death+0.01, dead==1)~age+as.factor(sex) +  years_ed + heart_attack + stroke + cancer + high_bp+ 
                             self_rated_health +smoke_at_baseline +num_rx + katz +cog_imp + depressed_yn  + bmi + white,            
                           method="breslow", na.action=na.exclude, data=epese)
summary (Cox_time_to_full)



Cox_age <- coxph (Surv(age, age_at_death+0.01, dead==1)~ as.factor(sex) +  years_ed + heart_attack + stroke + cancer + high_bp+ 
                             self_rated_health +smoke_at_baseline +num_rx + katz +cog_imp + depressed_yn  + bmi + white,            
                           method="breslow", na.action=na.exclude, data=epese)
summary (Cox_age)



Cox_time_to_reduced <- coxph (Surv(years_to_death+0.01, dead==1)~as.factor(sex) +  years_ed + heart_attack + stroke + cancer + high_bp+ 
                                self_rated_health +smoke_at_baseline +num_rx + katz +cog_imp + depressed_yn  +  white,            
                              method="breslow", na.action=na.exclude, data=epese)
summary (Cox_time_to_reduced)

anova(Cox_time_to_full,Cox_time_to_reduced)

