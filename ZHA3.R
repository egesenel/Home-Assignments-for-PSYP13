##################################################################
###### PSYP13 Sub-course 1 - ZK Assignment 3 - by Ege Şenel ######
##################################################################

# Load packages
library(lsr)
library(psych) 
library(tidyverse)
library(lm.beta)
library(gridExtra)
library(dplyr)
library(car)
library(olsrr)
library(Hmisc)
library(leaps)
library(MASS)
library(lme4)
library(insight)
library(sjstats)
library(influence.ME)
library(r2glmm)
library(cAIC4)
library(lmerTest)

# Load data
home_sample_3 <- read.csv("~/Desktop/Uni Stuff/Lund/PSYP13 Advanced Scientific Methods in Psychology/Zoltan/home_sample_3.csv")
home_sample_4 <- read.csv("~/Desktop/Uni Stuff/Lund/PSYP13 Advanced Scientific Methods in Psychology/Zoltan/home_sample_4.csv")

# Cleaning the data files
summary(home_sample_3)

home_sample_3 <- home_sample_3 %>% 	
  mutate(sex = droplevels(replace(sex, sex == "Female", "female")))

home_sample_3 <- home_sample_3[!home_sample_3$mindfulness >6, ]

summary(home_sample_4)

# Creating the mixed model with random intercept of hospital ID
mixed_model <- lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + 
                      cortisol_serum + (1|hospital), data = home_sample_3, REML = FALSE)
summary(mixed_model)
coef(mixed_model)

# Confidence intervals for the mixed model
confint(mixed_model)

# Mixed model diagnosis
# Influental outliers
influence_observation = influence(mixed_model, obs = T)$alt.fixed # this can take a minute or so	
influence_group = influence(mixed_model, group = "hospital")$alt.fixed	

data_plot_influence = as_tibble(influence_group) %>% 	
  gather(colnames(influence_group), value = coefficient, key = predictor)

data_plot_influence %>% 	
  ggplot() +	
  aes(x = 1, y = coefficient, group = predictor) +	
  geom_violin() +	
  facet_wrap( ~ predictor, scales = "free")	

# Normality
qqmath(mixed_model, id=0.05)	
qqmath(ranef(mixed_model))	

# Linearity
plot(mixed_model, arg = "pearson")
# Scatterplots
home_sample_3 = home_sample_3 %>% 
  mutate(resid = residuals(mixed_model))

home_sample_3 %>% 	
  ggplot() +	
  aes(x = age, y = resid) +	
  geom_point()	

home_sample_3 %>% 	
  ggplot() +	
  aes(x = sex, y = resid) +	
  geom_point()	

home_sample_3 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = resid) +	
  geom_point()	

home_sample_3 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = resid) +	
  geom_point()	

home_sample_3 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = resid) +	
  geom_point()

home_sample_3 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva, y = resid) +	
  geom_point()

# Homoscedasticity
plot(mixed_model, arg = "pearson")

homosced_mod = lm(resid^2 ~ hospital, data = home_sample_3)	
summary(homosced_mod)	

IQR_of_residuals_by_hospital = sapply(split(home_sample_3, f = home_sample_3$hospital), function(x) IQR(x$resid))	
# rank ordering them	
rank = rank(IQR_of_residuals_by_hospital)	
# adding rank to the dataframe containing the residuals	
home_sample_3$rank = rep(rank, each = length(c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5",	"hospital_6",	"hospital_7", 'hospital_8', 'hospital_9', 'hospital_10')))	
# creating a vector of participant IDs ordered based on the rank, this will be used as labels	
IDforplot = unique(home_sample_3$hospital[order(home_sample_3$rank)])	

ggplot(home_sample_3, aes(y = resid, x = factor(rank), labels = hospital))+	
  geom_boxplot()+	
  scale_x_discrete(labels=IDforplot)+	
  coord_flip()

# create the plot	
ggplot(data_wound_long, aes(y = resid, x = factor(rank), labels = ID))+	
  geom_boxplot()+	
  scale_x_discrete(labels=IDforplot)+	
  coord_flip()	

# Multicollinearity

pairs.panels(home_sample_3[,c("age", "sex", "STAI_trait", "mindfulness", 'cortisol_saliva', 'pain_cat')], col = "red", lm = T, stars = T)	

# Computing the marginal and conditional r2 values
library(insight)
vars <- insight::get_variance(mixed_model)
r2_marginal <- vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual)
r2_marginal # 0.37 (indicates how much of the "model variance" is explained by the fixed effects)
r2_conditional <- (vars$var.fixed + vars$var.random) / (vars$var.fixed + vars$var.random + vars$var.residual)
r2_conditional # 0.45 (indicates how much of the "model variance" is explained by the fixed and random effect terms combined)
icc_conditional <- vars$var.random / (vars$var.random + vars$var.residual)
icc_conditional # 0.08 how much of the model variance is explained by random effects
(vars$var.residual/(vars$var.fixed + vars$var.random + vars$var.residual)) # 0.55
vars$var.fixed # 0.86
vars$var.random # 0.18
vars$var.residual # 1.28

# Alternative way of doing the same thing
library(sjstats)
performance::r2(mixed_model)

# Another way of finding r2 values with confidence intervals
r2beta(mixed_model, method = "nsj", data = home_sample_3)

# Likelihood Ratio test for reporting significance
mod_null <- lmer(pain ~ 1 + (1|hospital), data = home_sample_3, REML = FALSE)

chisq = round(anova(mod_null, mixed_model)$Chisq[2], 2)		
chisq_p = round(anova(mod_null, mixed_model)$Pr[2], 3)		
chisq_df = anova(mod_null, mixed_model)[2,"Chi Df"]	
R2 = round(as.data.frame(r2beta(mixed_model, method = "nsj", data = home_sample_3))[1,"Rsq"], 4)		
R2ub = round(as.data.frame(r2beta(mixed_model, method = "nsj", data = home_sample_3))[1,"upper.CL"], 2)		
R2lb = round(as.data.frame(r2beta(mixed_model, method = "nsj", data = home_sample_3))[1,"lower.CL"], 2)		
anova(mod_null, mixed_model)

cAIC(mod_null)
cAIC(mixed_model)

# Use the regression equation obtained on data file 3 to predict pain in data file 4
Mixed_predict <- predict(mixed_model, home_sample_4, allow.new.levels = TRUE)
Null_predict <- predict(mod_null, home_sample_4, allow.new.levels = TRUE )

# Compute the variance explained by the model on data file 4
RSS = sum((home_sample_4$pain - Mixed_predict)^2)	
TSS = sum((home_sample_4$pain - Null_predict)^2)

R2_new_data = 1 - (RSS/TSS)	

# Compare this R2 to the marginal and conditional R2 values computed for the model on data file 3
R2_new_data # .31
r2_marginal # .37
r2_conditional # .45
# R2 is closer to r2_marginal then r2_conditional

# Reportable results
# Marginal R squared	
r2beta(mixed_model, method = "nsj", data = home_sample_3)	

# Conditional AIC	
cAIC(mixed_model)$caic	

# Model coefficients	
summary(mixed_model)	

# Confidence intervals for the coefficients	
confint(mixed_model)	

# standardized Betas	
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

stdCoef.merMod(mixed_model)	

# cortisol_serum was picked as the most influental predictors

# Bulding the random slope model
mixed_slope <- lmer(pain ~ cortisol_serum + (1|hospital) + (cortisol_serum|hospital), data = home_sample_3)

mixed_intercept <- lmer(pain ~ cortisol_serum + (1|hospital), data = home_sample_3)

# Plot the intercept and the slope predictions
home_sample_3 = home_sample_3 %>% 		
  mutate(pred_int = predict(mixed_intercept),
         pred_slope = predict(mixed_slope)) 

home_sample_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line(color='red', aes(y=pred_int, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)

home_sample_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)

# Compare random slope to random intercept
cAIC(mixed_slope)$caic
cAIC(mixed_intercept)$caic	
anova(mixed_intercept, mixed_slope)
r2beta(mixed_slope, method = "nsj", data = home_sample_3)
