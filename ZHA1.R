##################################################################
###### PSYP13 Sub-course 1 - ZK Assignment 1 - by Ege Şenel ######
##################################################################

# Load packages
library(tidyverse) 
library(psych)
library(olsrr)
library(car)
library(dplyr)
library(corrplot)
library(Hmisc)
library(lmtest)

# Load the data file
home_sample_1 <- read.csv("~/Desktop/Uni Stuff/Lund/PSYP13 Advanced Scientific Methods in Psychology/Zoltan/home_sample_1.csv")

# Check the data
home_sample_1 %>% 
  view() %>% 
  summary() %>% 
  str() 

# Scatter plots of the predictors
home_sample_1 %>% 	
  ggplot() +	
  aes(x = sex, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)
home_sample_1 %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)
home_sample_1 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)
home_sample_1 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)
home_sample_1 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)
home_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)
home_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)

# Cleaning the data from coding errors
home_sample_1.2 <- home_sample_1[!home_sample_1$STAI_trait <= 20, ]
home_sample_1.2 <- home_sample_1.2[!home_sample_1.2$household_income <= 0, ]

# Building a model containing age and sex as predictors of pain (model 1)
model1 <- lm(pain ~ sex + age, data = home_sample_1.2)
model1

# Building a new model with the predictors: age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures (model 2)
model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + 
               cortisol_saliva, data = home_sample_1.2)
model2

# Cook's distance plot to see and remove the outliers
ols_plot_cooksd_bar(model1)
ols_plot_cooksd_bar(model2)

## Checking for assumptions
# Diagnostic plots
x11()
par(mfcol = c(2, 2))
plot(model1, which = 1:4)

x11()
par(mfrow = c(2, 2))
plot(model2, which = 1:4)

## Normality
ols_test_normality(model1)
ols_test_normality(model2)

residuals_model1 = enframe(residuals(model1))	
residuals_model1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	
residuals_model2 = enframe(residuals(model2))	
residuals_model2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

## Linearity
residualPlots(model1)
residualPlots(model2)

## Homoscedasticity
# 1-) Non-constant variance test for checking the homogeneity of variance
ncvTest(model1)
ncvTest(model2)
# 2-) Breusch Pagan test for heteroscedasticity
bptest(model1)
bptest(model2)

## Collinearity
# 1-) Variance inflation factors
vif(model1)
vif(model2) # cortisol_serum and cortisol_saliva have high values
# 2-) Checking for correlations between predictors in model2
data_matrix <- data.matrix(home_sample_1.2)
rcorr(data_matrix)
cor.test(home_sample_1.2$cortisol_saliva, home_sample_1.2$cortisol_serum)# r = .87, p < .001
# cortisol_serum and cortisol_saliva are significantly correlated

# Model comparisons with cortisol_serum and cortisol_saliva
model_serum <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = home_sample_1.2)
model_saliva <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = home_sample_1.2)

summary(model_serum)$adj.r.squared # .40
summary(model_saliva)$adj.r.squared # .43

AIC(model_serum) # 512
AIC(model_saliva) # 503

# Despite that the model with cortisol_saliva had a better model fit, we decided to take a theory based approach and include cortisol_serum instead

# cortisol_saliva removed for collinearity
model2.1 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = home_sample_1.2)
model2.1

## Re-running the assumption tests for the new model
# Diagnostic plots
x11()
par(mfrow = c(2, 2))
plot(model2.1, which = 1:4)

## Normality
ols_test_normality(model2.1)

## Linearity
residualPlots(model2.1)

## Homoscedasticity
# 1-) Non-constant variance test for checking the homogeneity of variance
ncvTest(model2.1)
# 2-) Breusch Pagan test for heteroscedasticity
bptest(model2.1)

## Collinearity
# 1-) Variance inflation factors
vif(model2.1)
# 2-) Checking for correlations
data_matrix <- data.matrix(home_sample_1.2)
rcorr(data_matrix)

# model2.1 equation:
# = 2.854038  +  (0.491384)*sex +  (-0.044473)*age + (-0.002374)*STAI_trait + (0.077493)*pain_cat + (-0.290727)*mindfulness + (0.429763)*cortisol_serum

# Confidence intervals and standardized coefficients
confint(model1)	
lm.beta::lm.beta(model1)	

confint(model2.1)	
lm.beta::lm.beta(model2.1)	

# Tables for the coefficients and confidence intervals for both models
sm1 = summary(model1)	
sm2 =	summary(model2.1)

sm1_p_values = as.character(round(sm1$coefficients[,4], 3))	
sm1_p_values[sm1_p_values != "0" & sm1_p_values != "1"] = substr(sm1_p_values[sm1_p_values != "0" & sm1_p_values != "1"], 2, nchar(sm1_p_values[sm1_p_values != "0" & sm1_p_values != "1"]))	
sm1_p_values[sm1_p_values == "0"] = "<.001"

sm2_p_values = as.character(round(sm2$coefficients[,4], 3))	
sm2_p_values[sm2_p_values != "0" & sm2_p_values != "1"] = substr(sm2_p_values[sm2_p_values != "0" & sm2_p_values != "1"], 2, nchar(sm2_p_values[sm2_p_values != "0" & sm2_p_values != "1"]))	
sm2_p_values[sm2_p_values == "0"] = "<.001"

sm_table1 = cbind(as.data.frame(round(cbind(coef(model1), confint(model1), c(0, lm.beta::lm.beta(model1)$standardized.coefficients[c(2,3)])), 2)), sm1_p_values)	
names(sm_table1) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table1["(Intercept)","Std.Beta"] = "0"

sm_table2 = cbind(as.data.frame(round(cbind(coef(model2.1), confint(model2.1), c(0, lm.beta::lm.beta(model2.1)$standardized.coefficients[c(2,3)])), 2)), sm2_p_values)	
names(sm_table2) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table2["(Intercept)","Std.Beta"] = "0"	

sm_table1
#                 b    95%CI lb 95%CI ub Std.Beta p-value
# (Intercept)  7.83     5.96     9.70        0   <.001
# sexmale      0.64     0.19     1.10     0.21    .006
# age         -0.09    -0.13    -0.04    -0.28   <.001

sm_table2
#                    b 95%CI lb 95%CI ub Std.Beta p-value
#(Intercept)     2.85    -0.21     5.92        0    .068
#sexmale         0.49     0.10     0.89     0.16    .015
#age            -0.04    -0.09     0.00    -0.15    .043
#STAI_trait      0.00    -0.05     0.05        0    .923
#pain_cat        0.08     0.03     0.12     0.16    .001
#mindfulness    -0.29    -0.51    -0.07    -0.15    .011
#cortisol_serum  0.43     0.21     0.65        0   <.001

# Comparing the models
anova(model1, model2.1) #F(4, 151) = 19.03, p <.001

summary(model1)$adj.r.squared # .12
summary(model2.1)$adj.r.squared # .40

AIC(model1) # 569
AIC(model2.1) # 512.19

# model 2.1 wins!
