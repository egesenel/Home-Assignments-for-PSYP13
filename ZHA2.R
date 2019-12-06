##################################################################
###### PSYP13 Sub-course 1 - ZK Assignment 2 - by Ege Şenel ######
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

# Remove the participants that were excluded in assignment 1
home_sample_1.2 <- home_sample_1[!home_sample_1$STAI_trait <= 20, ]
home_sample_1.2 <- home_sample_1.2[!home_sample_1.2$household_income <= 0, ]

# Create a linear model with every variable except cortisol_saliva
model3 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + 
               IQ + household_income, data = home_sample_1.2)
model3

summary(model3)

## Run diagnostics
# Plots
x11()
par(mfrow = c(2, 2))
plot(model3, which = 1:4)

## Normality
ols_test_normality(model3)

## Linearity
residualPlots(model3)

# Checking for linearity again
residualPlots(model3)

## Homoscedasticity
# 1-) Non-constant variance test for checking the homogeneity of variance
ncvTest(model3)
# 2-) Breusch Pagan test for heteroscedasticity
bptest(model3)

## Collinearity
# 1-) Variance inflation factors
vif(model3)
# 2-) Checking for correlations
data_matrix <- data.matrix(home_sample_1.2)
rcorr(data_matrix)

# Backward elimination
step(object = model3, direction = 'backward')
step(model3)

# STAI_trait, household_income and IQ were removed through backward elimination
model3.1 <- lm(pain ~ sex + age + pain_cat + mindfulness + cortisol_serum + 
                 weight, data = home_sample_1.2)

## Run diagnostics
# Plots
x11()
par(mfrow = c(2, 2))
plot(model3.1, which = 1:4)

## Normality
ols_test_normality(model3.1)

## Linearity
residualPlots(model3.1)

## Homoscedasticity
# 1-) Non-constant variance test for checking the homogeneity of variance
ncvTest(model3.1)
# 2-) Breusch Pagan test for heteroscedasticity
bptest(model3.1)

## Collinearity
# 1-) Variance inflation factors
vif(model3.1)
# 2-) Checking for correlations
data_matrix <- data.matrix(home_sample_1.2)
rcorr(data_matrix)

# Comparing the backward eliminated model to the initial one
anova(model3.1, model3)

summary(model3)$adj.r.squared
summary(model3.1)$adj.r.squared

AIC(model3)
AIC(model3.1) 


# Comparing the theoretical and backward models
BackwardModel <- lm(pain ~ sex + age + pain_cat + mindfulness + cortisol_serum + 
                      weight, data = home_sample_1.2)
TheoreticalModel <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + 
                         cortisol_serum, data = home_sample_1.2)

summary(TheoreticalModel) # r2 = .40
summary(BackwardModel) # r2 = .41

AIC(TheoreticalModel) # 512.18
AIC(BackwardModel) # 508.74

# Comparing the predictive power of the theoretical and the backward models on a new sample
home_sample_2 <- read.csv("~/Desktop/Uni Stuff/Lund/PSYP13 Advanced Scientific Methods in Psychology/Zoltan/home_sample_2.csv")

# Theoretical_equation = 2.854038  +  (0.491384)*sex +  (-0.044473)*age + (-0.002374)*STAI_trait + (0.077493)*pain_cat + (-0.290727)*mindfulness + (0.429763)*cortisol_serum
# Backward_equation = 4.21180 + (0.47307)*sex + (-0.04965 )*age + (0.07981)*pain_cat + (0.41203)*cortisol_serum + (-0.29661)*mindfulness + (-0.01715)*weight

Theoretical_predict <- predict(TheoreticalModel, home_sample_2)
Backward_predict <- predict(BackwardModel, home_sample_2)

RSS_theoretical = sum((home_sample_2[, "pain"] - Theoretical_predict)^2) 
RSS_theoretical
RSS_backward = sum((home_sample_2[, "pain"] - Backward_predict)^2)
RSS_backward

# Tables for coefficients and confidence intervals of both the theoretical and the backward model
s_t <- summary(TheoreticalModel)
s_b <- summary(BackwardModel)

confint(TheoreticalModel)	
lm.beta::lm.beta(TheoreticalModel)	

confint(BackwardModel)	
lm.beta::lm.beta(BackwardModel)	

s_t_p_values = as.character(round(s_t$coefficients[,4], 3))	
s_t_p_values[s_t_p_values != "0" & s_t_p_values != "1"] = substr(s_t_p_values[s_t_p_values != "0" & s_t_p_values != "1"], 2, nchar(s_t_p_values[s_t_p_values != "0" & s_t_p_values != "1"]))	
s_t_p_values[s_t_p_values == "0"] = "<.001"

s_b_p_values = as.character(round(s_b$coefficients[,4], 3))	
s_b_p_values[s_b_p_values != "0" & s_b_p_values != "1"] = substr(s_b_p_values[s_b_p_values != "0" & s_b_p_values != "1"], 2, nchar(s_b_p_values[s_b_p_values != "0" & s_b_p_values != "1"]))	
s_b_p_values[s_b_p_values == "0"] = "<.001"

st_table = cbind(as.data.frame(round(cbind(coef(TheoreticalModel), confint(TheoreticalModel), c(0, lm.beta::lm.beta(TheoreticalModel)$standardized.coefficients[c(2,3)])), 2)), s_t_p_values)	
names(st_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
st_table["(Intercept)","Std.Beta"] = "0"

sb_table = cbind(as.data.frame(round(cbind(coef(BackwardModel), confint(BackwardModel), c(0, lm.beta::lm.beta(BackwardModel)$standardized.coefficients[c(2,3)])), 2)), s_b_p_values)	
names(sb_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sb_table["(Intercept)","Std.Beta"] = "0"	

st_table
#                    b 95%CI lb 95%CI ub Std.Beta p-value
#(Intercept)     2.85    -0.21     5.92        0    .068
#sexmale         0.49     0.10     0.89     0.16    .015
#age            -0.04    -0.09     0.00    -0.15    .043
#STAI_trait      0.00    -0.05     0.05        0    .923
#pain_cat        0.08     0.03     0.12     0.16    .001
#mindfulness    -0.29    -0.51    -0.07    -0.15    .011
#cortisol_serum  0.43     0.21     0.65        0   <.001

sb_table
#                    b    95%CI lb 95%CI ub Std.Beta p-value
# (Intercept)     4.21     0.90     7.53        0    .013
# sexmale         0.47     0.09     0.86     0.15    .016
# age            -0.05    -0.09    -0.01    -0.16    .018
# pain_cat        0.08     0.03     0.12        0    .001
# mindfulness    -0.30    -0.51    -0.08     0.15    .008
# cortisol_serum  0.41     0.21     0.62    -0.16   <.001
# weight         -0.02    -0.04     0.00        0     .07



