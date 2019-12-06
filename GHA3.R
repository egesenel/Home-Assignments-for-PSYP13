##################################################################
###### PSYP13 Sub-course 1 - GP Assignment 3 - by Ege Şenel ######
##################################################################

# load all packages
library('paramtest')
library('pwr')
library('ggplot2')
library('knitr')
library('nlme')
library('lavaan')
library('dplyr')
library(lsr)
library(psych)

# Create the anova function
an_func <- function(simNum, N, f) {
  v1 = rnorm(N, mean = 0.5, sd = 1) # generate the data
  v2 = rnorm(N, mean = -0.5, sd = 1)
  v3 = rnorm(N, mean = 0, sd = 1)
  v4 = rnorm(N, mean = 0, sd =1)
  grp1= c(rep(1, N)) # define how many in groups
  grp2= c(rep(2, N))
  grp3= c(rep(3, N))
  grp4= c(rep(4, N))
  f.treat=c(grp1, grp2, grp3, grp4) # make a single vector of groups
  meas=c(v1, v2, v3, v4) # make a single vector of the data (measurements)
  mydata=cbind(f.treat,meas) # column bind to create a matrix
  mydata=as.data.frame(mydata) # convert to a data frame
  mydata$f.treat = as.factor(mydata$f.treat) # convert groups to a factor
  #Report the descriptive statistics. What are the means, and standard deviations of your variables?
  my.anova <- aov(formula = meas ~ f.treat, data = mydata ) # run anova on generated data
  sm <- summary(my.anova)
  sm1 <- sm[[1]]
  sm2 <- sm1[-2,]
  stat <- sm2$`F value`
  p <- sm2$`Pr(>F)`
  
  return(c(my.anova = stat, p = p, sig = (p < .05)))
}

# varying N and Cohen's f
power_anova <- grid_search(an_func, params=list(N = seq(10, 50, 1), f = .35),
                                 n.iter=100, output='data.frame')
power <- results(power_anova) %>%
  group_by(N.test, f.test) %>%
  summarise(power=mean(sig))
print(power)
ggplot(power, aes(x=N.test, y=power, group=factor(f.test), colour=factor(f.test))) +
  geom_point() +
  geom_line() +
  ylim(c(0, 1)) +
  labs(x='Sample Size', y='Power', colour="Cohen's f") +
  theme_minimal()

# Counting significant p-values for every sample size
pcount10 <- power_anova$results$p[c(1:100)]
table(pcount10 < 0.05)
pcount11 <- power_anova$results$p[c(101:200)]
table(pcount11 < 0.05)
pcount12 <- power_anova$results$p[c(201:300)]
table(pcount12 < 0.05)
pcount13 <- power_anova$results$p[c(301:400)]
table(pcount13 < 0.05)
pcount14 <- power_anova$results$p[c(401:500)]
table(pcount14 < 0.05)
pcount15 <- power_anova$results$p[c(501:600)]
table(pcount15 < 0.05)
pcount16 <- power_anova$results$p[c(601:700)]
table(pcount16 < 0.05)
pcount17 <- power_anova$results$p[c(701:800)]
table(pcount17 < 0.05)
pcount18 <- power_anova$results$p[c(801:900)]
table(pcount18 < 0.05)
pcount19 <- power_anova$results$p[c(901:1000)]
table(pcount19 < 0.05)
pcount20 <- power_anova$results$p[c(1001:1100)]
table(pcount20 < 0.05)
pcount21 <- power_anova$results$p[c(1101:1200)]
table(pcount21 < 0.05)
pcount22 <- power_anova$results$p[c(1201:1300)]
table(pcount22 < 0.05)
pcount23 <- power_anova$results$p[c(1301:1400)]
table(pcount23 < 0.05)
pcount24 <- power_anova$results$p[c(1401:1500)]
table(pcount24 < 0.05)
pcount25 <- power_anova$results$p[c(1501:1600)]
table(pcount25 < 0.05)
pcount26 <- power_anova$results$p[c(1601:1700)]
table(pcount26 < 0.05)
pcount27 <- power_anova$results$p[c(1701:1800)]
table(pcount27 < 0.05)
pcount28 <- power_anova$results$p[c(1801:1900)]
table(pcount28 < 0.05)
pcount29 <- power_anova$results$p[c(1901:2000)]
table(pcount29 < 0.05)
pcount30 <- power_anova$results$p[c(2001:2100)]
table(pcount30 < 0.05)
pcount31 <- power_anova$results$p[c(2101:2200)]
table(pcount31 < 0.05)
pcount32 <- power_anova$results$p[c(2201:2300)]
table(pcount32 < 0.05)
pcount33 <- power_anova$results$p[c(2301:2400)]
table(pcount33 < 0.05)
pcount34 <- power_anova$results$p[c(2401:2500)]
table(pcount34 < 0.05)
pcount35 <- power_anova$results$p[c(2501:2600)]
table(pcount35 < 0.05)
pcount36 <- power_anova$results$p[c(2601:2700)]
table(pcount36 < 0.05)
pcount37 <- power_anova$results$p[c(2701:2800)]
table(pcount37 < 0.05)
pcount38 <- power_anova$results$p[c(2801:2900)]
table(pcount38 < 0.05)
pcount39 <- power_anova$results$p[c(2901:3000)]
table(pcount39 < 0.05)
pcount40 <- power_anova$results$p[c(3001:3100)]
table(pcount40 < 0.05)
pcount41 <- power_anova$results$p[c(3101:3200)]
table(pcount41 < 0.05)
pcount42 <- power_anova$results$p[c(3201:3300)]
table(pcount42 < 0.05)
pcount43 <- power_anova$results$p[c(3301:3400)]
table(pcount43 < 0.05)
pcount44 <- power_anova$results$p[c(3401:3500)]
table(pcount44 < 0.05)
pcount45 <- power_anova$results$p[c(3501:3600)]
table(pcount45 < 0.05)
pcount46 <- power_anova$results$p[c(3601:3700)]
table(pcount46 < 0.05)
pcount47 <- power_anova$results$p[c(3701:3800)]
table(pcount47 < 0.05)
pcount48 <- power_anova$results$p[c(3801:3900)]
table(pcount48 < 0.05)
pcount49 <- power_anova$results$p[c(3901:4000)]
table(pcount49 < 0.05)
pcount50 <- power_anova$results$p[c(4001:4100)]
table(pcount50 < 0.05)

# Calculating power with non-central f-distribution
ncf_func <- function(simNum, N, d) {
  
  ncf = (1 - pf(q = qf(0.95, 3, 4*N - 3), df1 = 3, df2 = 4*N -3, ncp = (4*N*(d)^2)))
  
  p <- ncf
  
  return(c(d.test = d, p = p))
  
}

# give 'params' a list of parameters we want to vary;
# testing at N = (10:50)
power_ncf <- grid_search(ncf_func, params=list(N = seq(10, 50, 1)),
                                n.iter=1, output='data.frame', d = 0.35)


powerf <- results(power_ncf) %>%
  group_by(N.test, d.test) %>%
  summarise(power = p)
print(powerf)

# Plot
ggplot(powerf, aes(x=N.test, y=power, group=factor(d.test), colour=factor(d.test))) +
  geom_point() +
  geom_line() +
  ylim(c(0, 1)) +
  labs(x='Sample Size', y='Power', colour="Cohen's f") +
  theme_minimal()








