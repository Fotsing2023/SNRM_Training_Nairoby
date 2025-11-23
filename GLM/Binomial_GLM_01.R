
title: "GLM with R" 
author: "Ernest Fotsing, PhD"

#clean my environnement
rm(list=ls())
install.packages("gamlss")
library(gamlss)

### Section 1: importation and descriptive analysis

# We will analyse the data collected by Jones 
#(Unpublished BSc dissertation, University of Southampton, 1975). 
#The aim of the study was to define if the probability of having 
#Bronchitis is influenced by smoking and/or pollution.

# The data are stored under data/Bronchitis.csv and contains information on 212 participants.

Bronchitis = read.csv("CourseData/Bronchitis.csv",header=TRUE)

plot(Bronchitis$cigs,Bronchitis$bron,col="blue4",
     ylab = "Absence/Presense of Bronchitis", xlab = "Daily number of cigarettes")
abline(h=c(0,1),col="light blue")

# Section 8: GLM, Logistic regression

# We will analyse the data collected by Jones (Unpublished BSc dissertation, University of Southampton, 1975). 
# The aim of the study was to define if the probability of having Bronchitis is influenced by smoking and/or pollution.
# The data are stored under CourseData/Bronchitis.csv and contains information on 212 participants.

# -----------------------------
# Section 8.1: Importation and descriptive analysis
# -----------------------------

# Import the dataset
Bronchitis = read.csv("CourseData/Bronchitis.csv", header = TRUE)
str(Bronchitis)
# Plot bron (0/1) as a function of daily cigarettes
plot(Bronchitis$cigs, Bronchitis$bron, col = "blue4",
     ylab = "Absence/Presence of Bronchitis", xlab = "Daily number of cigarettes")
abline(h = c(0,1), col = "light blue")

# -----------------------------
# Section 8.2: Model fit
# -----------------------------

# Fit logistic regression using glm
fit.glm = glm(bron ~ cigs, data = Bronchitis, family = binomial)
summary(fit.glm)

# Fit logistic regression using gamlss
# It allows you to fit statistical models where not only the mean 
# of the distribution depends on predictors, but also variance, 
# skewness, and kurtosis can be modeled.
# It is more flexible than GLMs and GAMs
# logit(P(bron=1))=β0+β1*cigs and L = -2(logrestricted -logfull )

library(gamlss)
fit.gamlss = gamlss(bron ~ cigs, data = Bronchitis, family = BI)

# Display summary of glm
summary(fit.glm)
summary(fit.gamlss)

# Plot logistic curve
plot(Bronchitis$cigs, Bronchitis$bron, col = "blue4",
     ylab = "Absence/Presence of Bronchitis", xlab = "Daily number of cigarettes")
abline(h = c(0,1), col = "light blue")

axe.x = seq(0, 40, length = 1000)
f.x = exp(fit.glm$coef[1] + axe.x * fit.glm$coef[2]) / 
  (1 + exp(fit.glm$coef[1] + axe.x * fit.glm$coef[2]))
lines(axe.x, f.x, col = "red", lwd = 2)

#plot(fit.gamlss)
# In GAMLSS (Generalized Additive Models for Location, 
# Scale and Shape), the predict() function 
# is used to compute predictions from the fitted model.
# now let's predicted the probabilities of bronchitis for each value of cigs

Pred<- as.data.frame(predict(fit.gamlss, type="response"))
Pred


# -----------------------------
# Section 8.3: Model selection
# When you fit a generalized linear model (GLM) 
# (e.g., logistic regression), you often want to know whether 
# the predictor(s) significantly improve the model.
# 
# H0:predictors do NOT improve the model
# H1: predictors DO improve the model
#  Lrt will test the fitted model vs the null model
 
# -----------------------------
anova(fit.glm, test = "LRT")
# -----------------------------
# Section 8.4: Model check
# -----------------------------
# Deviance residuals and randomised normalised quantile residuals
par(mfrow = c(2,2), mar = c(3,5,3,0))
plot(fit.glm)
plot(gamlss(bron ~ cigs, data = Bronchitis, family = BI))

# -----------------------------
# Section 8.5: Poisson regression
# -----------------------------
# Import students dataset
students = read.csv("./CourseData/students.csv", header = TRUE)
str(students)

# Plot daily cases
plot(students$day, students$cases, col = "blue4",
     ylab = "Number of diagnosed students", xlab = "Days since initial outbreak")
abline(h = 0, col = "light blue")

# Fit Poisson regression using glm
fit.glm = glm(cases ~ day, data = students, family = poisson)

# Fit Poisson regression using gamlss

fit.gamlss = gamlss(cases ~ day, data = students, family = PO)

# Summary of glm
summary(fit.glm)

# Plot fitted Poisson curve
plot(students$day, students$cases, col = "blue4",
     ylab = "Number of diagnosed students", xlab = "Days since initial outbreak")
abline(h = 0, col = "red")

axe.x = seq(0, 120, length = 1000)
f.x = exp(fit.glm$coef[1] + axe.x * fit.glm$coef[2])
lines(axe.x, f.x, col = "red", lwd = 2)

# Model selection

fit.glm0 = glm(cases ~ 1, data = students, family = poisson)
anova(fit.glm, test = "LRT")
anova(fit.glm, fit.glm0, test = "LRT")

# Model check
par(mfrow = c(2,2), mar = c(3,5,3,0))
plot(fit.glm)
plot(fit.gamlss)


# -----------------------------
# Section 8.6: Practicals
# -----------------------------

# (i) Bronchitis.csv
# Analyse if probability of having bronchitis depends on pollution (poll) and 
# check for interaction between cigs and pollution

# We will apply a bit of model selection

m1 <- gamlss(bron ~ poll, data = Bronchitis, family = BI)
summary(m1)

m2 <- gamlss(bron ~ cigs + poll, data = Bronchitis, family = BI)
summary(m2)

#To see if pollution modifies the effect of cigarettes, include an interaction term:
m3 <- gamlss(bron ~ cigs * poll, data = Bronchitis, family = BI)
summary(m3)#Pollution significantly increases the probability of bronchitis
#As pollution increases, the probability of having bronchitis also increases, 
#independent of smoking. Pollution does not change the effect of cigarettes on bronchitis risk.
#Smoking does not modify the impact of pollution either.
#So no evidence that the effect of one depends on the level of the other.

#question?  Yes — because it’s not significant

m2 <- gamlss(bron ~ cigs + poll, data = Bronchitis, family = BI)
GAIC(m2, m3, k = 2)   # AIC comparison
GAIC(m2, m3, k = log(212))   # BIC comparison

# or use LRT
LRT <- 2 * (logLik(m3) - logLik(m2))
pchisq(LRT, df = 1, lower.tail = FALSE)#Because adding the interaction term adds 1 parameter, so df = 1.

#-----model plotting and diagnostics----------------


##############################################
### 1. Fit final GAMLSS model
##############################################
library(ggplot2)
install.packages("plot3D")
library(plot3D)

# Final model without interaction (since not significant)
m2 <- gamlss(bron ~ cigs + poll, data = Bronchitis, family = BI)
summary(m2)

##############################################
### 2. Predicted probability vs Pollution
##############################################

poll.new <- data.frame(
  poll = seq(min(Bronchitis$poll), max(Bronchitis$poll), length = 100),
  cigs = mean(Bronchitis$cigs)
)

poll.new$pred <- predict(m2, newdata = poll.new, type = "response")

p1 <- ggplot(poll.new, aes(poll, pred)) +
  linewidth(size = 1.2, color = "blue") +
  labs(
    x = "Pollution level",
    y = "Predicted probability of bronchitis",
    title = "Effect of Pollution on Bronchitis Probability"
  ) +
  theme_minimal()
p1


##############################################
### 3. Predicted probability vs Cigarettes
##############################################

cigs.new <- data.frame(
  cigs = seq(min(Bronchitis$cigs), max(Bronchitis$cigs), length = 100),
  poll = mean(Bronchitis$poll)
)

cigs.new$pred <- predict(m2, newdata = cigs.new, type = "response")

p2 <- ggplot(cigs.new, aes(cigs, pred)) +
  geom_line(size = 1.2, color = "red") +
  labs(
    x = "Cigarette consumption",
    y = "Predicted probability of bronchitis",
    title = "Effect of Cigarettes on Bronchitis Probability"
  ) +
  theme_minimal()

p2

##############################################
### 4. Combined 3D surface plot
##############################################

poll.seq <- seq(min(Bronchitis$poll), max(Bronchitis$poll), length = 50)
cigs.seq <- seq(min(Bronchitis$cigs), max(Bronchitis$cigs), length = 50)

grid <- expand.grid(poll = poll.seq, cigs = cigs.seq)
grid$pred <- predict(m2, newdata = grid, type = "response")

zmat <- matrix(grid$pred, nrow = 50, byrow = TRUE)

persp3D(
  x = poll.seq,
  y = cigs.seq,
  z = zmat,
  xlab = "Pollution",
  ylab = "Cigarettes",
  zlab = "Probability",
  main = "Bronchitis Probability Surface",
  theta = 40, phi = 20, expand = 0.8,
  col = ramp.col(c("lightblue", "blue", "darkblue"))
)


##############################################
### 5. Model Diagnostic Plots for GAMLSS
##############################################

# 5.1 Residual plots
plot(m2)     # fitted values vs residuals

# 5.2 Normal Q-Q plot of residuals
qqnorm(resid(m2))
qqline(resid(m2), col="red")

# 5.3 Worm plot (diagnostic tool for GAMLSS)
wp(m2)

# 5.4 Histogram of residuals
hist(resid(m2), breaks = 20, col = "lightgray",
     main = "Histogram of Residuals",
     xlab = "Residuals")

# 5.5 Check overdispersion
deviance(m2)# Deviance is a measure of the model's lack of fit compared to a saturated model
AIC(m2)

##############################################
### Finished
##############################################


#----------------Exercice-to practice--------------------------

# (ii) myocardialinfarction.csv
# Analyse if Aspirin decreases the probability of myocardial infarction attack

# (iii) crabs.csv

# Analyse width of female crabs (W) and number of satellites (Sa) using Poisson regression
# Variables:
# C = color, S = spine condition, Wt = weight, W = carapace width, Sa = # satellites



