
title: "Multiple Regression with R" 
author: Ernest Fotsing, PhD

# Section 1: Multiple Regression

# The in-built dataset `trees` contains data pertaining to the `Volume`, `Girth` and `Height` of 31 felled black cherry trees. In the Simple Regression session, we constructed a simple linear model for `Volume` using `Girth` as the independent variable. Now we will expand this by considering `Height` as another predictor.
# 
# Start by plotting the dataset:

plot(trees)

# This plots all variables against each other, enabling visual information about correlations within the dataset.

#Re-create the original model of `Volume` against `Girth`:

  
m1 = lm(Volume~Girth,data=trees)
summary(m1)
#Now include `Height` as an additional variable:
m2 = lm(Volume~Girth+Height,data=trees)
summary(m2)
# 
# Note that the R^2 has improved, yet the `Height` term is less significant than the other two parameters.

#Try including the interaction term between `Girth` and `Height`:
m3 = lm(Volume~Girth*Height,data=trees)
summary(m3)

#All terms are highly significant. Note that the `Height` is more significant than in the previous model, despite the introduction of an additional parameter.

# We'll now try a different functional form - rather than looking for an additive model, we can explore a multiplicative model by applying a log-log transformation (leaving out the interaction term for now).


m4 = lm(log(Volume)~log(Girth)+log(Height),data=trees)
summary(m4)

#All terms are significant. Note that the residual standard error is much lower than for the previous models. However, this value cannot be compared with the previous models due to transforming the response variable. The R^2 value has increased further, despite reducing the number of parameters from four to three.

confint(m4)
# Looking at the confidence intervals for the parameters reveals that the estimated power of `Girth` is around 2, and `Height` around 1. This makes a lot of sense, given the well-known dimensional relationship between `Volume`, `Girth` and `Height`!

#For completeness, we'll now add the interaction term.

m5 = lm(log(Volume)~log(Girth)*log(Height),data=trees)
summary(m5)

# The R^2 value has increased (of course, as all we've done is add an additional parameter), but interestingly none of the four terms are significant. This means that none of the individual terms alone are vital for the model - there is duplication of information between the variables. So we will revert back to the previous model.
# 
# Given that it would be reasonable to expect the power of `Girth` to be 2, and Height to be 1, we will now fix those parameters, and instead just estimate the one remaining parameter.

m6 = lm(log(Volume)-log((Girth^2)*Height)~1,data=trees)
summary(m6)

# Note that there is no R^2 (as only the intercept was included in the model), and that the Residual Standard Error is incomparable with previous models due to changing the response variable.
# We can alternatively construct a model with the response being y, and the error term additive rather than multiplicative.

m7 = lm(Volume~0+I(Girth^2):Height,data=trees) # The 0 + in the formula removes the intercept.I(Girth^2) ensures it literally squares the girth values.I(Girth^2):Height is the product of Girth^2 and Height.
summary(m7)

# Note that the parameter estimates for the last two models are slightly different... this is due to differences in the error model.

# Section 2: Model Selection

# Of the last two models, the one with the log-Normal error model would seem to have the more Normal residuals. This can be inspected by looking at diagnostic plots, by and using the `shapiro.test()`

plot(m6)
plot(m7)
shapiro.test(residuals(m6))
shapiro.test(residuals(m7))
# The Akaike Information Criterion (AIC) can help to make decisions regarding which model is the most appropriate. Now calculate the AIC for each of the above models:

summary(m1)
AIC(m1)
summary(m2)
AIC(m2)
summary(m3)
AIC(m3)
summary(m4)
AIC(m4)
summary(m5)
AIC(m5)
summary(m6)
AIC(m6)
summary(m7)
AIC(m7)

# Whilst the AIC can help differentiate between similar models, it cannot help deciding between models that have different responses. Which model would you select as the most appropriate?

# Section 3: Stepwise Regression

# The in-built dataset `swiss` contains data pertaining to fertility, along with a variety of socioeconomic indicators. We want to select a sensible model using stepwise regression. First regress `Fertility` agains all available indicators
head(swiss)
m8 = lm(Fertility~.,data=swiss)
summary(m8)

# Are all terms significant?
# Now use stepwise regression, performing backward elimination in order to automatically remove inappropriate terms

library(MASS)
summary(stepAIC(m8))

#Are all terms significant? Is this model suitable? What are the pro's and con's of this approach? 

