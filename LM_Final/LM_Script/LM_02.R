
title: "Simple Regression with R" 
author: "Ernest Fotsing, PhD"
  
# Section 7: Simple Regression with R
  # -----------------------------
# Section 1: Correlation Coefficients

#We'll start by generating some synthetic data to investigate correlation coefficients.

#Generate 50 random numbers in the range [0,50]:
x = runif(50,0,50)

#Now let's generate some y-values that are linearly correlated with the x-values with gradient=1, applying a random Normal offset (with sd=5):
y = x + rnorm(50,0,5)

#Plotting y against x, you'll observe a positive linear relationship:
plot(y~x)


#This strong linear relationship is reflected in the correlation coefficient and in the coefficient of determination (R^2):

pearson_cor_coef = cor(x,y)
list("cor"=pearson_cor_coef,"R^2"=pearson_cor_coef^2)

#If the data exhibit a negative linear correlation then the correlation coefficient will become #strong and negative, whilst the R^2 value will remain strong and positive:

y = -x + rnorm(50,0,5)
plot(y~x)
pearson_cor_coef = cor(x,y)
list("cor"=pearson_cor_coef,"R^2"=pearson_cor_coef^2)

# If data are uncorrelated then both the correlation coefficient and R^2 values will be close to zero:

y = rnorm(50,0,5) #will be a numeric vector of length 50, with values randomly drawn from a normal distribution centered at 0 and spread with standard deviation 5.
plot(y~x)
pearson_cor_coef = cor(x,y)
list("cor"=pearson_cor_coef,"R^2"=pearson_cor_coef^2)

#The significance of a correlation can be tested using `cor.test()`, which also provides a 95% confidence interval on the correlation:

cor.test(x,y)

# In this case, the value 0 is contained within the confidence interval, indicating that there is insufficient evidence to reject the null hypothesis that the true correlation is equal to zero.

# Section 2: Simple Regression

#Now let's look at some real data.

# The in-built dataset `trees` contains data pertaining to the `Volume`, `Girth` and `Height` of  felled black cherry trees.
# 
# We will now attempt to construct a simple linear model that uses `Girth` to predict `Volume`.
?trees # to what's is these data about 
data(trees)

plot(Volume~Girth,data=trees)
m1 = lm(Volume~Girth,data=trees)
abline(m1)
cor.test(trees$Volume,trees$Girth)

# It is evident that `Volume` and `Girth` are highly correlated.
# The summary for the linear model provides information regarding the quality of the model:

summary(m1)
#Model residuals can be readily accessed using the `residuals()` function:

hist(residuals(m1),breaks=10,col="light grey")
# Diagnostic plots for the model can reveal whether or not modelling assumptions are reasonable. In this case, there is visual evidence to suggest that the assumptions are not satisfied - note in particular the trend observed in the plot of residuals vs fitted values:
plot(m1)

# Section 3: Assessing the quality of linear models

# Let's see what happens if we try to describe a non-linear relationship using a linear model. Consider the sine function in the range [0,1.5*pi):

z = seq(0,1.5*pi,0.2)
plot(sin(z)~z)
m0 = lm(sin(z)~z)
abline(m0)

# In this case, it is clear that a linear model is not appropriate for describing the relationship. However, we are able to fit a linear model, and the linear model summary does not identify any major concerns:

summary(m0)

# Here we see that the overall p-value is low enough to suggest that the model has significant utility, and both terms (the intercept and the coefficient of `z`) are significantly different from zero. The R^2 value of 0.5422 is high enough to indicate that there is a reasonably strong correlation between `sin(z)` and `z` in this range. 
# 
# This information is misleading, as we know that a linear model is inappropriate in this case. Indeed, the linear model summary does not check whether the underlying model assumptions are satisfied. 
# 
# By observing strong patterns in the diagnostic plots, we can see that the modelling assumptions are not satisified in this case.

plot(m0)

# Section 4: Modelling Non-Linear Relationships

# It is sometimes possible to use linear models to describe non-linear relationships (which is perhaps counterintuitive!). This can be achieved by applying transformations to the variable(s) in order to linearise the relationship, whilst ensuring that modelling assumptions are satisfied.
# 
# Another in-built dataset `cars` provides the speeds and associated stopping distances of cars in the 1920s.

#Let's construct a linear model to predict stopping distance using speed:

plot(dist~speed,data=cars)
m2 = lm(dist~speed,data=cars)
abline(m2)
summary(m2)

# The model summary indicates that the intercept term does not have significant utility. So that term could/should be removed from the model.
# 
# In addition, the plot of residuals versus fitted values indicates potential issues with variance stability
  
plot(m2)

# In this case, variance stability can be aided by a square-root transformation of the response variable:

plot(sqrt(dist)~speed,data=cars)
m3 = lm(sqrt(dist)~speed,data=cars)
abline(m3)
plot(m3)
summary(m3)

The R^2 value is improved over the previous model.
Note that again that the intercept term is not significant.

# We'll now try a log-log transformation, that is applying a log transformation to the predictor and response variables. This represents a power relationship between the two variables.


plot(log(dist)~log(speed),data=cars)
m4 = lm(log(dist)~log(speed),data=cars)
abline(m4)
plot(m4)
summary(m4)


# The R^2 value is improved, and the diagnostic plots don't look too unreasonable. However, again the intercept term does not have significant utility. So we'll now remove it from the model:

m5 = lm(log(dist)~0+log(speed),data=cars)
plot(m5)
summary(m5)

# This model seems reasonable. However, remember that R^2 values corresponding to models without an intercept aren't meaningful (or at least can't be compared against models with an intercept term).

#We can now transform the model back, and display the regression curve on the plot

plot(dist~speed,data=cars)
x = order(cars$speed)
lines(exp(fitted(m5))[x]~cars$speed[x])


# Section 5: Relationship between the t-test, ANOVA and linear regression

# In the ANOVA session we looked at the `diet` dataset, and performed the t-test and ANOVA. Here's a recap:
#   
# Now let's use a different strategy. Instead of directly testing whether there is a difference between the two groups, let's attempt to create a linear model describing the relationship between `weight.loss` and `diet.type`. Indeed, it is possible to construct a linear model where the independent variable(s) are categorical - they do not have to be continuous or even ordinal!
#   

summary(lm(weight.loss~diet.type,data=diet[diet$diet.type!="B",]))

You can see that the p-value corresponding to the `diet.type` term is the same as the overall p-value of the linear model, which is also the same as the p-value from the t-test and ANOVA. Note also that the F-test statistic is the same as given by the ANOVA.

So, we are also able to use the linear model to test the hypothesis that there is a difference between the two diet groups, as well as provide a more detailed description of the relationship between `weight.loss` and `diet.type`. 

# Section 6: Practical Exercises

## Old Faithful
data(faithful)

The inbuilt R dataset `faithful` pertains to the waiting time between eruptions and
the duration of the eruption for the Old Faithful geyser in Yellowstone National
Park, Wyoming, USA.

- Create a simple linear regression model that models the eruption duration `faithful$eruptions` using waiting time `faithful$waiting` as the independent variable, storing the model in a variable. Look at the summary of the model.
+ What are the values of the estimates of the intercept and coefficient of 'waiting'?
  + What is the R^2 value?
  + Does the model have significant utility?
  + Are neither, one, or both of the parameters significantly different from zero?
  + Can you conclude that there is a linear relationship between the two variables?
  - Plot the eruption duration against waiting time. Is there anything noticeable
about the data?
  - Draw the regression line corresponding to your model onto the plot. Based on this graphical representation, does the model seem reasonable?
  - Generate the four diagnostic plots corresponding to your model. Contemplate the appropriateness of the model for describing the relationship between eruption duration and waiting time.

## Anscombe datasets

Consider the inbuilt R dataset `anscombe`. This dataset contains four x-y datasets,
contained in the columns: (x1,y1), (x2,y2), (x3,y3) and (x4,y4).

- For each of the four datasets, calculate and test the correlation between the x and y
variables. What do you conclude?
  - For each of the four datasets, create a linear model that regresses y on x. Look
at the summaries corresponding to these models. What do you conclude?
  - For each of the four datasets, create a plot of y against x. What do you
conclude?
  
  ## Pharmacokinetics of Indomethacin
  
  Consider the inbuilt R dataset `Indometh`, which contains data on the pharmacokinetics of indometacin.

- Plot `Indometh$time` versus `Indometh$conc` (concentration). What is the nature of the relationship
between `time` and `conc`?
  - Apply monotonic transformations to the data so that a simple linear regression model can be used to model the relationship (ensure both linearity and stabilised variance, within reason). Create a plot of the transformed data, to confirm that the relationship seems linear.
- After creating the linear model, inspect the diagnostic plots to ensure that the
assumptions are not violated (too much). Are there any outliers with large influence? What are the parameter estimates? Are both terms significant?
  - Add a line to the plot showing the linear relationship between the transformed data.
- Now regenerate the original plot of `time` versus `conc` (i.e. the untransformed
                                                            data). Using the `lines` function, add a curve to the plot corresponding to the
fitted values of the model.

