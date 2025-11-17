#______________________________________________
### Diagnostics of Linear Models in R
### Week 3: Generalized Linear and Mixed Models in Ecology
### Smithsonian-Mason School of Conservation
### Exercise developed by Joe Kolowski
# 
### Objectives:   1. Get familiar with model diagnostic tools
#                 2. Continue to practice basic plotting approaches to help with visual diagnostics
#                   
#
### DataSets used in Script: Anadonetal2014JECOL.txt

### Activities: Re-use the best model found in lm_inter_poly.R and perform all the relevant model diagnostics we covered in lecture
#_____________________________________________

# Clear the environment and load packages ####

rm(list = ls())
library(GGally)
library(car) # allows us to assess VIF on a completed model
library(tidyverse)
library(ncf) # for spline.correlog function
library(pgirmess) # for correlog function
library(performance) # for test of homoscedasticity
theme_set(theme_bw(12))


# Load and Prepare Data ####

# Remember that we already cleaned and prepared this tree data set to our liking, and we saved it as an RDS object. Let's read that in now and assign it a name.

treeData <- read_rds(file = "./CourseData/myCleanTreeData.RDS")

# DATA DIAGNOSTICS ------------------------------------------------
# There are a few checks we should do with our data before we actually run any models. We've done one of these already in previous scripts, checking for collinearity among predictor variables. But we can also look at outlier points. If this were our own data, we would have a sense of what values make sense, and what do not, and we could use this to look for potentially erroneous points.

# Identify potential outliers ####

# Show boxplots of response and predictor data. You might want to enlarge your plotting window before creating these.
par(mfrow = c(2,2)) #set graphing to 2 rows by 2 columns

boxplot(treeData$Temp, xlab = "Temperature")
boxplot.stats(treeData$Temp)$out

boxplot(treeData$Precip, xlab = "Precipitation")
boxplot.stats(treeData$Precip)$out

boxplot(treeData$PET, xlab = "PET")
boxplot.stats(treeData$PET)$out

boxplot(treeData$TreeCov, xlab = "Tree cover")
boxplot.stats(treeData$TreeCov)$out

# As we saw in lecture, only precipitation has values that are unusually far from their mean. Before proceeding, if this was our own data, we would want to double check that those values were not mistakes. We can see what those values are, as we did in lecture, using the boxplot.stats function
boxplot.stats(treeData$Precip)$out

# Remember though that removal of outliers is not recommended unless you have reason to believe that they are in error. You may elect to also re-run analyses without certain outlying points that you deem to have very high level of influence on your results, but we'll look at that below, once we have a model.

# Check for multi-collinearity####

# As we've seen before, we can use a few different approaches to look for multicollinearity, but remember that we'll have a whole separate lecture/module on this, so we won't spend too much time on it here. Let's use ggpairs to have a look at relationships among our predictors.
ggpairs(data = treeData, columns = c("Temp", "Precip", "PET"))

# PET is highly correlated with temperature. We weren't planning on using PET here, but if we were, we would have to avoid running PET and Temp in the same model. Our predictors of interest, Precip and Temp, are not correlated above 0.70, so we can move forward with these.


# MODEL DIAGNOSTICS -------------------------------------------------------
# We will now focus on diagnostics performed on an existing model. In most cases, this would take place after model comparison and selection identifies an optimal model.

### Let's recreate the best model we found during the lecture on polynomials, so we have a strong model to work with and perform diagnostics on.
### 
bestM <- lm(TreeCov ~ Temp * poly(Precip, 2), data = treeData)

# Main effect of Temp
# 
# Main effects of both polynomial terms of Precip, Interaction terms between Temp and both polynomial components of Precip
# 
# TreeCov=β0​+β1​Temp+β2​P1​(Precip)+β3​P2​(Precip)+β4​Temp×P1​(Precip)+β5​Temp×P2​(Precip)
# where P1 and P2 are the orthogonal polynomial terms.
# ​
# Interpretation
# 
# we’re modeling how tree cover depends on temperature and precipitation,
# 
# allowing a curved (quadratic) relationship with precipitation,
# 
# and letting the effect of precipitation vary with temperature (via the interaction).


### 1 - Look at influential points with Cook's distance ----

# Calculate Cook's distance to assess level of influence of each point in the data set. Remember from lecture this is different than being an outlier. Influential points have a large influence on model estimates, and this is assessed by removing them from the model and looking at the change in estimates.
par(mfrow = c(1,1)) #return to plotting in a single window

# Saving cook's distance values as a separate object
cd <- cooks.distance(bestM) 

# plot cook's distance values as vertical bars
plot(cd, type = "h", ylab = "Cook's distance")

# add a horizontal lines representing different thresholds of concern for Cook's Distance found in literature
abline(h = 1, lty = 1, col = "green") # any value > 1
abline(h = 0.5, lty = 2, col = "blue") # any value > 0.5
abline(h = 3* mean(cd),  lty = 4, col = "red") # any value > 3 times the mean cook's distance

# Here we will move forward with the third threshold above, and save those points exceeding the threshold for visual purposes. This creates a logical vector of TRUE and FALSE indicating which items 1 through 515 are above the threshold.
cd.idx <- cd >  3* mean(cd)
head(cd.idx, 20)

# Let's make a new dataframe of only the rows in treeData with Cook's Distance values above our threshold. Because cd.idx is already a logical vector, we can use it directly to filter our dataset.

treeData_inf <- treeData %>% 
  filter(cd.idx)

# Now we will add the rownames of the samples exceeding this threshold. The text function adds to the existing plot. This could help us find the potentially influential points in our dataset. Note how we can use the vector/matrix notation [] to insert a T/F vector (cd.idx) that will pull only the TRUE values from the particular vector or data frame. The statement cd[cd.idx] will return the values of cd that are listed as TRUE in cd.idx. These two vectors need to be the same length to have this work.

# In the text() function, we specify where on the plot to write the text, with x and y. The which() function is telling us which row numbers are true. Run the individual pieces of code by highlighting if you're unsure about what is happening here.

text(x = which(cd.idx), 
     y = cd[cd.idx], 
     labels = rownames(treeData)[cd.idx], 
     cex = 1)


# Now we will look at points that exceed the threshold on plots with meaningful variables, to identify why they may be influential. In this approach, I'm actually feeding ggplot new data for the last geom_point() function. 

ggplot(data = treeData, 
       aes(x = Temp, y = TreeCov)) +
  geom_point() +
  geom_point(data =  treeData_inf, 
             col = "red")

ggplot(data = treeData, 
       aes(x = Precip, y = TreeCov)) +
  geom_point() +
  geom_point(data =  treeData_inf, 
             col = "red")

# Many of the influential points seem to have particularly high precipitation

ggplot(data = treeData, 
       aes(x = Precip, y = Temp)) +
  geom_point() +
  geom_point(data =  treeData_inf, 
             col = "red")

# There are some handy built in plots generated automatically in R when you plot a regression model object. But, I'll be showing you how to do many of these manually so you have more control over these plots. There are 6 plots that R generates by default, so it is best to set up a graphing window that can show them all.
par(mfrow = c(2,3))
plot(bestM, 
     which = 1:6) #we need to specify that we want to see all 6

par(mfrow = c(1,1))
plot(bestM, 
     which = 4) # We can also just pick one of them. Plot 4 shows the cook's distance values, but is labeling those above a different cutoff than ours.


# Making conclusions: Points 195 and 193 are highly influential points. Others points are influential but not nearly as much.  Most influential points have high precipitation. First check to make sure there is nothing wrong with these points in terms of data quality. If they are good, it may be worth noting their influence in your paper and potentially looking at coefficients of the same model, run without those data points.

# For example, we can use the handy "update" function to update any component of a model. Here we will run a model with a data set that does not include the high cook's distance values. Running it like this just shows the model, because I'm not saving it as a model object. You could also save it with a new model name.

bestM
update(bestM, data = treeData[-cd.idx, ])

# All coefficient values are very similar to the original model, so there is nothing to be concerned about and we can proceed with the original model.

### 2 - No perfect multicollinearity----

# We can also investigate multicollinearity once we have a final model, just to double check there are no issues. This can be particularly useful when running models with large numbers of covariates, to do a final check of the level of collinearity. Note that the test above looking at correlation values are pairwise tests. The Variance Inflation Factor (VIF) indicates the level to which every covariate is collinear with the full set of other covariates in the model. 

# We will use the function "vif" from the package "car". Be sure you have the "car" package installed. Here we are setting type = "terms" because the inclusion of an interaction term greatly complicates the calculation of the vif. By setting the type to terms, we ask the function to ignore the relationship between the base terms and the interaction term (which will of course be related to the base terms).

vif(bestM, type = "terms") 

# OK bear with me for a bit of an aside here. So far I've only mentioned vif, and our goal of having this value not exceed 3 (or 10 depending on who you ask). The GVIF is the generalized variance inflation factor and this appears in the output only when you have model terms that are represented by more than 1 coefficient. So this happens when you have polynomial terms, interactions, and factors. The GVIF is meant to account for these additional model terms, which create a more complicated scenario for assessing VIF. Interpretation of these GVIF values, and a further corrected value GVIF^(1/(2*Df)), is slightly different, and a different threshold for problematic values needs to be used.

# In short, it seems we can still apply our general rule of thumb (threshold of 3.0) if we further adjust the GVIF^(1/(2*Df)). The consistent suggestion is to square this term, and then we can use the same threshold scale. So in our case the value for temp would be 1.05 * 1.05 = 1.10; 1.02 * 1.02 = 1.04. This are nice and low. --> Assumption satisfied 

# VIF or GVIF or GVIF^(1/(2*Df)) values for interaction terms will often be unusually high and these should be generally ignored. 

# It does seem that the GVIF^(1/(2*Df)) is a nice way to account for challenges in interpreting VIF for categorical variables, so when present, you should use GVIF^(1/(2*Df)), then square it and apply our typical threshold of 3.0.

# ADD INFORMATION ABOUT GVIF HERE FROM 2022 post.

### 3 - Normality of residuals ----

### We can look at a histogram of the residuals and add a normal curve to it to assess their normality. The residuals are saved in our linear model object and can be pulled out of the list object with the dollar sign, just as we do for columns in a data.frame. Remember we can look at what items are in a list like this:
names(bestM)

hist(bestM$residuals, freq = F)
curve(dnorm(x, 
            mean = 0, 
            sd = sd(bestM$residuals)), 
      add = T, 
      col = "red" ,
      lwd = 2) # here x is not an object, it just tells the dnorm function to use the x axis of the plot.

# This looks quite good. Remember that the most important are the tails. We don't want too many points in the tails. There are some data points in the upper tail, but not many. Assumption satisfied.


### We can also look at the Normal QQ plot to assess normality of the residuals. This is one of the automatic plots of a regression model object.

plot(bestM, which = 2) # This looks quite good. We can see the right hand tail is a bit off. As long as it doesn't look way off we are good. Much of this is admittedly subjective, but always best practice to visualize these things to look for severe anomalies.



### 4 - The mean of residuals is zero ----

mean(bestM$residuals) # e-16 is extremely small, so it is basically zero --> Assumptions satisfied


### 5 - Homoscedasticity of residuals or equal variance ----

#### We will here plot the residuals against the fitted/predicted values. This will become a standard diagnostic step for us once we've identified an "optimal" model that we want to move forward with and interpret. We can plot these manually, or use the default plotting option in R. This plot is plot #1 from that default group of plots. Note that the fitted values are also stored in the regression object list.

#Manual method
plot(bestM$residuals ~ bestM$fitted.values)
abline(h = 0, lty = 2)

# Canned R default method. This plot also labels points with the largest residuals and adds a smoothed line to help give you a sense of whether there is a pattern, which is helpful.

plot(bestM, which = 1) 

# Here we expect the residuals to be normally distributed around zero, and that the magnitude of the residuals (this is closely related to variance) is consistent across the range of fitted values. Here we are essentially looking for a pattern, and any pattern would be problematic. 

# Again we are in a subjective area here, and I would say that there may be some cause for concern in this model. There are very few large negative residuals at small fitted values, and very few large positive residuals at high fitted values. This causes the scatterplot to seem to trend down to the right.

# Fortunately, at least for basic linear models there are some tests available to help us be less subjective in this assessment. The package "performance" has a number of tests to help with model diagnostics. The function below performs a Breusch-Pagan test of the null hypothesis of constant variance of residuals

check_heteroscedasticity(bestM)

# Friendly little function no? So it looks like we are indeed OK, and the patterns we see in the plot above are not severe enough to be concerned.

# I should note that there are a separate series of statistical tests for homogeneity of variances among GROUPS. This would be relevant for categorical variables in an ANOVA framework. So for example we could run a model with our grazing predictor against fruit production in our anova script (we called it lm_Grazed), and use the function check_homogeneity() to see if the variance was similar across the grazing and non-grazing groups.


### 6 - The X variables and residuals are unrelated----

# In addition to plotting residuals against predicted/fitted values, we also want to look for patterns between our residuals and specific predictor variables. 

plot(bestM$residuals ~ treeData$Temp)
panel.smooth(treeData$Temp, bestM$residuals ) 
# this is how you add a smoothed line to an existing base R plot. There is clearly no trend or pattern, residuals evenly spread across range--> Assumption satisfied

plot(bestM$residuals ~ treeData$Precip)
panel.smooth(treeData$Precip, bestM$residuals ) # No trend here either --> Assumption satisfied

# We also want to do the same for covariates not selected in final model as much as possible, to ensure there are no patterns there as well. A pattern would indicate that perhaps that covariate should in fact be in the model

plot(bestM$residuals ~ treeData$PET) 
panel.smooth(treeData$PET, bestM$residuals ) # --> Assumption satisfied

### 7 - No autocorrelation of residuals----

### Check for spatial autocorrelation - ONLY IF YOU HAVE SPATIAL DATA!!!
#### In this case we do in fact have spatial data. Every sample is a data cell with coordinates.

# let's do some GIS!
treeData$residuals <- bestM$residuals # first let's add a column containing the residuals to our original data set

library(sp) # load the library we need to do GIS - install this package if you have not yet used it on your machine

treeData.sp <- treeData # save a copy of our data, which we will turn into a spatial dataframe

coordinates(treeData.sp) <- c("X", "Y") # this turns treeData.sp into a spatial layer by indicating which columns represent the spatial coordinates

plot(treeData.sp) # we can see the center points of the cells that were characterized as forest

bubble(obj = treeData.sp, 
       zcol = "residuals", 
       scales = list(draw = TRUE)) 

# This type of plot changes the size of the point, based on some column value, in this case we picked the column called 'residuals', which is in our spatial data frame. Although hard to see on such a big scale, there does certainly seem to be spatial correlation in residuals, with some areas clustered in red (left of plot) where all the residuals in that area are negative, and other areas clustered green. This implies there are some important covariates we have not included that are likely influencing forest cover in those locations. This makes sense, given that there are very likely more factors than temperature and precipitation that influence forest cover in a location.

# We can also look at a spline correlogram to visualize potential spatial autocorrelation. This function can take a while to run, so I'm only selecting 100 resamples, but one would probably want to use something like 1000 to get the most accurate representation relationships across the spatial region.

Correlog <- spline.correlog(x = treeData$X,
                            y = treeData$Y,
                            z = bestM$residuals,
                            latlon = T,
                            resamp = 50)

plot(Correlog, main = "Correlogram")
abline(v = Correlog$real$x.intercept) 

Correlog$real$x.intercept

# This x.intercept value is the lowest value at which the function is = 0, that is, the distance at which values are no more similar than expected by chance alone. Here it is at 1005 km. Although we can see the confidence bounds overlap zero before this point. This means there is evidence of spatial autocorrelation out to 1005m from a sample point. Points within this distance are more similar (in their residual value) than would be expected.

# Ideally, in these correlograms, you would see the lines always at or near zero, starting with zero distances. If not, your residuals are spatially autocorrelated, at least up to a certain distance, and this implies there is some covariate acting on your response data that you have failed to account for. In this case perhaps it is soil type, or some other geologic variable.

# There is another package that creates similar figures, but uses Moran's I statistic, which measures the level of spatial correlation. In the figure below, observations are binned into distance categories, and the Moran's I value is calculated. If it is significantly different from zero it will be colored red. This is helpful because it shows whether you have a problem, from a statistical significance standpoint, but it is harder to determine what the actual problematic distance is, below which you can observe spatial autocorrelation. 


Moran_plot <- correlog(coords = data.frame(x = treeData$X,
                            y = treeData$Y),
                 z = bestM$residuals,
                 method = "Moran")
plot(Moran_plot)


# Remember that with time-series data, you would also want to check for temporal autocorrelation. Check out the function acf() for this. Your assignment this week will have you adding some complexity (grazing factor) to your investigation of bird abundance in forest fragments, and practicing these model diagnostics (except for the spatial autocorrelation) on your final model.

