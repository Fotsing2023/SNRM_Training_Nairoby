#______________________________________________
### Introduction to Linear Models
### Exercise developed by Ernest Fotsing, PhD
# 
### Objectives:   
# 1. Practice code for importing and exploring a dataset
# 2. Demonstrate implementation of basic linear models
# 3. Practice use of coefficients to predict novel response values
#
### DataSets used in Script:
# Global Climate Data ("temperature.csv") located in CourseData

# MetaData: Although there are various columns here, we are primarily interested in just a few:
# YEAR: Mean annual temperature in degrees Celsius
# ELEV: Elevation in meters of the weather station
# LAT: Latitude of the weather station
# LON: Longitude of the weather station
# CNTRY_NAME: Name of the weather station country
# NAME: Name of the weather station

# Broadly, this dataset contains temperature, location and elevation data for many weather stations around the world, and can be used to look at patterns in elevation (and location) with climate. Data provided by Sven Lautenbach, unsure of initial origin. 

# Activities: Subset climate data for a particular country and analyze the effect of elevation on annual temperature

# Clean Environment and Load Packages -------------------------------------------

rm(list=ls()) # we will use this at the start of every script to clean what is in our environment.

library(tidyverse) # this gives us a lot of excellent functionality for data manipulation. It also includes ggplot2.

# Data Import and Preparation -------------------------------------------------------------

# Remember your data files should all be located in your "data" folder within your primary course folder. So we will always find data files in the same place. Be sure to open Rstudio by clicking on your project file, or select your project in the upper right corner of the Rstudio window.

getwd() #Check to make sure you are in the same folder that your project file sits in

# Note that now we are using read.csv because this data is stored as a csv file. We could also use read_csv if we wanted this to be a tibble instead of a data frame. This is what tidyverse functions are designed to work with.
tempData <- read.csv("./CourseData/temperature.csv")

summary(tempData) # notice that the minimum value for elevation is -999.00. In climate data, -999.00 is often used to represent missing values. If we end up with these values in our final dataset, we would need to remove them before proceeding. 

# There are quite a few columns here. We are mostly interested in yearly average temperature ("YEAR") and site elevation ("ELEV") as well as country name ("CNTRY_NAME"). To practice data manipulation, let's remove the columns called "AREA" and "PERIMETER" since they have no information in them.

names(tempData) # shows the names of all the columns in this object

tempData <- tempData[ , -c(4,5)] # this removes the 3rd and 4th columns. Remember that using vector notation (the brackets) on a dataframe or matrix, we refer to a portion of the object by: [rows, columns]. Here, I'm indicating all rows, since I provide no row notation, and the removal of columns 3 and 4. This code isn't ideal though, partly because if you run it twice by accident, you will keep removing additional columns.

# Using a tidyverse function (select) we can remove column X, FID_1, FID_2 and TEMP_STA_1. This method is a bit safer because it uses the actual name of the column instead of the column number, which could change if you change your data file. Rerunning this line of code will not accidentally delete other columns of data.
tempData <- select(tempData, -c(X, FID_1, FID_2, TEMP_STA_1))

# We can also use this function to remove a whole group of columns. Just as we type 1:5 to give a sequence from 1 to 5, we can use the same notation here. This removes all the columns from "JAN" to "DEC". 
tempData <- select(tempData, -(JAN:DEC)) #very handy! We don't need any of those monthly columns here.

#Feel free to practice removing columns here, as long as you keep the important ones listed above!

#Finally let's rename our YEAR column as this name is confusing. Here I'm using the function 'rename' from tidyverse, putting the NewName = OldName. We have to remember to write over our object.

?rename #check help menu to reiew the use of this function

tempData <- rename(tempData, temp = YEAR)



# Subset and Explore the Data ---------------------------------------------------------

dim(tempData) # in total 6039 stations worldwide. 6039 rows and 11 columns.
str(tempData) 

# Above we used the tidyverse function select() to identify columns to include or remove. The equivalent tidyverse function for rows is filter(). Here we simply need to provide a logical statement that will indicate what rows we want to keep or remove.

# In this case I'd like to focus the analysis on stations in Germany alone.

temp.ge <- filter(tempData, CNTRY_NAME == "Germany")

dim(temp.ge) # we have now only the 116 stations in Germany

# Explore and Plot the Data -----------------------------------------------------------

hist(temp.ge$ELEV) # Because there is a single value relatively far from the rest we could consider log10 transforming elevation, but in this case it is likely not necessary, as the gap is not terribly large. Remember the log or log10 transformation is not used to bring together data that is simply spread over a wide range of values, or to address skewed data, but to deal with very large gaps of no data.

hist(temp.ge$temp) # here again we see a gap in the data. Although this is all admittedly subjective, I consider this to be on the edge of where I would transform this data. In this case I will leave it as is.

# let's plot our data, looking at elevation vs. annual temperature
ggplot(data = temp.ge, 
       mapping = aes(x = ELEV, 
                     y = temp)) +
  labs(y = "Yearly average temperature [C]",
       x = "Elevation [m]") +
  geom_point()

# There is an outlier here, but it does not seem to be an error. We can confirm the location name of this point, pulling out the value in the column called "NAME" where the value for ELEV is greater than 2500 meters. The which function is very handy and returns always the row number(s) that meet your logical statement. Because this returns a row number, we can use that inside the vector notation brackets to call that row from the NAME column. Highlight and run the code inside the brackets to confirm what it is doing.
temp.ge$NAME[which(temp.ge$ELEV > 2500)]

# this does the same thing in tidyverse code with a pipe

temp.ge %>% 
  filter(ELEV > 2500) %>% 
  select(NAME)

# If we google this name, we see this is the highest peak in Germany, so the high elevation and cold average yearly temperature make sense.


#Question: What is the likely intercept of a linear model in this scenario? What is the likely slope value? Take a minute to estimate these.


# Fitting a Linear Model --------------------------------------------------
# We want now to fit a model that uses elevation data to predict the average annual temperate at a given location in Germany.

lm1 <- lm(temp ~ ELEV, 
          data = temp.ge) # running a linear model really is that easy!

print(lm1) # shows the regression coefficients. Can also use function "coef"

coef(lm1)

# Let's take a second to look at what this R object is that we've created: lm1

class(lm1) # this is a specific type of R object for linear models. If you look in your environment you'll see it is a list of 12 items. In R a list is like a cabinet full of drawers. We can put anything we want in those drawers, including single values, vectors, matrices, data frames, or even other lists. We can pull out those items with a "$", just as we pull columns from a data frame.

names(lm1) # we can see the names of each item in the list using "names", just as we do for data frame columns. In fact, a data frame is a specific type of list!

# Coefficient values in the model are stored in the first item (or drawer) of the list, and it is called "coefficients". So we can pull those coefficient values directly from the list object like this:
lm1$coefficients

# We can do this for any of the many values and results in the model object, but the "summary" function does a lot of this for us, and makes it look nice as well.

# View Regression Model Output Summary ------------------------------------

summary(lm1)

# As expected, temperature drops with elevation, starting from a mean temperature of 9.7C at sea level (ELEV = 0, also the intercept by definition). Temperature drops by 0.004C per 1 unit (meter) of elevation. We could also say the temp drops by 0.42C per 100m elevation. Based on the p-value, it is highly unlikely we would find a t-value of -21.34 if the slope was actually zero. The model explains about 80% of the variance in the temperature data. We won't often get R2 values this high in ecological research.


# Plot the Regression Line with the Data ----------------------------------

ggplot(data = temp.ge, 
       mapping = aes(x = ELEV, 
                     y = temp)) +
  labs(y = "Yearly average temperature [C]",
       x = "Elevation [m]",
       title = "German stations" ) +
  geom_point(shape = 21, 
             fill = "tomato", 
             size = 3) +
  geom_abline(intercept = lm1$coefficients[1], 
              slope = lm1$coefficients[2])

# Note here in ggplot that I've done a few new things. First I added a title to the plot within the labs() function. I've also used the geom_abline() function which plots a line, if you provide the "a" and "b" or, intercept and slope. This is only useful for simple models like this with one predictor. Note how I'm using vector notation to provide the first and second items in the "coefficients" drawer of the lm1 list.  

# You can also use geom_hline or geom_vline to plot horizontal and vertical values.

# Use Predict Function to Predict Values in Novel Situations --------------

# Our regression equation allows us to predict the temperature value at any given elevation. We can manually write out the regression equation to calculate a given value, or we can use the predict() function. We'll see later when you have multiple covariates, using the predict function with a new dataframe is essential for visualizing your results.

# Manual use of regression equation

# Remember our equation for a line is a + bx
9.7178381 - 0.0041944 * 2500 # predicted temperature at 2500m, where we have no stations. That is cold.

# I can also pull out the coefficients of the linear model object (lm1) with the helper function coef(), and use this to write my equation.

coef(lm1) 

coef(lm1)[1] + (coef(lm1)[2] * 2500) # Here I am calling the first value for our coefficients to get the intercept, and the second value to get the slope. Note that this would work, even if we changed our linear model. It is always good practice to write code in this way, so that changes made above in your script will not impact the code you've written below.

# To use predict, we create a new data frame, with the value(s) over which you want to predict temperature. The new data frame needs the same information that we fed into the lm formula. The name of the column(s) here must be exactly the same as in the regression equation. Here this is a basic example, we are just making a dataframe with 1 column, and one value.

mydf <- data.frame(ELEV = 2500)

predict(lm1, newdata = mydf)

# Note that we can also give the predict function a vector of values to predict over. We can use "seq" to define a sequence of values, and we supply the start, the end, and the # of values to create. See ?seq. Here we are predicting yearly average temperature values for 100 points, evenly spaced between 1 and 2500m elevation. As before we put our values into a data frame, naming our column exactly as the covariate is named in the model.

new.elev <- data.frame(ELEV = seq(1, 2500, 
                                  length.out = 100))
View(new.elev)

# this is doing the prediction for the value range defined above

lm1.pred <- predict(lm1, 
                    newdata = new.elev)

View(lm1.pred) #100 predicted values of temperature, for your 100 defined values of elevation

# We can also turn on the confidence intervals as an argument to get this information along with our predicted/fitted values.

lm1.pred <- predict(lm1, 
                    newdata = new.elev, 
                    interval = "confidence")
View(lm1.pred)

# We will be using the predict function a lot in this course to create response curves and visualize the result of sometimes complicated regression models. Take the time to make sure you understand what this function is doing and how we are feeding information to it. Understanding this code will be critical as our models become more complicated.

# This concludes the demonstration script. Be sure to check for this week's assignment, which asks you to replicate this type of analysis for climate stations in the USA.
# 
# 


# ---------------------Ercercices-------------------------------------
# 
Exercise Instructions
In a new and appropriately-named
script file, follow the linear regression
workflow demonstrated to analyze the
climate data, this time for only stations in the United States. The ultimate objective is to determine how
well elevation predicts annual temperature in this region, and to use the regression model to predict
annual temperature for Front Royal, VA.

Proceed through the entire analysis workflow and answer the following questions


Study Questions

1. What is the regression (beta) coefficient for elevation? 
2. What is the predicted change in annual temperature, with a 100 m increase in elevation? 
3. How much variation in temperature is explained by elevation in the US? 4. Using ggplot, generate a scatterplot of temperature vs. elevation data. Include your fitted regression
line and give appropriate labels for both axes. 
5. Use your regression equation to predict the annual average temperature for Front Royal, VA, which is at an elevation of 170m. 