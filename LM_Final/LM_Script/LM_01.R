

title: "Statistical Analysis in R"
author: "Ernest Fotsing, PhD, University of Fribourg"
date: "`r format(Sys.time(), "Last modified: %d %b %Y")`"

#-------SNRM course, developped by Ernest Fotsing, PhD, University of Fribourg

# define the working directory 
# get WRD
getwd()

wrd <- "C:/Users/Fotsing Ernest/OneDrive/Bureau/SNRM_Training/R_Precourse_ErnestFotsing/LM/SNRM_Training_Nairoby"

# get all files in my wdr
list.files(wrd)

#------------------First let do basic of plotting---------------------
#1 ------first dowload the code from the repository online----

# Define the URL of the Excel file
url <- "https://github.com/Fotsing2023/SNRM_Training_Nairoby/blob/main/Basic_Plotting00.R"

# Define the file path where you want to save the Excel file
file_path <- "Basic_Plotting00.R"

# Download the Excel file
download.file(url, file_path, mode = "wb")

#-------------Here we're going into modeling and I assume that you already did the previous and are able to deal with installing packages and read data

# see the content of your files
list.files("./CourseData/")

# check if diet data exist
file.exists("./CourseData/diet.csv")

# read data
diet <- read.csv("./CourseData/diet.csv")
head(diet)

summary(diet)

# asess some variable
diet$gender
diet$age

# create new columns based on existing ones

diet$weight.loss <- diet$final.weight - diet$initial.weight


#Subsetting rows and columns is done using the `[rows, columns]` syntax; where `rows` and `columns` are *vectors* containing the rows and columns you want

diet[1:5,]
diet[,2:3]
diet$diet.type == "A"

dietA <- diet[diet$diet.type == "A",]
dietA


## Visualisation

#All your favourite types of plot can be created in R
# a histogram is commonly-used to examine the distribution of a particular variable

hist(diet$weight.loss)

boxplot(diet$weight.loss~diet$diet.type)

#scatter plots can be constructed by given two vectors as arguments to `plot`

plot(diet$age,diet$initial.weight)

boxplot(diet$weight.loss~diet$diet.type, 
        ylab="Weight Loss", 
        xlab="Diet Type",
        col=c("yellow","blue","red"),
        main="Weight Loss According to diet type")
#get help on any of the functions that we will be using in this course by using the '?' 

help(lm)

# -----------------------------
# Section 1: Descriptive analysis
# -----------------------------

# Import dataset (overwrite previous one)
diet = read.csv("./CourseData/diet.csv", row.names = 1)

# Define new column: weight loss
diet$weight.loss = diet$initial.weight - diet$final.weight 

# Factorize diet type and gender
diet$diet.type = factor(diet$diet.type, levels = c("A", "B", "C"))
diet$gender    = factor(diet$gender, levels = c("Female", "Male"))

# Boxplot of weight loss per diet type
boxplot(weight.loss ~ diet.type, data = diet, col = "light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h = 0, col = "blue")


# -----------------------------
# Section 2: ANOVA
# -----------------------------

# Fisher's ANOVA
diet.fisher  = aov(weight.loss ~ diet.type, data = diet)# nor dist and var equal
summary(diet.fisher)

# Welch's ANOVA
diet.welch   = oneway.test(weight.loss ~ diet.type, data = diet)# nor dist and var unequal
print(diet.welch)

# Kruskal-Wallis ANOVA
diet.kruskal = kruskal.test(weight.loss ~ diet.type, data = diet)# non normdist
print(diet.kruskal)

# Compare Diet A vs Diet C using Fisher and t-test
summary(aov(weight.loss ~ diet.type, data = diet[diet$diet.type != "B", ]))
t.test(weight.loss ~ diet.type, data = diet[diet$diet.type != "B", ], var.equal = TRUE)


# -----------------------------
# Section 3: Model check
# -----------------------------

# Mean and median weight loss per group
mean_group   = tapply(diet$weight.loss, diet$diet.type, mean)
median_group = tapply(diet$weight.loss, diet$diet.type, median)
mean_group
median_group

# Residuals
diet$resid.mean   = diet$weight.loss - mean_group[as.numeric(diet$diet.type)]
diet$resid.median = diet$weight.loss - median_group[as.numeric(diet$diet.type)]
diet[1:10, ]

# Boxplot and QQ plot of residuals
par(mfrow = c(1,2), mar = c(4.5, 4.5, 2, 0)) 

# Boxplot of residuals
boxplot(resid.mean ~ diet.type, data = diet, main = "Residual boxplot per group",
        col = "light gray", xlab = "Diet type", ylab = "Residuals")
abline(h = 0, col = "blue")

# QQ plot of residuals
col_group = rainbow(nlevels(diet$diet.type))
qqnorm(diet$resid.mean, col = col_group[as.numeric(diet$diet.type)])
qqline(diet$resid.mean)
legend("top", legend = levels(diet$diet.type), col = col_group, pch = 21, ncol = 3, box.lwd = NA)

# Shapiro-Wilk test for normality
shapiro.test(diet$resid.mean)

# Bartlett's test for equal variances
bartlett.test(diet$resid.mean ~ as.numeric(diet$diet.type))


# -----------------------------
# Section 4: Multiple comparisons
# -----------------------------

# Tukey HSD test
plot(TukeyHSD(diet.fisher))

# Compare CI for Diet A vs B using t-test
t.test(weight.loss ~ diet.type, data = diet[diet$diet.type != "C", ], var.equal = TRUE)

# -----------------------------
# Section 5: Two-way ANOVA
# -----------------------------

# Two-way ANOVA using aov
diet.fisher = aov(weight.loss ~ diet.type * gender, data = diet)
summary(diet.fisher)

# Two-way ANOVA using lm
anova(lm(weight.loss ~ diet.type * gender, data = diet))


# -----------------------------
# Section 6: Practicals
# -----------------------------


# Section 5: Practicals

Analyse the two following datasets with the suitable analysis:
  
  ## (i) *amess.csv*
The data for this exercise are to be found in *amess.csv*. The data are the red cell folate levels in three groups of cardiac bypass patients given different levels of nitrous oxide (N2O) and oxygen (O2) ventilation. (There is a reference to the source of this data in Altman, Practical Statistics for Medical Research, p. 208.)
The treatments are

* 50% N2O and 50% O2 continuously for 24 hours 
* 50% N2O and 50% O2 during the operation
* No N2O but 35-50% O2 continuously for 24 hours

## (ii) *globalBreastCancerRisk.csv*

The file *globalBreastCancerRisk.csv* gives the number of new cases of Breast Cancer (per population of 10,000) in various countries around the world, along with various health and lifestyle risk factors. 

Let’s suppose we are initially interested in whether the number of breast cancer cases is significantly different in different regions of the world.

Visualise the distribution of breast cancer incidence in each continent. Check how many observations belong to each group (continent). Are there any groups that you would consider removing/grouping before performing the analysis ? 
  
#