
# Create by Ernest Fotsing, PhD
# GETTING STARTED

# data(are in'02_water_cons.txt' located in CourseData folder)
# readthemintoRusing

xdata=read.table("./CourseData/02_water_cons.txt",header=T,sep="\t")
str(xdata)

# motivationofthemodel
# a stats teacher drinks lots of water while teaching and the question is whether her/his water consumption is influenced by the number of participants
# data
# predictor:number.participants
# response:liters.per.hour
# totalsamplesize:22


# INSPECT THE DATA

# asafirststep,inspectthedistri-butionofthepredictorandtheresponse
hist(xdata$number.participants)
# =>looksokay(1)
hist(xdata$liters.per.hour)

# =>looksokay

# THE FUNCTION lm is use for simple linear mODEL IN R

# FIT THE MODEL

res=lm(liters.per.hour~number.participants,data=xdata)

 # CHECKS OF ASSUMPTIONS

# normality of residuals
# getresiduals using
residuals(res)
# checknormalityofresiduals:
hist(residuals(res), probability=T)
# add a line showing the respective normal distribution:
x=seq(from=min(residuals(res)), to=max(residuals(res)), length.out=100)
lines(x=x, y=dnorm(x, mean=0, sd=sd(residuals(res))))
# are not really normal but neither very skewed or with utliers(?okay)

# CHECKS OF ASSUMPTIONS

# aqq-plot gives view of the residual distribution
# use
qqnorm(residuals(res));qqline(residuals(res))
# it suggests that smallv alues are too large (compare also with histogram on previous page)
# inanidealcaseallpointswouldfallonastraightline

# CHECKS OF ASSUMPTIONS

# homogeneity of residuals
# get fitted values using fitted(res)
# plot residuals against the fitted values:
plot(x=fitted(res), y=residuals(res), pch=19)
# no obvious'pattern'should bevisible
# =>seemsokay
# CHECKS OF ASSUMPTIONS

# one may also correlate absolute residuals withfitted values
cor.test(fitted(res),abs(residuals(res)))
# reveals
# ...
# t=1.0632,df=20,p-value=0.3004
# ...
# cor
# 0.2312891
# the correlation coefficient should be close to 0 and not significant?
# =>seemsokay

# CHECKS OF ABSENCE OF INFLUENTIAL CASES

# influence diagnostics,DFFit:
# compare fitted values between model using all data and model with cases exclude doneatatime
dffits(res)
 # reveals standardized DFFit-values
# the argument(hereres)is the result of a linear model
# anabsolutevalue>~2isareasontoworry
max(abs(dffits(res))) #reveals0.89 value > ca. 2

# get the distribution of the DFFits:
hist(dffits(res))
# incaseyouwantDFFitsonthescaleoftheresponseaskme
# ifyouwanttolocateasinglelargeorsmallvalue,use
which.max(dffits(res))#,or
which.min(dffits(res))#,or
which.max(abs(dffits(res)))
# which.maxreturnsthepositioninthevectorwherethemaximumoccurs


# influencediagnostics,DFBeta:
# comparemodelcoefficientsbetweenmodelusingalldataandmodelwithcasesexcludedoneatatime
# dfbeta(res)revealsunstandardizedDFBetavalues(differencebetweenestimatesderivedfromalldataandwithcasesexcludedoneatatime)
head(dfbeta(res))
#reveals
# (Intercept)number.participants
# 1  -0.0029749           0.0000840
# 2   0.0009189          -0.0000820
# 3  -0.0024179          -0.0000443
# 4  -0.0048567           0.0000720
# 5  -0.0019383           0.0000464
# 6   0.0086773          -0.0005203
# =>comprisesonecolumnforeachestimatedparameterandonerowforeachdatapoint)

# CHECKS OF ABSENCE OF INFLUENTIAL CASES


# influencediagnostics,DFBeta:
# tocomparetheestimatedcoefficientsbasedonalldatawiththerangeofestimatesderivedfromdatadroppingcasesoneatatimeuse
round(cbind(coefficients(res), coefficients(res)+ t(apply(X=dfbeta(res), MARGIN=2, FUN=range))), 5)
# => looks good (little variation)
# CHECKS OF ABSENCE OF INFLUENTIAL CASES

# influencediagnostics,Cook'sdistance:
max(cooks.distance(res))#reveals0.34
# severalthresholdswererecommended:
# values>1areareasontoworry
# values>4/n(here4/22=0.18)arereasontoworry
# values>Fk,n-k,1-aareareasontoworry
# canbedeterminedusing
qf(p=1-0.05,df1=2,df2=20,lower.tail=T)
# whichreveals3.49

# CHECKS OF ABSENCE OF INFLUENTIAL CASES

# influence diagnostics, leverage:
max(as.vector(influence(res)$hat)) #reveals 0.17
# values > 2*(k+1)/n or > 3*(k+1)/n are a reason to worry(1)
# with
# k = number of predictors (here 1, or 
length(coefficients(res)) #)  n = number of cases (here 22, or 
length(residuals(res))
# reveals 0.18or 0.27, respectively
# => is okay

# CHECKS OF ABSENCE OF INFLUENTIAL CASES
# CHECKS FOR ASSUMPTIONS AND INFLUENTIAL CASES

# generally,also plots of the various diagnostics might be helpful
plot(residuals(res))
plot(dffits(res))
plot(dfbeta(res)[,1])#etc.(needstobedonebycolumn)
plot(cooks.distance(res))
plot(as.vector(influence(res)$hat))

# a potentially interesting function is identify(<?>)
identify(as.vector(influence(res)$hat))


# SAVING THE WORKSPACE

# for now we are fine with the model:
# =>noobviousviolationsofitsassumptions
# =>noobviouslyinfluentialcases
# butwhataretheresults?
# somemoretheoryisneededforthat?
# butbeforethat:
# 
# savetheworkspace(wewilllaterneedtocontinueworkingwithit):
getwd()
save.image("./water_cons.RData")#(don'tforgettoaddtheextension!)

diagnostics.plot<-function(mod.res){
	old.par = par(no.readonly = TRUE)
	par(mfrow=c(2, 2))
	par(mar=c(3, 3, 1, 0.5))
	hist(residuals(mod.res), probability=T, xlab="",
		ylab="", main="")
	mtext(text="histogram of residuals", side=3, line=0)
	x=seq(min(residuals(mod.res)), max(residuals(mod.res)), 		length.out=100)
	lines(x, dnorm(x, mean=0, sd=sd(residuals(mod.res))))
	qqnorm(residuals(mod.res), main="", pch=19)
	qqline(residuals(mod.res))
	mtext(text="qq-plot of residuals", side=3, line=0)
	plot(fitted(mod.res), residuals(mod.res), pch=19)
	abline(h=0, lty=2)
	mtext(text="residuals against fitted values", side=3,
		line=0)
	par(old.par)
}


mean(1:10)



#LINEAR REGRESSION

res=lm(liters.per.hour~number.participants,data=xdata)


# RESULTS

# first,getasummaryoftheresultofthelinearmodel
(sres=summary(res))

# typingsresreveals,besidesotherthings,overallstatisticsandstatisticsfortheindividualpredictors

# Call:
# lm(formula = liters.per.hour~ number.participants, data = xdata)
# Residuals:
# Min        1Q    Median        3Q       Max 
# -0.098981 -0.058333 -0.006507  0.048934  0.118071 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         0.655187   0.040951  15.999 7.28e-13 ***
# number.participants0.009015   0.001541   5.848 1.01e-05 ***
# ---
# Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
# Residual standard error: 0.06592 on 20 degrees of freedom
# Multiple R-squared:  0.631,     Adjusted R-squared:  0.6126 
# F-statistic:  34.2 on 1 and 20 DF,  p-value: 1.012e-05


# resultscanbereadandcopiedfromthescreen(usingtheuglymouse),butthereisabetterway?
# names(sres)revealsthenamesoftheobjectsbeingpartofsres typing
names(sres)reveals
# "call""terms""residuals""coefficients""aliased""sigma""df""r.squared""adj.r.squared""fstatistic""cov.unscaled"
# thesecanbeaccessedusings res$<name>
# extracttheR2asaneffectsize
# use
sres$adj.r.squared #togetaneffectsize(R2)forthefullmodelwhichisadjustedforthenumberofpredictorsinit(reveals0.61

sres$coefficients #displaystheresultsfortheindi-vidualpredictors
round(sres$coefficients,digits=3)#showsthemsomewhatnicer
# results:number.participantsisclearlysignificant
# =>waterconsumptionincreasedbyca.0.01litersperadditionalparticipant
# youcansavethecoefficientsetc.using
write.table(x=sres$coefficients, file="clipboard", row.names=T, col.names=T, sep="\t")

# theinterceptisthepredictedwaterconsumptionforacoursewithzeroparticipants?
# isthataverymeaningfulvalue?
# no,whentherearezeroparticipantsthereisnocourse
# plus:0istotallyoutsidetherangeofthepredictor(10to37)
# togetamoremeaningfulinterceptonecouldz-transformthepredictorusing
z.num.part=as.vector(scale(xdata$number.participants))
# andthenrunthemodelusing
z.res=lm(liters.per.hour~z.num.part,data=xdata)

round(summary(z.res)$coefficients,3)

# CONFIDENCE INTERVALS FOR THE INDIVIDUAL EFFECTS

# togetconfidenceintervalsfortheestimatesoftheindividualpredictorsuse
confint(object=res)

# withobjectbeingtheresultofthefunctionlmthefunctionconfintreveals95%confidenceintervalsoftheestimatedcoefficients(95%isthedefaultlevel;canbechangedusingtheargumentlevel)

cbind(coefficients(res),object=confint(res))

# reveals
# 2.5%97.5%
# (Intercept)0.65520.56980.7406
# number.participants0.00900.00580.0122
# interpretation:thereislittleuncertaintyintheindividualestimates

# PLOTTING THE RESULT

# toplottheresult,Iwouldbeginwithplottingtheresponseagainstthepredictor:

plot(x=xdata$number.participants, y=xdata$liters.per.hour, las=1, pch=19, xlab="number participants", ylab="liters per hour")
# plot(x,y)revealsascatterplotofyagainstx

# theargumentpchdefinesthesymbol('pointcharacter')
# try
plot(x=1:25,y=rep(x=1,times=25),pch=1:25)
# togetanoverview
# ylab="text",andxlab=...definethelabelsattheaxes
# las=1:printaxistickslabelsupright('labelaxisstyle')
# check?parformanymorearguments/options

# add regression line:
abline(res, lty=2, lwd=6)
# ablineaddsastraightlinebeingdefinedby,e.g.,anintercept(a)andaslope(b)
# notethatthefunctionablinecanextractthemodelcoefficientsfromtheobjectresultingfromacallof
# lm(whenthemodelwasasimpleregression)
# lty=2:meanstogetadashedline


# GETTING CONFIDENCE INTERVALS INTO THE PLOT

# togetaconfidenceintervalofthefittedmodelintotheplot,onefirstneedstoconstructadataframeforwhichpredictionsaretobemade
pred.data=data.frame(
number.participants= seq(from=min(xdata$number.participants), 
                           to=max(xdata$number.participants), length.out=100))

# nextwegettheconfidenceintervalusing
ci.plot=predict.lm(object=res, newdata=pred.data, interval="confidence")
# (thelevelof95%canbechangedusingtheargumentlevel)
# inspectci.plotusing
str(ci.plot)


# toaddthelinesdepictingtheconfidenceintervalofthemodelpredictionsuse
lines(x=pred.data$number.participants, y=ci.plot[, "lwr"], lty=3)
lines(x=pred.data$number.participants, y=ci.plot[, "upr"], lty=3)
# inamatrix(likeci.plot)columnscanbeaddressedbytheirnamesonlyasshownabove
# notethatherethelinedepictingthefittedmodelextendsoverawiderrangethanthelinesshowingtheCI
# thiscouldbefixedbyaddingthelineshowingthemodelusingthefunctionlinesaswell


# SAVING THE PLOT (all OS)

# oneveryOSonecanusethefunctiondev.copy2pdf
# savesthecurrent/activeplot('device')intoapdf-file
# use:
dev.copy2pdf(file="<?>.pdf")
# analternativeisdev.copy2epswhichworkscorrespondingly
# anotheroptionisto'draw'theplotdirectlyintoafile(functionspdf,png,tiff,jpeg,bmp)
# usethemasfollows:
# pdf(file="<filename>")
# plot(<?>)
# <etc>
# dev.off()

#---------------------SECOND EXERCICES-----------------------------
#
##############################Picth---------------------
pitch <- c(233,204,242,130,112,142)
sex <-c(rep("female",3),rep("male",3))
levels(sex)
str(my.df1)
mydf1$sex <- as.factor(sex)

age <- c(20,14,15,30,100,90)
tail <- c(167,178,200,115,199,156)
long <-  c(16,17,20,11,19,39)

rm(my.df1)

#Create a dataframe
my.df1 = data.frame(sex,pitch, age, tail, long)

xmdl = lm(pitch ~ sex, my.df1)


corrM <- my.df1 %>% 
  dplyr::select(pitch, tail, long, age) %>% cor()

str(my.df1)

corrplot(corrM, 
         method = "number", 
         type = "lower", 
         bg = "white", 
         diag = F, 
         tl.col = "black") # intaller corrplot

corrplot(corrM, method = "ellipse", # this is just another method for good visibility
         type = "upper", 
         diag = F)

#Run the linear model

xmdl = lm(tail ~ sex + age + pitch + long, my.df1)

xmdl2<- lm(tail ~ pitch + sex + long, my.df1)

xmdl3<- lm(tail ~ pitch + sex + age, my.df1)


AIC(xmdl)
AIC(xmdl2)
AIC(xmdl3)


summary(xmdl)

install.packages((AICcmodavg))

# create age
age<- c(15, 20, 30, 56, 12, 45)

my.df2 = data.frame(my.df1,age)

xmdl2<- lm(pitch ~ sex + age, data=my.df2) #Multiple linear regression: model one response variable as a function of multiple predictor variables
summary(xmdl2)
plot(xmdl)
abline

#-------------------------LM------------------------
data(trees)

trees<- trees
str(trees)
head(trees)
summary(trees)

lm1 <- lm(Volume ~ Girth,
          data = trees)
summary(lm1)

plot(Volume ~ Girth, data = trees)

abline(lm1, col ="orange",
       lwd = 2)

coefficients(lm1) 

plot(residuals(lm1))
abline(h=0, lty=5, col ="red")
plot(dffits(lm1))
plot(dfbeta(lm1)[,1]) #etc. (needs to be done for each column/estimate, separately)
plot(cooks.distance(lm1))
plot(as.vector(influence(lm1)$hat))

# checks of assumptions
hist(residuals(lm1)) 
qqnorm(residuals(lm1))
qqline(residuals(lm1))


#------------------------

data(mtcars)
input <- mtcars[,c('mpg','cyl')]
print(head(input))
# Give the chart file a name.
png(file = "boxplot.png")
# Plot the chart.
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")
# Save the file.
dev.off()

# Give the chart file a name.
png(file = "boxplot_with_notch.png")
# Plot the chart.
boxplot(mpg ~ cyl, data = mtcars,
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon",
        main = "Mileage Data",
        notch = TRUE,
        varwidth = TRUE,
        col = c("green","yellow","purple"),
        names = c("High","Medium","Low")
)
# Save the file.
dev.off()

# chisqauretest
library("MASS")
print(str(Cars93))
# Create a data frame from the main data set.
car.data <- data.frame(Cars93$AirBags, Cars93$Type)
# Create a table with the needed variables.
car.data = table(Cars93$AirBags, Cars93$Type)
print(car.data)
# Perform the Chi-Square test.
print(chisq.test(car.data))

#------The result shows the p-value of less than 0.05 which indicates a strong correlation.
#install.packages("party") # for decision trees 


