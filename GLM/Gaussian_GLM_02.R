#---------------------
# erxercide develpped by Ernest Fotsing , PhD


mice<-data.frame (stillb=c(15, 17, 22, 38, 144), total=c(297, 242, 312, 299, 285), conc=c(0, 62.5, 125, 250, 500))

mice$resp <- cbind(mice$stillb, mice$total-mice$stillb)

mice.glm <- glm(resp ~ conc, family = binomial(link = logit), data = mice)

summary(mice.glm)

#----------------------------------
#structure of the data/model motivation:
stress hormone levels (cort) of wild individuals of a primate species
were measured from feacal samples1 we assume that individuals are 
more stressed closer to roads (dist2road), 
due to increased hunting pressure we further want to 
control for temperature (temp) and food availability (fai) 
which we both assume to have a negative effect on
stress levels

file_path <- "./CourseData/03_cort.txt"
# Read the data from the file
xdata<- read.table(file_path, header = TRUE, sep = "\t", quote = "\"", dec = ",")

str(xdata)
names(xdata)
# convert in numeric

xdata$dist2road<- as.numeric(xdata$dist2road) 

xdata$fai<-  as.numeric(xdata$fai) 

xdata$temp <- as.numeric(xdata$temp) 

xdata$log.cort <- as.numeric(xdata$cort)

str(xdata)

#distribution of the response

hist(as.numeric(xdata$cort))

#distributions of the predictors
hist( as.numeric(xdata$dist2road)) #looks good

hist(as.numeric(xdata$fai)) #looks good

hist(as.numeric(xdata$temp)) #looks good

#distribution of the response

hist(as.numeric(xdata$cort)) #  is pretty skewed

# ----------------do histogramm once-----------------
test.vars=c("dist2road", "fai", "temp", "log.cort")
par(mar = c(5.1, 4.1, 4.1, 2.1))# defaults margin in R 
par(mar = c(3, 3, 3, 3))# smaller margins
par(mfrow=c(3, 2))

for(i in 1:length(test.vars)){
  #par(mfrow=c(2, 2))
  hist(xdata[, test.vars[i]], main=test.vars[i])
}

#suggests to try transforming cort. I’d try a log-transformation first
#can one just log-transform a variable?
  
#the logarithm is defined only for values ≥ 0, and for 0 it is -∞ hence, check 
min(xdata$cort) #3(reveals 0.52)
#also, first check whether log-transforming has the desired effect:
  
  hist(log(as.numeric(xdata$cort))) #look good
  
xdata$log.cort=log(as.numeric(xdata$cort))# create object
  
#overwriting a vector
xdata$cort<- log(as.numeric(xdata$cort))
 
#or creating a new one
  
xdata$log.cort <- log(xdata$log.cort)
  
#inspecting the data
 # check correlations among the predictors using
  
  pred<-  xdata[, 1:3]
  par(mfrow=c(1, 1))
  
  corrM <- xdata %>% 
    dplyr::select(dist2road, fai, temp) %>% cor()
  
  corrplot(corrM, 
           method = "number", 
           type = "lower", 
           bg = "white", 
           diag = F, 
           tl.col = "black")
  
  corrplot(corrM, method = "ellipse", # this is just another method for good visibility
           type = "upper", 
           diag = F)
  
vars=c("dist2road", "fai", "temp")
xx=cor(xdata[, vars])
  
upper.tri(xx) #reveals a matrix indicating TRUE for those cells in
  #xx which are above the diagonal
  #indexing xx with upper.tri(xx) reveals those values in xx which
  #are above the diagonal
  xx[upper.tri(xx)]
  [1] -0.15445349 0.13941907 -0.06650747
 
  # now it is easy to get the largest absolute correlation:
    max(abs(xx[upper.tri(xx)]))
  #[1] 0.1544535 # but absence of a large absolute correlation is not equating to absence of a collinearity issue (’absence of evidence is not evidence of
                                                                                                absence’) => one more collinearity check is required (see below)
 
  #fitting the model now we can fit the model:
    
  full=glm(log.cort~dist2road+fai+temp, family = gaussian, data=xdata)
  full1=lm(log.cort~dist2road+fai+temp, data=xdata)
  
  summary(full)
  
  #  normally distributed and homogeneous residuals
  # to get all diagnostics plots at once, source a file I provided
  # source("./CourseData/diagnostic_fcns.r")
  # diagnostics.plot(full)  
  
   # checks of assumptions
  hist(residuals(full)) 
  qqnorm(residuals(full))
  qqline(residuals(full))
  
  #2 homogeneity of residuals:
  
  fitted(full)
  
  #3plot residuals against fitted values:
  
  plot(x=fitted(full), y=residuals(full),
       pch=19)
  abline(h=0, lty=3)
  
  
  #checks of absence of influential cases
  my.dfbeta<-function(m){
    xx=cbind(coef(m), coef(m)+
               t(apply(X=dfbeta(m), MARGIN=2, FUN=range)))
    colnames(xx)=c("orig", "min", "max")
    return(xx)
  }
  
  round(my.dfbeta(full), 3)# looks good
  
#absence of collinearity
 
  library(car)

#installation is required only once, loading once in each session
  max(vif(full))
dist2road fai temp
  #1.042328 1.026608 1.021983 does not suggest existence of a collinearity problem
  #remember the rules of thumb:
    - a VIF>10 indicates a problem
  - an average VIF ’substantially in excess’ of 1 indicates a problem
  - my experience: a VIF>3 to 4 may indicate a problem
  one should be pretty cautious about collinearity
  
  
  #plotting the result for temperature
  #so we should better fit a model with the other two predictors being centered (to a mean of zero):
    
xdata$z.fai=as.vector(scale(xdata$fai))
    
xdata$z.temp=as.vector(scale(xdata$temp))
  
plot.res=glm(log.cort~dist2road+z.fai+z.temp, data=xdata)
  
#and then create the plot based on this model:
    
par(mar=c(3, 3, 0.2, 0.2), mgp=c(1.7, 0.3, 0), las=1, tcl=-0.15)

plot(x=xdata$dist2road, 
     y=xdata$log.cort, 
     las=1, 
     yaxt="n",
     xlab="Distance to roads", 
     ylab="Cortisol level",
     # Add the cex argument here
     cex = 0.5, col="purple"  # Sets the dot size to 70% of the default
)
y.at=2^(0:9)

axis(side=2, at=log(y.at), labels=y.at, las=1)
#points(x=xdata$dist2road, y=xdata$log.cort, pch=16)

  #getting confidence intervals into the plot  
  pred.data=data.frame(
    dist2road=seq(from=min(xdata$dist2road),
                  to=max(xdata$dist2road), length.out=100),
    z.fai=0, z.temp=0)
  
  ci.plot=predict.lm(object=plot.res, newdata=pred.data,
                     interval="confidence") 
  
  polygon(
    x=c(pred.data$dist2road, rev(pred.data$dist2road)),
    y=c(ci.plot[, "lwr"], rev(ci.plot[, "upr"])),
    border=NA, col="grey")
  
  abline(a=coefs["(Intercept)"],
         b=coefs["dist2road"], lty=2, col="Black", lwd="2")
  
  lines(x=pred.data$dist2road, y=ci.plot[, "lwr"], lty=3, col="red", lwd="2")
  lines(x=pred.data$dist2road, y=ci.plot[, "upr"], lty=3, col="red", lwd="2")  
  
  #a plot for a presentation now one can add the model line and the data:
  #lines(x=pred.data$dist2road, y=ci.plot[, "fit"], lwd=1, lty=2)
  
  