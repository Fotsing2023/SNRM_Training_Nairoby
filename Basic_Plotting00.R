
#Script developped by Ernest Fotsing, PhD SNTM Nairoby
# Basic plotting in R 
# dowload the script from git

# Define the URL of the Excel file
url <- "https://github.com/Fotsing2023/Ernest_UY1ModelingTraining/raw/main/Basic_Plotting.R"

# Define the file path where you want to save the Excel file
file_path <- "Basic_Plotting.R"

# Download the Excel file
download.file(url, file_path, mode = "wb")


plot(1:10)

# The plot() function takes many default arguments. One of the most important
# arguments is the way your data is to be plotted: you either indicate type='l'
# for lines, or type='p' for points or type='b' for both of them. As seen above,
# the default is p.

plot(1:10, type='b')

# There are many more arguments that you can use to change the appearance of
# your plot. Let’s look at a few of them using the following two example vectors:

 x <- seq(1, 100, length.out=20)

y <- log(x) + (x/100)^5

# The first things that you usually want to do is to give your plot a title and to
# properly annotate the axis. The former is achieved with the argument main,
# the latter with the arguments xlab and ylab that label the x-axis and y-axis,
# respectively. The default axis labels are the names of your variables, and they
# are often not very informative.

plot(x, y, xlab='X axis', ylab='Y axis', main='A nice plot')

# define other paraemeters

# col for color. 
# (you can get all available colors using the function colors()), or the
# numbers 1-8 that are matched to basic colors.
# • pch denes plotting characters. The numbers 0-25 indicate different symbols, but you can also use any character.
# • cex (for character expansion) defines the size of your plotting characters.
# Default is cex=1 and larger values increase the size of the plotting character.
# • lwd defines the line-width, i.e. how think lines are to be drawn. 
#The default is lwd=1.

plot(x, y, type='b', col="mediumvioletred", pch=5, cex=2, lwd=0.5, lty=3)

# Importantly, all these options can also provided as vectors with one element per
# data point. If the vectors are shorter, they get recycled.

randomColors <- colors()[sample(1:length(colors()), size=length(x))];
plot(x, y, col=randomColors, pch=3:12)

# Generally, if you don’t know how to plot something, the ?plot help function is
# a good place to start, and a quick google search get you usually even faster to a
# solution

# The Par function

x <- 10*(0:10)
y <- log(x) + (x/100)^5
par(mar=c(4,4,0.1,0.1), lty=2, pch=19)
plot(x,y, type='b', xlab="The variable x")

# If you check ?par you will see that the list of options is close to endless and
# allows to specify pretty much any aspect of your plot

# In addition to speciying grapical parameters, the par() function can also be
# used to retrieve current settings by providing the name of the graphical parameter
# of interest. par("lty") for instance returns the current line type set. In many
# cases the call par("usr") is particularly useful as it returns a vector of length
# 4 specifying the coordinate system of the plot: the rst pair of values give the
# x-values at the left-most and right-most position, respectively, and the second
# pair gives the y-values of the bottom and top of the plot.

 plot(1, xlim=c(-2,2), ylim=c(-1,1))
 
 par("usr")
 #[1] -2.16 2.16 -1.08 1.08
 
 # As the above example illustrates, R adds some extra space by default. This
 # behavior, while mostly useful, is easily turned o using xaxs='i' and yaxs='i'
 # for the x and y axis, respectively.
  plot(1, xlim=c(-2,2), ylim=c(-1,1), xaxs='i', yaxs='i')
  
  
  Exercises: Basic Plotting
  
  1. Create a vector x of length 20 with equally spaced values between -5 and 5. Then, create a vector y with elements yi = 2x^i . 
  Plot y against x using red filled circles connected by dashes. Label the axis “My variable x” and
  “Some transformation of x” and give the plot the title “The plot of a noob”.
  
  2. Plot y against x again, but this time as a solid, thick line. Use a random
  color, add no title nor axis-labels and modify the margins to maximizes
  the area used for the plot itself.
  
  3. In R there are length(colors()) predefined colors. Make a plot with a
  grid of 26x26 points, colored by following the list of available colors. Use
  large squares as symbols.
  
  4. Plot the numbers 1-100 as filled circles and color them in red if they are a
  multiple of 7 and in orange other wise
  
  
  
  #Multiple Data sets
  # Often we want to compare multiple data sets in a plot. Here you’ll learn the
  # basics of doing so: adding data to an existing plot and providing a legend.
  #  Adding data to a plot
  
   x <- seq(0,100, length.out=10)
   y <- log(x) + (x/100)^5
   plot(x, y)
   lines(x, y+1, col='purple', lwd=0.5)
   points(x, y-1, type='b', pch="g", cex=2, col="lightseagreen")

   # Legends
   x <- seq(0,100, length.out=10)
   y <- log(x) + (x/100)^5
   plot(x,y, type="l", lwd=3, col="dodgerblue")
   lines(x,y+1, type='b', lwd=0.5, lty=2, pch=2, col="orange2")
   points(x,y-1, pch=19, col="firebrick")
   legend("topleft", legend = c("Dataset 1", "Dataset 2","Dataset 3"), 
         lwd=c(3, 0.5, 1))

   #how to save the plot----------
   
   # a in png with good resolution
   png("my_plot.png", width = 2000, height = 1500, res = 300)
   
   x <- seq(0,100, length.out=10)
   y <- log(x) + (x/100)^5
   plot(x,y, type="l", lwd=3, col="dodgerblue")
   lines(x,y+1, type='b', lwd=0.5, lty=2, pch=2, col="orange2")
   points(x,y-1, pch=19, col="firebrick")
   legend("topleft", legend = c("Dataset 1", "Dataset 2","Dataset 3"), 
          lwd=c(3, 0.5, 1))
   
   dev.off()
   
   # in jpeg 
   
   jpeg("my_plot.jpg", width = 2000, height = 1500, res = 300)
   
   x <- seq(0,100, length.out=10)
   y <- log(x) + (x/100)^5
   
   plot(x,y, type="l", lwd=3, col="dodgerblue")
   lines(x,y+1, type='b', lwd=0.5, lty=2, pch=2, col="orange2")
   points(x,y-1, pch=19, col="firebrick")
   legend("topleft", legend = c("Dataset 1", "Dataset 2","Dataset 3"), 
          lwd=c(3, 0.5, 1))
   
   dev.off()
   
   # in pdf
   
   pdf("my_plot.pdf", width = 7, height = 5)
    x <- seq(0,100, length.out=10)
    y <- log(x) + (x/100)^5
   plot(x,y, type="l", lwd=3, col="dodgerblue")
   lines(x,y+1, type='b', lwd=0.5, lty=2, pch=2, col="orange2")
   points(x,y-1, pch=19, col="firebrick")
   legend("topleft", legend = c("Dataset 1", "Dataset 2","Dataset 3"), 
          lwd=c(3, 0.5, 1))
   
   dev.off()
   
   
   #-------------------------------------------------------
   x <- 1:10
   y <- c(0.0041, 0.034, 0.565, 1.01, 5.2, 21.7, 91.8, 1278, 60756, 132785)
   plot(x, log10(y), yaxt='n', type='b', pch=15)
   at <- seq(-2, 4, by=2)
   axis(side=2, at=at, 10^at, las=1)
   
   # side which denotes the site on which to plot the axis. side=1 refers to
   # the x-axis (bottom), side=2 to the y-axis (left) and side=3 and side=4
   # to the top and right, respectively.
   # at denotes the values at which to draw an axis.
   # labels provides the labels to be printed.
   # In order to make use of it, you may need to turn default axis printing o. You
   # can do so by setting xaxt= 'n' and / or yaxt='n'. Here is an exaple:
   
   
   # Sometimes we wish to plot multiple data sets in one plot even if they use dierent
   # units. A classic example are climate diagrams that plot both temperature and
   # rainfall. To plot data on multiple axis, some calculations are required since
   # the plot has only one unique coordinate system. Plotting data in a new axis
   # therefore requires to scale the additional data (and the corresponding axis) to
   # the coordinate system of the plot.
   
   # example with rain and climate data 
   temp <- c(-0.9, 0.5, 3.9, 7.5, 11.6, 15, 17, 16.5, 13.7, 8.8, 3.8, 0.3)
   rain <- c(75, 70, 63, 64, 77, 95, 85, 111, 81, 63, 85, 75)
   par(las=1, mar=c(4,4,0.5,4))
   plot(1:12, temp, type='b', col='red', pch=19, xlab="Month", ylab="Average temperature")
   scale <- 10
   lines(1:12, rain / scale, type='b', col='blue', pch=16)
   
   # Since choosing nice values for tick marks and labels is an art, R oers the function
   # pretty() to produce pretty labels.
   # > pretty(1:17)
   # [1] 0 5 10 15 20
   labels <- pretty(0:max(rain))
   axis(side=4, at=labels / scale, labels=labels)
   mtext("Average rainfall [mm]", side=4, line=3, las=3)
# 
#    Adding Text to a Plot
#    In addition to lines() and points(), you may also add text to a plot using
#    the function text() that takes the coordinates where text is to be put, followed
#    by the argument labels that species the labels to be printed.
#    Before illustrating this, let us introduce another important concept: empty plots.
#    You can generate an empty plot with plot() by setting type='n' (n for none).
#    Creating empty plots is particularly helpful when building up plots using loops.
#    But back to text():
   
    plot(0, type='n', xlim=c(-10, 10), ylim=c(-10, 10))
    text(-9:9, -9:9, labels=paste("Number", -9:9)) 
    
    # To plot any other line, use either aand b denoting the intercept and slope,
    # or coef to provide a vector of length two with said values.
    plot(0, type='n', xlim=c(-1,1), ylim=c(-1,1))
    abline(v=0, col='red')
    abline(h=c(-0.5,0.5), col='blue')
    abline(a=0, b=1, col='purple')
    abline(coef=c(0,-1), col='orange2')
    
    
    Exercises: Plotting
   
    1. Create a data frame with three columns x, y, and z as follows: x contains
    a vector with 1.0, 1.5, 2.0, 2.5, . . . 100, each element of y is yi = sqrt(xi),
    and each element of z is zi = log(xi). Plot y and z against x in the same
    plot, using a red solid line for y and a blue dashed line for z. Add the
    axis labels “x” and “A transformation of x” and a legend specifying the
    transformation.
    
    2. Open an empty plot with xlim=c(-10,10) and ylim=c(-1,1) and axis
    labels “x” and “y”. Plot a vertical and a horizontal dashed red line crossing
    the origin (0,0). Put the Roman numbers I, II, III and IV to name the
    https://en.wikipedia.org/wiki/Quadrant_(plane_geometry).
    
    3. Plot a climate diagram for Bagui, Central African replublic showing the 
    average daily high temperature av.high <- c(32, 34, 34, 32, 32, 30,                                                                                            32, 30, 30, 30, 32, 32) and the average percipitation av.rain <-
      (16, 31, 104, 131, 161, 155, 193, 225, 192, 199, 76, 27). Put
    the two quantities on seperate axis and make sure that their maximum is
    at the same height in the plot. Add a legend.
    
    
    # #Multiple panels
    # 
    # Often it may be desirable to create gures with multiple panels. That is rather
    # straight forward with the par() option mfrow that species a grid of panels to 
    #be used via a vector with the desired number of rows and columns.
    
     x <- seq(-10, 10, length.out=31)
     par(mfrow=c(2,3), pch=16, lty=2)
     for(slope in seq(-2, 2, length.out=6)){
      plot(0, type='n', main=paste("A linear function with slope ", slope), xlab="x", 
           ylab="y")
       abline(a=0, b=slope)
       } 

 
  