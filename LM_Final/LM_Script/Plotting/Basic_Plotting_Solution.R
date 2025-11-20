
#Developped by Ernest Fotsing, PhD
#Corresponding exercises: see Section Basic-Plotting.R

#------------------First section--------------------------------------
#1. 
x <- seq(-5, 5, length.out=20)
y <- x^2*2
plot(x, y, type='b', col='red', pch=19, lty=2, xlab="My variable x", 
     ylab="Some transformation of x")

#2. 

par(mar=c(2,2,0,0))
plot(x, y, type='l', col=colors()[sample(length(colors()), 1)], lwd=10, xlab="", ylab="")
     
#3.

x <- rep(1:26, each=26)
y <- rep(1:26, 26)
plot(x, y, pch=15, col=colors(), cex=1.6)

#4. #There are at least two solutions:

plot(1:100, pch=19, col=c(rep("orange", 6), "red"))

dev.off()

# or
plot(1:100, pch=19, col=c("orange", "red")[1 + (1:100 %% 7 == 0)])

#1:100 %% 7 == 0 checks which numbers are multiples of 7.

# This gives TRUE for multiples of 7, FALSE otherwise.
# 
# 1 + (...) converts FALSE → 1 and TRUE → 2.
# 
# c("orange","red")[...] selects:
#   
#   index 1 → orange (not a multiple of 7)
# 
# index 2 → red (multiple of 7)
# 
# So multiples of 7 are red, all other points are orange.


# -----------------------Seconf section----------------------------------

#1. 
x <- seq(1, 100, by=0.5)
d <- data.frame(x=x, y=sqrt(x), z=log(x))
plot(d$x, d$y, type='l', col='red', xlab="x", ylab="A transformation of x")
lines(d$x, d$z, col='blue', lty=2)
legend('bottomright', legend=c("sqrt(x)", "log(x)"), col=c('red', 'blue'), lty=c(1, 2))

#2.

plot(0, type='n', xlim=c(-10, 10), ylim=c(-1,1), xlab="x", ylab="y")
abline(v=0, col='red', lty=1)
abline(h=0, col='red', lty=1)
text(c(5, -5, -5, 5), c(0.5, 0.5, -0.5, -0.5), labels=c("I", "II", "III", "IV"))


#3.

av.high <- c(32, 34, 34, 32, 32, 30, 32, 30, 30, 30, 32, 32)
av.rain <- c(16, 31, 104, 131, 161, 155, 193, 225, 192, 199, 76, 27)
par(las=1, mar=c(4,4,0.5,4)) # las changes the orientation of the labels
# of the axis. The plot becomes more readable this way
plot(1:12, av.high, ylim=c(0, max(av.high)), type='b', col='red', pch=19, xlab="Months")
scale <- max(av.rain) / max(av.high)
lines(1:12, av.rain / scale, type='b', col='blue', pch=17)
labels <- pretty(av.rain)
axis(side=4, at=labels / scale, labels=labels)
mtext("Average rainfall [mm]", side=4, line=3, las=3)
legend('bottom', bty='n', col=c('red', 'blue'), lwd=1, pch=c(19, 17), legend=c("Average temp", "Average rain"))

#Multiple panels

Often it may be desirable to create gures with multiple panels. That is rather
straight forward with the par() option mfrow that species a grid of panels to

