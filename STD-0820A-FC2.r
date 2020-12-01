#####Rate of change#####
#linear rate of change
q = function(x){2*x + 1}

## Construct the data frame.
df = data.frame(x = seq(0,10))
df$y = q(df$x) ## Call g(x) with the vector df2$x

## Make the plot
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=3)
ggplot(df, aes(x,y)) + geom_line(color = 'green', size = 1) +
  scale_x_continuous(breaks = seq(0,10)) +
  scale_y_continuous(breaks = seq(0,22, by = 2)) +
  xlab('Seconds') + ylab('Meters')

#average rate of change
r = function(x){x^2 + x}

## Construct the data frame.
df2 = data.frame(x = seq(0,10))
df2$y = r(df2$x) ## Call g(x) with the vector df2$x

## Make the plot
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=3)
ggplot(df2, aes(x,y)) + geom_line(color = 'green', size = 1) +
  scale_x_continuous(breaks = seq(0,10)) +
  scale_y_continuous(breaks = seq(0,120,20)) +
  xlab('Seconds') + ylab('Meters')

#secant line
## Create a data frame with the end points.
df3 = df2[c(1,nrow(df2)),]

## Make the plot
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=3)
ggplot(df2, aes(x,y)) + geom_line(color = 'green', size = 1) +
  geom_line(data = df3, aes(x,y), color = 'magenta', size = 1)
scale_x_continuous(breaks = seq(0,10)) +
  scale_y_continuous(breaks = seq(0,120,20)) +
  xlab('Seconds') + ylab('Meters')

#average velocity between 2 to 7 seconds
## Create a data frame with the end points.
df4 = data.frame(x = c(2,7))
df4$y = r(df4$x)

## Make the plot
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=3)
ggplot(df2, aes(x,y)) + geom_line(color = 'green', size = 1) +
  geom_line(data = df4, aes(x,y), color = 'magenta', size = 1) +
  scale_x_continuous(breaks = seq(0,10)) +
  scale_y_continuous(breaks = seq(0,120,20)) +
  xlab('Seconds') + ylab('Meters')
#####Rate of change#####

#####Limits#####
#showing the points on the graph
f = function(x){x^2 + x}

## Construct the data frame.
df = data.frame(x = seq(0,10))
df$y = f(df$x) ## Call g(x) with the vector df2$x

## Make the plot
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=3)
ggplot(df, aes(x,y)) + geom_line() +
  geom_point(color = 'green', size = 2) +
  scale_x_continuous(breaks = seq(0,10,2)) +
  scale_y_continuous(breaks = seq(0,120,20)) +
  xlab('x') + ylab('f(x)')

#calculating f(x) values
## Construct the data frame.
df2 = data.frame(x = c(seq(0,4), 4.25, 4.5, 4.75, 5, 5.25, 5.5, 5.75, seq(6,10)))
df2$y = f(df2$x) ## Call g(x) with the vector df2$x

## Make the plot
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=3)
ggplot(df2, aes(x,y)) + geom_line() +
  geom_point(color = 'green', size = 2) +
  scale_x_continuous(breaks = seq(0,10,2)) +
  scale_y_continuous(breaks = seq(0,120,20)) +
  xlab('x') + ylab('f(x)')

#showing x=5
## Create a new data frame with the closest points
df3 = data.frame(x = c(4.75,5,5.25))
df3$y = f(df3$x)

## and another data frame with ofsets so numbers are legible 
df4 = df3
df4$y = c(df3[1,2] - 5, df3[2,2], df3[3,2] + 5)

## Make the plot
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=3)
ggplot(df2, aes(x,y)) + geom_line() +
  geom_point(color = 'green', size = 2) +
  geom_point(data = df3, color = 'red', size = 2.5, shape = 5) +
  geom_text(data = df4, aes(label = round(df3$x,2))) +
  scale_x_continuous(breaks = seq(0,10,2)) +
  scale_y_continuous(breaks = seq(0,120,20)) +
  xlab('x') + ylab('f(x)')

#continuity
g = function(x){
  ## Use vectorized ifelse to return the value or the missing value, NA
  ifelse(x != 0,-(12/(2*x))**2, NA)
}

## Construct the data frame.
df5 = data.frame(x = seq(-20,20))
df5$y = g(df5$x) ## Call g(x) with the vector df2$x

## Make the plot
ggplot(df5, aes(x,y)) + geom_line(color = 'green', size = 1) +
  scale_x_continuous(breaks = seq(-20,20,4)) +
  scale_y_continuous(breaks = seq(-40,0,4)) +
  xlab('x') + ylab('g(x)')

#non continuous plot
df6 = data.frame(x = 0, y = -37)

## Make the plot
ggplot(df5, aes(x,y)) + geom_line(color = 'green', size = 1) +
  geom_point(data = df6, aes(x,y), size = 3, color = 'green') +
  scale_x_continuous(breaks = seq(-20,20,4)) +
  scale_y_continuous(breaks = seq(-40,0,4)) +
  xlab('x') + ylab('g(x)')

#plot function h
h = function(x){
  ## Use vectorized ifelse to return the value or the missing value, NA
  ifelse(x >= 0,2*sqrt(x), NA)
}

## Construct the data frame.
df7= data.frame(x = seq(-20,20))
df7$y = h(df7$x) ## Call g(x) with the vector df2$x

## data frame to plot terminal point
df8 = data.frame(x = 0, y = 0)

## Make the plot
ggplot(df7, aes(x,y)) + geom_line(color = 'green', size = 1) +
  geom_point(data = df8, aes(x,y), size = 3, color = 'green') +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,10)) +
  xlim(c(0,20)) +
  xlab('x') + ylab('h(x)')

#plot function k
k= function(x){
  ## Use vectorized ifelse to return the value or the missing value, NA
  ifelse(x <= 0, x+20, x-100)
}

## Construct the data frame.
df9= data.frame(x = seq(-20,20))
df9$y = k(df9$x) ## Call g(x) with the vector df2$x

## data frame to plot terminal points
df10 = data.frame(x = 0, y = k(0))
df11 = data.frame(x=0.0001, y = k(0.0001))


## Make the plot
ggplot(df9[1:21,], aes(x,y)) + geom_line(color = 'green', size = 1) +
  geom_line(data = df9[22:40,], aes(x,y), color = 'green', size = 1) +
  geom_point(data = df10, aes(x,y), size = 3, color = 'green') +
  geom_point(data = df11, aes(x,y), size = 3, color = 'green', shape = 1) +
  scale_x_continuous(breaks = seq(-20,20,4)) +
  scale_y_continuous(breaks = seq(-100,20,20)) +
  xlab('x') + ylab('k(x)')

#finding limits of function graphically
a = function(x){x^2 + 1}

## Construct the data frame.
df12 = data.frame(x = seq(-10,10))
df12$y = a(df12$x) ## Call g(x) with the vector df2$x

## Make the plot
ggplot(df12, aes(x,y)) + geom_line(color = 'magenta', size = 1) +
  xlab('x') + ylab('a(x)')

#plotting for a(0) value
df13 = data.frame(x = 0, y = a(0))

## Make the plot
ggplot(df12, aes(x,y)) + geom_line(color = 'magenta', size = 1) +
  geom_point(data = df13, aes(x,y), size = 3, color = 'red')  +
  annotate("text", x = 0, y = 8, label = toString(df13$y)) +
  xlab('x') + ylab('a(x)')

#plotting for x slightly higher or lower than 0
## Data frames for the marker position with x set for correct display of shape
df14 = data.frame(x = 0.5, y = a(0.1))
df15 = data.frame(x = -0.4, y = a(-0.1))

## Make the plot
ggplot(df12, aes(x,y)) + geom_line(color = 'magenta', size = 1) +
  geom_point(data = df14, aes(x,y), size = 6, color = 'blue', shape = '<')  +
  geom_point(data = df15, aes(x,y), size = 6, color = 'green', shape = '>')  +
  annotate("text", x = 1, y = 8, label = toString(df14$y)) +
  annotate("text", x = -1, y = 8, label = toString(df15$y)) +
  xlab('x') + ylab('a(x)')

#plotting for non continuous points
b = function(x){
  ## Use ifelse to vectorize the if statement
  ifelse(x != 0, (-2*x**2)/x, NA)
}

## Construct the data frame.
df16 = data.frame(x = seq(-10,10))
df16$y = b(df16$x) ## Call g(x) with the vector df2$x

## Make the plot
ggplot(df16, aes(x,y)) + geom_line(color = 'magenta', size = 1) +
  xlab('x') + ylab('b(x)')

#plotting for x slightly less than 0
## Data frame for marker
df17 = data.frame(x = -0.1, y = b(-0.1))

## Make the plot
ggplot(df16, aes(x,y)) + geom_line(color = 'magenta', size = 1) +
  geom_point(data = df17, aes(x,y), size = 6, color = 'orange', shape = '>')  +
  annotate("text", x = df17$x + 2, y = df17$y, label = toString(df17$y)) + 
  xlab('x') + ylab('b(x)')

#plotting for x negative value less than 0
## Data frame for marker
df18 = data.frame(x = -0.0001, y = b(-0.0001))

## Make the plot
ggplot(df16, aes(x,y)) + geom_line(color = 'magenta', size = 1)  +
  geom_point(data = df18, aes(x,y), size = 6, color = 'orange', shape = '>')  +
  annotate("text", x = df18$x + 2, y = df18$y, label = toString(df18$y)) + 
  xlab('x') + ylab('b(x)')

#plotting when x=0.1
## Data frame for marker
df19 = data.frame(x = 0.1, y = b(0.1))

## Make the plot
ggplot(df16, aes(x,y)) + geom_line(color = 'magenta', size = 1)  +
  geom_point(data = df19, aes(x,y), size = 6, color = 'blue', shape = '<')  +
  annotate("text", x = df19$x + 2, y = df19$y, label = toString(df19$y)) + 
  xlab('x') + ylab('b(x)')

#plotting when x even closer to 0
## Data frame for marker
df18 = data.frame(x = 0.0001, y = b(0.0001))

## Make the plot
ggplot(df16, aes(x,y)) + geom_line(color = 'magenta', size = 1)  +
  geom_point(data = df18, aes(x,y), size = 6, color = 'blue', shape = '<')  +
  annotate("text", x = df18$x + 2, y = df18$y, label = toString(df18$y)) + 
  xlab('x') + ylab('b(x)')

#1 sided limits
#plotting function c (aka k)
k= function(x){
  ## Use vectorized ifelse to return the value or the missing value, NA
  ifelse(x <= 0, x+20, x-100)
}

## Construct the data frame.
df19= data.frame(x = seq(-20,20,by=0.1))
df19$y = k(df19$x) ## Call g(x) with the vector df2$x

## data frames to marker points
df20 = data.frame(x = c(-15, -10, -5, 0, 0.001, 6, 10, 15, 20))
df20$y = k(df20$x)
df21 = data.frame(x = c(0,0), y = k(c(0.0,0.01)))

## Make the plot
ggplot(df19, aes(x,y)) + geom_line(color = 'green', size = 1) +
  geom_point(data = df20[1:5,], aes(x,y), size = 5, color = 'orange', shape = '>') +
  geom_point(data = df20[6:9,], aes(x,y), size = 5, color = 'blue', shape = '<') +
  geom_point(data = df21, aes(x,y), size = 3, color = 'blue', shape = 'O') +
  annotate("text", x = df20$x, y = df20$y + 8, label = df20$y) + 
  scale_x_continuous(breaks = seq(-20,20,4)) +
  scale_y_continuous(breaks = seq(-100,20,20)) +
  xlab('x') + ylab('k(x)')

#asymptotes and infinity
## Function to compute d
d = function(x){
  return(ifelse(x != 25, 4/(x-25), NULL))
}

## Compute the x and y values above and below the sigularity
df20 = data.frame(x = seq(-100,24.9,by=0.1))
df20$d = d(df20$x)
df21 = data.frame(x = seq(25.1,100,by=0.1))
df21$d = d(df21$x)

## plot the function
ggplot() + 
  geom_line(data = df20, aes(x, d), color = 'magenta', size = 1) +
  geom_line(data = df21, aes(x, d), color = 'magenta', size = 1)  +
  xlab('x') + ylab('d(x)')

## Data frame for the annotations
df22 = data.frame(x = c(-55, 0, 23, 24.5, 24.8, 24.9, 25.1, 25.2, 25.5, 30, 50, 75))
df22$d = d(df22$x)

## plot the function with annotaton
ggplot() + 
  geom_line(data = df20, aes(x, d), color = 'magenta', size = 1) +
  geom_line(data = df21, aes(x, d), color = 'magenta', size = 1) +
  geom_point(data = df22, aes(x,d)) +
  annotate("text", x = df22$x + 7, y = df22$d + 3, label = round(df22$d,8)) +
  xlab('x') + ylab('d(x)')

#finding limits numerically using table
## Function a(x)
a = function(x) x^2 + 1

## Create the data frame
df23 = data.frame(x = c(-1, -0.5, -0.2, -0.1, -0.01, 0, 0.01, 0.1, 0.2, 0.5, 1))
df23[,'a(x)'] = a(df23$x)
df23

## Function e(x)
e = function(x){
  ifelse(x == 0, 5, 1 + x^2) 
}

## Create the data frame
df24 = data.frame(x = c(-1, -0.5, -0.2, -0.1, -0.01, 0, 0.01, 0.1, 0.2, 0.5, 1))
df24[,'e(x)'] = e(df24$x)
df24

ggplot() +
  geom_line(data = df24[df24$x < 0,], aes(x,e(x)), color = 'magenta', size = 1) + 
  geom_line(data = df24[df24$x > 0,], aes(x,e(x)), color = 'magenta', size = 1) +
  geom_point(data = df24[df24$x == 0,], aes(x,e(x)), color = 'magenta', size = 2) +
  geom_point(data = data.frame(x = 0, y = 1), aes(x,y), size = 3, color = 'magenta', shape = 1)


#determining limits analytically
#direct substituition
g = function(x){
  ## Use ifelse to vectorize the if statement
  ifelse(x != 1, (x^2 - 1)/(x - 1), NA)
}

## Construct the data frame.
df25 = data.frame(x = seq(-20,20))
df25$g = g(df25$x) ## Call g(x) with the vector df25$x

## Make the plot
ggplot(df25, aes(x,g)) + geom_line(color = 'magenta', size = 1) +
  xlab('x') + ylab('g(x)')

df_point = data.frame(x = 4, y = g(4))

## Make the plot
ggplot(df25, aes(x,g)) + 
  geom_line(color = 'magenta', size = 1) +
  geom_point(data = df_point, aes(x,y), size = 3) +
  annotate("text", x = df_point$x + 2, y = df_point$y, 
           label = round(df_point$y, 8)) +
  xlab('x') + ylab('g(x)')

#factorization
df_point = data.frame(x = 1, y = 2)

## Make the plot
ggplot(df25, aes(x,g)) + 
  geom_line(color = 'magenta', size = 1) +
  geom_point(data = df_point, aes(x,y), size = 3) +
  annotate("text", x = df_point$x + 2, y = df_point$y, 
           label = round(df_point$y, 8)) +
  xlab('x') + ylab('g(x)')

#rationalisation
h = function(x){
  ## Use ifelse to vectorize the if statement
  ifelse((x >= 0 & x != 4), (sqrt(x) - 2)/(x - 4), NA)
}

## Construct the data frame.
df26 = data.frame(x = seq(0,20))
df26$h = h(df26$x) ## Call g(x) with the vector df26$x

## Make the plot
ggplot(df26, aes(x,h)) + geom_line(color = 'magenta', size = 1) +
  xlab('x') + ylab('h(x)')

df_point = data.frame(x = 4, y = 1 / ((sqrt(4)) + 2))

## Make the plot
ggplot(df26, aes(x,h)) + geom_line(color = 'magenta', size = 1) +
  geom_point(data = df_point, aes(x,y), size = 3) +
  annotate("text", x = df_point$x + 2, y = df_point$y, 
           label = round(df_point$y, 8)) +
  xlab('x') + ylab('h(x)')

#rules for limit operations
j = function(x) 2*x - 2
l = function(x) -2*x + 4

df27 = data.frame(x = seq(-10,10))
df27$j = j(df27$x)
df28 = data.frame(x = df27$x, l = l(df27$x))

## Make the plot
ggplot() + 
  geom_line(data = df27, aes(x,j), color = 'green', size = 1) +
  geom_line(data = df28, aes(x,l), color = 'magenta', size = 1) +
  ggtitle('j(x) [green] and l(x) [magenta] vs. x')
xlab('x') + ylab('y')
#####Limits#####

#####Differentiation and Derivatives#####
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

## Define the function
f = function(x) x^2 + x

df1 = data.frame(x = seq(0,10))
df1$f = f(df1$x)

ggplot(df1, aes(x,f)) +
  geom_line(color = 'green', size = 1) +
  ylab('f(x)')

df2 = data.frame(x = c(4,6), y = c(f(4),f(6)))
m = (df2$y[2] - df2$y[1])/(df2$x[2] - df2$x[1])

ggplot() +
  geom_line(data = df1, aes(x,f), color = 'green', size = 1) +
  geom_line(data = df2, aes(x,y), color = 'magenta', size = 1) +
  geom_point(data = df2, aes(x,y), color = 'magenta', size = 1)  +
  annotate("text", x = 7.5, y = 26, label = paste('Average change =', as.character(m))) +
  ylab('f(x)')

## Find the slope of the tanget line
m = (f(4.51) - f(4.49))/(4.51 - 4.49)
x_tan = 4.5
y_tan = f(x_tan)

## Create data frame with end points of tangent line
df3 = data.frame(x = c(1.5,7.5), 
                 y = c(m*(1.5 - x_tan) + y_tan, m*(7.5 - x_tan) + y_tan))

ggplot() +
  geom_line(data = df1, aes(x,f), color = 'green', size = 1) +
  geom_line(data = df3, aes(x,y), color = 'magenta', size = 1) +
  geom_point(data = data.frame(x=x_tan, y = y_tan), aes(x,y), color = 'magenta', size = 3)  +
  ylab('f(x)')

#calculating a derivative
df4 = data.frame(x = c(3,6))
df4$f = f(df4$x)
df4$labs = c('(x,f(x))', '(x+h, f(x+h))')

ggplot() +
  geom_line(data = df1, aes(x,f), color = 'green', size = 1) +
  geom_point(data = df4, aes(x,f), color = 'magenta', size = 3)  +
  annotate("text", x = df4$x + 2, y = df4$f, label = df4$labs)  +
  ylab('f(x)')

#finding derivative for specific point
ggplot() +
  geom_line(data = df1, aes(x,f), color = 'green', size = 1) +
  geom_point(data = data.frame(x = 2, y = f(2)), aes(x,y), color = 'magenta', size = 3)  +
  annotate("text", x = 3, y = f(2) - 5, label = '(x,f(x))')  +
  ylab('f(x)')

#tangent line
## Create data frame with end points of tangent line
x_tan = 2
y_tan = f(x_tan)
m = 2 * x_tan + 1 # compute derivative
df5 = data.frame(x = c(-1,5), 
                 y = c(m*(-1 - x_tan) + y_tan, m*(5 - x_tan) + y_tan))

ggplot() +
  geom_line(data = df1, aes(x,f), color = 'green', size = 1) +
  geom_point(data = data.frame(x = x_tan, y = y_tan), aes(x,y), color = 'magenta', size = 3) +
  geom_line(data = df5, aes(x,y), color = 'magenta', size = 1)  +
  annotate("text", x = x_tan + 1, y = y_tan - 5, label = '(x,f(x))')  +
  ylab('f(x)')

#tanget line for f'(5)
## Create data frame with end points of tangent line
x_tan = 5
y_tan = f(x_tan)
m = 2 * x_tan + 1 # compute derivative
df6 = data.frame(x = c(2,8), 
                 y = c(m*(2 - x_tan) + y_tan, m*(8 - x_tan) + y_tan))

ggplot() +
  geom_line(data = df1, aes(x,f), color = 'green', size = 1) +
  geom_point(data = data.frame(x = x_tan, y = y_tan), aes(x,y), color = 'magenta', size = 3) +
  geom_line(data = df6, aes(x,y), color = 'magenta', size = 1)  +
  annotate("text", x = x_tan + 1, y = y_tan - 5, label = '(x,f(x))')  +
  ylab('f(x)')

#differentiability
g = function(x){
  ifelse(x < -4, 40000/x^2,
         ifelse(x != 0 & x > -4 & x < 8, (x^2-2)*(x-1),
                ifelse(x >= 8, x^2 - 2, NA)))
}

## Data to plot
df7 = data.frame(x = c(seq(-10,-5),
                       -3.99,
                       -4.01,
                       seq(-4,7),
                       7.9999,
                       seq(8,10)))
df7$g = g(df7$x)

## Points for illustration
df8 = data.frame(x = c(-3.99, 0, 8))
df8$y = g(df8$x)
df8[2,] = c(0,0) # add point where function is not defined
df8$labs = c('A (x= -4)', 'B (x= 0)', 'C (x= 8)')

## Display plot
ggplot() +
  geom_line(data = df7, aes(x,g), color = 'green', size = 1) +
  geom_point(data = df8, aes(x,y), color = 'magenta', size = 3) +
  #    geom_line(data = df6, aes(x,y), color = 'magenta', size = 1)  +
  annotate("text", x = df8$x, y = df8$y - 100, label = df8$lab)  +
  ylab('g(x)')

#derivatives of equations
df9 = data.frame(x = seq(1,10))
df9$f = 2 * df9$x + 6 # Compute derivative for the x values

ggplot(df9, aes(x,f)) +
  geom_line(color = 'magenta', size = 1) +
  ylab('y')

#####Differentiation and Derivatives#####

#####Critical points and optimisation#####
#trajectory of ball
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

## Define the function
k = function(x) -10 * x^2 + 100 * x + 3

df1 = data.frame(x = seq(0,10))
df1$f = k(df1$x)

ggplot(df1, aes(x,f)) +
  geom_line(color = 'green', size = 1) +
  ylab('k(x) (height in feet)') + 
  xlab('x (time in seconds)')

## Function to compute derivative
kd = function(x) -20 * x + 100

df2 = data.frame(x = seq(0,10))
df2$y = kd(df2$x)

ggplot() +
  geom_line(data = df1, aes(x,f), color = 'green', size = 1) +
  geom_line(data = df2, aes(x,y), color = 'magenta', size = 0.8) +
  ylab('k(x) (height in feet)') + 
  xlab('x (time in seconds)')

# Tangent points to plot
x1 = 2
x2 = 5
x3 = 8

ggplot() +
  geom_line(data = df1, aes(x,f), color = 'green', size = 1) +
  geom_line(data = df2, aes(x,y), color = 'magenta', size = 0.8) +
  geom_line(data = data.frame(x = c(x1-1,x1+1), y = c(k(x1)-kd(x1),k(x1)+kd(x1))), 
            aes(x,y), color = 'red', size = 0.8) +
  geom_line(data = data.frame(x = c(x2-1,x2+1), y = c(k(x2)-kd(x2),k(x2)+kd(x2))), 
            aes(x,y), color = 'red', size = 0.8) +
  geom_line(data = data.frame(x = c(x3-1,x3+1), y = c(k(x3)-kd(x3),k(x3)+kd(x3))), 
            aes(x,y), color = 'red', size = 0.8) +
  ylab('k(x) (height in feet)') + 
  xlab('x (time in seconds)')

#flowers
## Define the function
w = function(x) x^2 + 2 * x + 7

df3 = data.frame(x = seq(-10,10))
df3$w = w(df3$x)

## Function to compute derivative
wd = function(x) 2 * x + 2

df4 = data.frame(x = seq(-10,10))
df4$y = wd(df4$x)

ggplot() +
  geom_line(data = df3, aes(x,w), color = 'green', size = 1) +
  geom_line(data = df4, aes(x,y), color = 'magenta', size = 0.8) +
  ylab('w(x) (flowers)') + 
  xlab('x (time in days)')

#critical points
## Define the function
v = function(x) x^3 -6 * x^2 + 100

df5 = data.frame(x = seq(-10,10))
df5$v = v(df5$x)

## Function to compute derivative
vd = function(x) 3 * x^2 + 2

df6 = data.frame(x = seq(-10,10))
df6$y = vd(df6$x)

ggplot() +
  geom_line(data = df5, aes(x,v), color = 'green', size = 1) +
  geom_line(data = df6, aes(x,y), color = 'magenta', size = 0.8) +
  ylab('v(x)') + 
  xlab('x')

## The second derivative max
df2$sec_der = rep(-20, length.out = nrow(df2)) 

ggplot() +
  geom_line(data = df1, aes(x,f), color = 'green', size = 1) +
  geom_line(data = df2, aes(x,y), color = 'magenta', size = 0.8) +
  geom_line(data = df2, aes(x,sec_der), color = 'red', size = 0.8) +
  ylab('k(x) (height in feet)') + 
  xlab('x (time in seconds)')

## The second derivative min
df4$sec_der = rep(2, length.out = nrow(df4)) 

ggplot() +
  geom_line(data = df3, aes(x,w), color = 'green', size = 1) +
  geom_line(data = df4, aes(x,y), color = 'magenta', size = 0.8) +
  geom_line(data = df4, aes(x,sec_der), color = 'red', size = 0.8) +
  ylab('w(x) (flowers)') + 
  xlab('x (time in days)')

## Define the function
v = function(x) x^3 - 6 * x^2 + 12 * x + 2

df7 = data.frame(x = seq(-5,10))
df7$v = v(df7$x)

## Function to compute derivative
vd = function(x) 3 * x^2 - 12 * x + 12

df7$first_der = vd(df7$x)

## Function to compute second derivative
vd2 = function(x) 6 * x - 12

df7$sec_der = vd2(df7$x)

ggplot() +
  geom_line(data = df7, aes(x,v), color = 'green', size = 1) +
  geom_line(data = df7, aes(x,first_der), color = 'magenta', size = 0.8) +
  geom_line(data = df7, aes(x,sec_der), color = 'red', size = 0.8) +
  ylab('v(x)') + 
  xlab('x')

cat(paste("v(2)   =", v(2), '\n'))
cat(paste("v'(2)  = ", vd(2), '\n'))
cat(paste("v''(2) = ", vd2(2)))

#optimization
s = function(x) -5 * x + 100
r = function(x) s(x) * x

df8 = data.frame(x = seq(0,20))
df8$r = r(df8$x)

ggplot(df8, aes(x,r)) +
  geom_line(color = 'green', size = 1) +
  ylab('x (monthly fee)') + 
  xlab('r(x) (revenue in $,000)')

#####Critical points and optimisation#####

#####Partial derivatives#####
library(ggplot2)
library(repr)
options(repr.plot.width=5, repr.plot.height=5) # Set the initial plot area dimensions

## Create data frame from grid with out {0,0}
df = data.frame(expand.grid(x=c(seq(-5,-1), seq(1,5)), y=c(seq(-5,-1), seq(1,5))))
df$dx = 2 * df$x # compute x derivative
df$dy = 2 * df$y # compute y derivative
df$dx_scale = 0.1 * df$dx # scale to get desired line length
df$dy_scale = 0.1 * df$dy # scale to get desired line length

## Data frame with values to contour
df2 = data.frame(expand.grid(x=seq(-5,5,by=0.1), y=seq(-5,5,by=0.1)))
df2$mag = sqrt(df2$x^2 + df2$y^2)

## Plot countours of function and gradient as arrows
ggplot() + 
  geom_contour(data = df2, aes(x, y, z = mag)) +
  geom_segment(data = df, aes(x = x, y = y, xend = x - dx_scale, yend = y - dy_scale),
               size = 1, arrow = arrow(length = unit(0.2,"cm")))

#####Partial derivatives#####

#####Integration#####
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

## Define the function
f = function(x) x

df1 = data.frame(x = seq(0,10))
df1$f = f(df1$x)

ggplot(df1, aes(x,f)) +
  geom_line(color = 'magenta', size = 0.8) +
  ylab('f(x)') + 
  xlab('x')

## Data frame for shaded area polygon
shade = data.frame(x=c(0,2,2), y=c(0,2,0))

ggplot(df1, aes(x,f)) +
  geom_line(color = 'magenta', size = 0.8) +
  geom_polygon(data = shade, aes(x = x, y = y), fill = 'orange') +
  ylab('f(x)') + 
  xlab('x')

#integral of 1 dimension function
integrate(f, 0, 2)$value

#more complex integral
## Define the function
f = function(x) 3 * x^2 + 2 * x + 1

## Data frame of function values
df2 = data.frame(x = seq(0,10))
df2$f = f(df2$x)

## Data frame for shaded area polygon
shade2 = data.frame(x = c(seq(0,3), 3, 0))
shade2$y = c(f(shade2$x[1:4]), 0, 0)

ggplot(df2, aes(x,f)) +
  geom_line(color = 'magenta', size = 0.8) +
  geom_polygon(data = shade2, aes(x = x, y = y), fill = 'orange') +
  ylab('f(x)') + 
  xlab('x')

integral = integrate(f, 0, 3)
integral$value
#abs error
integral$abs.error

#infinite limits
integrate(function(x) exp(-5*x), 0, Inf)

#normal distribution pdf
integrate(function(x) (1/sqrt(2 * pi)) * exp(-x^2/2.0), -Inf, Inf)

#####Integration#####