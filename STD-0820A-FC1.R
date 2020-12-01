#Getting started with equation
x<- -41
x+16

x<- 4
3*x-2

x<- 7
5*x+1-2*x

x<- 45
x/3+1

x<- 25
2/5*x+1

x<- 1.5
3*x+2
5*x-1
3*x+2==5*x-1
3*x+2==5*x+1

#Distributive property
x<- 2
4*(x+2)+3*(x-2)
#need to put * in order to work
4(x+2)+3(x-2)

#linear equations
#creating df with 'x' column containing values from -10 to 10
df<- data.frame(x=seq(-10,10))
df
#add 'y' column by applying the solve equation to x
df$y<- (3*df$x-4)/2 #where y=(3x-4)/2
df


#plot to see relationship
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=4)
ggplot(df, aes(x,y)) + geom_point() + geom_line(color = 'blue')

#intercepts
ggplot(df, aes(x,y)) + geom_point() + geom_line(color = 'blue') +
  geom_hline(yintercept=0) + geom_vline(xintercept=0) #adding the x-axis and y-axis to plot

#show where intercepts are
ggplot(df, aes(x,y)) + geom_line(color = 'blue') +
  geom_hline(yintercept=0) + geom_vline(xintercept=0) +
  annotate("text", x = 3, y = -2, label = "y-intercept")+
  annotate("text", x = 5, y = 1, label = "x-intercept")
#Just by calculating these two points and then draw a straight line through them
#to create the entire line for the equation

#verifying the linear equation line slope
line = data.frame(x = c(0,1.5), y = c(-2,0))
ggplot() + geom_line(data = df, aes(x,y),color = 'blue') +
  geom_hline(yintercept=0) + geom_vline(xintercept=0) +
  geom_line(data = line, aes(x,y), color = 'red', size = 3)

#slope-intercept form of equation
## Make a data frame with the x values
df = data.frame(x = seq(-10,10))

## Add the y values using the formula y = mx + b
m = 1.5
b = -2
df$y = m * df$x + b

## Plot the result
ggplot() + geom_line(data = df, aes(x,y),color = 'blue') +
  geom_hline(yintercept=0) + geom_vline(xintercept=0) +
  geom_line(data = line, aes(x,y), color = 'red', size = 3) +
  annotate("text", x = 3, y = -2, label = "y-intercept")

#systems of equations
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=4)

## Create a data frames with the extreems of the possible values of chips
chips = data.frame(x = c(16,0), y = c(0,16))

## A second data frame with the extreems of the values of the chips
values = data.frame(x = c(25,0), y = c(0,10))

ggplot() + geom_line(data = chips, aes(x,y), color = 'blue', size = 1) +
  geom_line(data = values, aes(x,y), color = 'orange', size = 1)

#besides solving via plotting, can also use algebra to get the values 
x = 10
y = 6
print((x + y == 16) & ((10*x) + (25*y) == 250))

#Exponentials, radicals and logs
x<- 5^3
x
4^7

#radicals
## calculate and display the square root of 25
x = sqrt(25)
print(x)

## calculate and display the cube root of 64
cr = 64^(1/3)
print(cr)

print(9^0.5)
print(sqrt(9))

#logarithms
x = logb(16, 4)
print(x)

## Natural log of 29
log(29)

## Base 10 log of 100
log10(100)

#solving equations with exponentials
# Create a dataframe with an x column containing values from -10 to 10
df = data.frame(x = seq(-10, 10))

# Add a y column by applying the slope-intercept equation to x
df$y = 3*df$x^3

#Display the dataframe
print(df)

# Plot the line
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=4)
ggplot(df, aes(x,y)) + 
  geom_line(color = 'magenta', size = 1) +
  geom_hline(yintercept=0) + geom_vline(xintercept=0)

#where x is the exponential and not the base
# Create a dataframe with an x column containing values from -10 to 10
df = data.frame(x = seq(-10, 10))

# Add a y column by applying the slope-intercept equation to x
df$y = 2.0^df$x

## Plot the line
ggplot(df, aes(x,y)) + 
  geom_line(color = 'magenta', size = 1) +
  geom_hline(yintercept=0) + geom_vline(xintercept=0)

#application where x is exponential e.g. bank deposit
# Create a dataframe with an x column containing values from -10 to 10
df = data.frame(Year = seq(1, 20))

# Calculate the balance for each year based on the exponential growth from interest
df$Balance = 100 * (1.05^df$Year)

## Plot the line
ggplot(df, aes(Year, Balance)) + 
  geom_line(color = 'green', size = 1) +
  geom_hline(yintercept=0) + geom_vline(xintercept=0)

#simplfying polynomials
x = sample.int(100, 1)

(x^3 + 2*x^3 - 3*x - x + 8 - 3) == (3*x^3 - 4*x + 5)

#adding polynomials
x = sample.int(100, 1)

(3*x^3 - 4*x + 5) + (2*x^3 + 3*x^2 - 2*x + 2) == 5*x^3 + 3*x^2 - 6*x + 7

#subtracting polynomials
x = sample.int(100, 1)

(2*x^2 - 4*x + 5) - (x^2 - 2*x + 2) == x^2 - 2*x + 3

#multiplying polynomials
x = sample.int(100, 1)

(x^4 + 2)*(2*x^2 + 3*x - 3) == 2*x^6 + 3*x^5 - 3*x^4 + 4*x^2 + 6*x - 6

#dividing polynomials
x = sample.int(100, 1)

(4*x + 6*x^2) / (2*x) == 2 + 3*x

#dividing polynomials long way
x = sample.int(100, 1)

(x^2 + 2*x -3)/(x-2) == x + 4 + (5/(x-2))

#factors of polynomial expressions
x = sample.int(100, 1)
y = sample.int(100, 1)

(2*x*y^2)*(-3*x*y) == -6*x^2*y^3

#factors of polynomial expressions with more than 1 term
x = sample.int(100, 1)
y = sample.int(100, 1)

(x + 2)*(2*x^2 - 3*y + 2) == 2*x^3 + 4*x^2 - 3*x*y + 2*x - 6*y + 4

#greatest common factor for polynomial expressions
x = sample.int(100, 1)
y = sample.int(100, 1)

print((3*x*y)*(5*x) == 15*x^2*y)
print((3*x*y)*(3*y^2) == 9*x*y^3)

#distributing factors
x = sample.int(100, 1)
y = sample.int(100, 1)

((6*x + 15*y) == (3*(2*x) + 3*(5*y))) & ((3*(2*x) + 3*(5*y)) == (3*(2*x + 5*y)))

x = sample.int(100, 1)
y = sample.int(100, 1)

(15*x^2*y + 9*x*y^3) == (3*x*y*(5*x + 3*y^2))

#differences of squares
x = sample.int(100, 1)

(x^2 - 9) == (x - 3)*(x + 3)

#perfect squares
a = sample.int(100, 1)
b = sample.int(100, 1)

a^2 + b^2 + (2*a*b) == (a + b)^2

#quadratic equations
# Create a dataframe with an x column containing values to plot
df = data.frame(x = seq(-9, 8))

# Add a y column by applying the quadratic equation to x
df$y = 2*df$x^2 + 2 *df$x - 4

## Plot the parabola
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=4)
ggplot(df, aes(x,y)) + 
  geom_line(color = 'blue', size = 1) +
  geom_hline(yintercept=0) + geom_vline(xintercept=0)

# Create a dataframe with an x column containing values to plot
df = data.frame(x = seq(-8,11))

# Add a y column by applying the quadratic equation to x
df$y = -2*df$x^2 + 6*df$x + 7

## Plot the parabola
library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=4)
ggplot(df, aes(x,y)) + 
  geom_line(color = 'blue', size = 1) +
  geom_hline(yintercept=0) + geom_vline(xintercept=0)

#vertex and line of symmetry
plot_parabola = function(a, b, c){
  # get the x value for the line of symmetry
  vx = (-1*b)/(2*a)
  
  # get the y value when x is at the line of symmetry
  vy = a*vx^2 + b*vx + c
  
  # Create a dataframe with an x column containing values from x-10 to x+10
  minx = as.integer(vx - 10)
  maxx = as.integer(vx + 10)
  df = data.frame(x = seq(minx, maxx))
  
  # Add a y column by applying the quadratic equation to x
  df$y = a*df$x^2 + b*df$x + c
  
  # get min and max y values
  miny = min(df$y)
  maxy = max(df$y)
  
  ## data frame for line of symmetry
  symmetry = data.frame(sx = c(vx,vx), sy = c(miny,maxy))
  
  ## Plot the parabola
  ggplot(df, aes(x,y)) + 
    geom_line(color = 'blue', size = 1) +
    geom_line(data = symmetry, aes(sx,sy), color = 'magenta', size = 1) +    
    geom_point(data = symmetry, aes(sx,sy), color = 'magenta', size = 2)  +
    annotate("text", x = vx, y = -10, label = "Vertex") +
    geom_hline(yintercept=0) + geom_vline(xintercept=0)
}

plot_parabola(2, 2, -4)   

plot_parabola(-2, 3, 5) 

#for every y value, there's 2 x value
plot_parabola_limits = function(a, b, c){
  # get the x value for the line of symmetry
  vx = (-1*b)/(2*a)
  
  # get the y value when x is at the line of symmetry
  vy = a*vx^2 + b*vx + c
  
  # Create a dataframe with an x column containing values from x-10 to x+10
  minx = as.integer(vx - 10)
  maxx = as.integer(vx + 10)
  df = data.frame(x = seq(minx, maxx))
  
  # Add a y column by applying the quadratic equation to x
  df$y = a*df$x^2 + b*df$x + c
  
  # get min and max y values
  miny = min(df$y)
  maxy = max(df$y)
  
  ## data frame for line of symmetry
  symmetry = data.frame(sx = c(vx,vx), sy = c(miny,maxy))
  
  ## data frame for xlimits
  xlims = data.frame(x = c(-2,1), y = c(0,0))
  
  ## Plot the parabola
  ggplot(df, aes(x,y)) + 
    geom_line(color = 'blue', size = 1) +
    geom_line(data = symmetry, aes(sx,sy), color = 'magenta', size = 1) +    
    geom_point(data = symmetry, aes(sx,sy), color = 'magenta', size = 2)  +
    geom_point(data = xlims, aes(x,y), color = 'red', size = 2) +
    geom_text(data = xlims, aes(label=c('x1','x2')),hjust=-0.5, vjust=0) +
    annotate("text", x = vx, y = -10, label = "Vertex") +
    geom_hline(yintercept=0) + geom_vline(xintercept=0)
}

plot_parabola_limits(2, 2, -4)   

#solving quadratic equation using sq root method
y = 0
x1 = as.integer(-sqrt(y + 12 / 3))
x2 = as.integer(sqrt(y + 12 / 3))

# Create a dataframe with an x column containing some values to plot
df = data.frame(x = seq(x1-10, x2+10))

# Add a y column by applying the quadratic equation to x
df$y = 3*df$x^2 - 12

# Get x at the line of symmetry (halfway between x1 and x2)
vx = (x1 + x2) / 2

# Get y when x is at the line of symmetry
vy = 3*vx^2 - 12

# get min and max y values
miny = min(df$y)
maxy = max(df$y)

## data frame for line of symmetry
symmetry = data.frame(sx = c(vx,vx), sy = c(miny,maxy))

## data frame for xlimits
xlims = data.frame(x = c(x1,x2), y = c(0,0))

## Plot the parabola
ggplot(df, aes(x,y)) + 
  geom_line(color = 'blue', size = 1) +
  geom_line(data = symmetry, aes(sx,sy), color = 'magenta', size = 1) +    
  geom_point(data = symmetry, aes(sx,sy), color = 'magenta', size = 2)  +
  geom_point(data = xlims, aes(x,y), color = 'red', size = 2) +
  geom_text(data = xlims, aes(label=c('x1','x2')),hjust=-0.5, vjust=0) +
  annotate("text", x = vx, y = -20, label = "Vertex") +
  geom_hline(yintercept=0) + geom_vline(xintercept=0)

#solving quadratic equation using perfect square method
x1 = as.integer(-sqrt(16) - 3)
x2 = as.integer(sqrt(16) - 3)

# Create a dataframe with an x column containing some values to plot
df = data.frame(x = seq(x1-10, x2+10))

# Add a y column by applying the quadratic equation to x
df$y = ((df$x + 3)^2) - 16

# Get x at the line of symmetry (halfway between x1 and x2)
vx = (x1 + x2) / 2

# Get y when x is at the line of symmetry
vy = ((vx + 3)^2) - 16

# get min and max y values
miny = min(df$y)
maxy = max(df$y)

## data frame for line of symmetry
symmetry = data.frame(sx = c(vx,vx), sy = c(miny,maxy))

## data frame for xlimits
xlims = data.frame(x = c(x1,x2), y = c(0,0))

## Plot the parabola
ggplot(df, aes(x,y)) + 
  geom_line(color = 'blue', size = 1) +
  geom_line(data = symmetry, aes(sx,sy), color = 'magenta', size = 1) +    
  geom_point(data = symmetry, aes(sx,sy), color = 'magenta', size = 2)  +
  geom_point(data = xlims, aes(x,y), color = 'red', size = 2) +
  geom_text(data = xlims, aes(label=c('x1','x2')),hjust=-0.5, vjust=0) +
  annotate("text", x = vx, y = -20, label = "Vertex") +
  geom_hline(yintercept=0) + geom_vline(xintercept=0)

#vertex form
x = sample.int(100, 1)

2*x^2 - 16*x + 2 == 2*(x - 4)^2 - 30

plot_parabola_from_vertex_form = function(a, h, k){
  # Create a dataframe with an x column containing values from x=-10 to x=10)
  df = data.frame(x = seq(h-10, h+10))
  
  # Add a y column by applying the quadratic equation to x
  df$y = (a*(df$x - h)^2) + k
  
  # get min and max y values
  miny = min(df$y)
  maxy = max(df$y)
  
  # calculate y when x is 0 (h+-h)
  y = a*(0 - h)^2 + k
  
  ## data frame for line of symmetry
  symmetry = data.frame(sx = c(h,h), sy = c(miny,maxy))
  
  ## data frame for xlimits
  xlims = data.frame(x = c(h-h, h+h), y = c(y,y))
  
  ## Plot the parabola
  ggplot(df, aes(x,y)) + 
    geom_line(color = 'blue', size = 1) +
    geom_line(data = symmetry, aes(sx,sy), color = 'magenta', size = 1) +    
    geom_point(data = symmetry, aes(sx,sy), color = 'magenta', size = 2)  +
    geom_point(data = xlims, aes(x,y), color = 'red', size = 2) +
    geom_text(data = xlims, aes(label=c(toString(h-h),toString(h+h))),hjust=-0.5, vjust=0) +
    annotate("text", x = h, y = -40, label = paste('v = ',toString(h),',',toString(miny))) +
    geom_hline(yintercept=0) + geom_vline(xintercept=0)
}

# Call the function for the example discussed above
plot_parabola_from_vertex_form(2, 4, -30)  

#quadratice formula
plot_parabola_from_formula = function(a, b, c){
  # Get vertex
  print('CALCULATING THE VERTEX')
  print('vx = -b / 2a')
  
  nb = -b
  a2 = 2*a
  print(paste('vx = ', toString(nb), ' / ', toString(a2)))
  
  vx = -b/(2*a)
  print(paste('vx = ', toString(vx)))
  
  cat('\n')
  print('vy = ax^2 + bx + c')
  print(paste('vy =', toString(a), '(', toString(vx), '^2) + ', 
              toString(b), '(', toString(vx), ') + ', toString(c)))
  
  avx2 = a*vx^2
  bvx = b*vx
  print(paste('vy =', toString(avx2), ' + ', toString(bvx), ' + ', toString(c)))
  
  vy = avx2 + bvx + c
  print(paste('vy = ', toString(vy)))
  
  cat('\n')  
  print (paste('v = ', toString(vx), ',', toString(vy)))
  
  # Get +x and -x (showing intermediate calculations)
  cat('\n')
  print('CALCULATING -x AND +x FOR y=0')
  print('x = -b +- sqrt(b^2 - 4ac) / 2a')
  
  b2 = b^2
  ac4 = 4*a*c
  print(paste('x = ', toString(nb), '+-sqrt(', toString(b2), 
              ' - ', toString(ac4), ')/', toString(a2)))
  
  sr = sqrt(b2 - ac4)
  print(paste('x = ', toString(nb), ' +- ', toString(sr), ' / ', toString(a2)))
  print(paste('-x = ', toString(nb), ' - ', toString(sr), ' / ', toString(a2)))
  print(paste('+x = ', toString(nb), ' + ', toString(sr), ' / ', toString(a2)))
  
  posx = (nb + sr) / a2
  negx = (nb - sr) / a2
  print(paste('-x = ', toString(negx)))
  print(paste('+x = ', toString(posx)))
  
  cat('\n')
  print('PLOTTING THE PARABOLA')
  
  # Create a dataframe with an x column containing values from x=-10 to x=10)
  df = data.frame(x = seq(round(vx)-10, round(vx)+10))
  
  # Add a y column by applying the quadratic equation to x
  df$y = a*df$x^2 + b*df$x + c
  
  # get min and max y values
  miny = min(df$y)
  maxy = max(df$y)
  
  ## data frame for line of symmetry
  symmetry = data.frame(sx = c(vx,vx), sy = c(miny,maxy))
  
  ## data frame for xlimits
  xlims = data.frame(x = c(negx, posx), y = c(0,0))
  
  ## Plot the parabola
  ggplot(df, aes(x,y)) + 
    geom_line(color = 'blue', size = 1) +
    geom_line(data = symmetry, aes(sx,sy), color = 'magenta', size = 1) +    
    geom_point(data = symmetry, aes(sx,sy), color = 'magenta', size = 2)  +
    geom_point(data = xlims, aes(x,y), color = 'red', size = 2) +
    geom_text(data = xlims, aes(label=c(toString(round(negx,4)),toString(round(posx,4)))),hjust=-0.2, vjust=0) +
    annotate("text", x = 4, y = -40, label = paste('v = ',toString(vx),',',toString(vy))) +
    geom_hline(yintercept=0) + geom_vline(xintercept=0)
}

plot_parabola_from_formula (2, -16, 2)

#functions
# define a function to return x^2 + 2
f = function(x){x^2 + 2}

# call the function
f(3)

x = 4
y = f(x) - 1
y

# Create a data.frame of x values from -100 to 100
df = data.frame(x = seq(-100, 100))
df$y = f(df$x)

library(ggplot2)
library(repr)
options(repr.plot.width=4, repr.plot.height=3)
ggplot(df, aes(x,y)) + geom_line(color = 'magenta', size = 1) +
  xlab('x') + ylab('f(x)')

g = function(x){
  ## Use vectorized ifelse to return the value or the missing value, NA
  ifelse(x != 0,(12/(2*x))^2, NA)
}

## Construct the data frame.
df2 = data.frame(x = seq(-100,100))
df2$y = g(df2$x) ## Call g(x) with the vector df2$x

## Make the plot
ggplot(df2, aes(x,y)) + geom_line(color = 'magenta', size = 1) +
  annotate("text", x = 0, y = 0, label = "O") + ## Put a symbol at the origin
  xlab('x') + ylab('g(x)')

options(warn=-1) ## Turn off warnings from attempts to plot NAs

h = function(x){
  ## Use vectorized ifelse to return the value or the missing value, NA
  ifelse(x >= 0,(2 * sqrt(x)), NA)
}

## Construct the data frame.
df3 = data.frame(x = seq(-100,100))
df3$y = h(df3$x) ## Call g(x) with the vector df2$x

## Make the plot
ggplot(df3, aes(x,y)) + geom_line(color = 'magenta', size = 1) +
  annotate("text", x = 0, y = 0, label = "O") + # Put a symbol at the origin
  xlim(-1,101) + # Limit the range of x values displayed
  xlab('x') + ylab('h(x)')

j = function(x){
  ## Use vectorized ifelse to return the value or the missing value, NA
  ifelse(x >= 0 & x <= 5, x + 2, NA)
}

## Construct the data frame.
df4 = data.frame(x = seq(-100,100))
df4$y = j(df4$x) ## Call g(x) with the vector df2$x

## Make the plot
suppressWarnings( # Suppress the warnings from attempts to plot NAs
  ggplot(df4, aes(x,y)) + geom_line(color = 'magenta', size = 1) +
    # Put a symbols at the ends of the lines
    geom_point(data = data.frame(x = c(0,5), y =c(2,7)), aes(x,y), color = 'magenta', size = 2) + 
    xlim(-1,6) + # Limit the range of x values displayed
    xlab('x') + ylab('j(x)')
)

k = function(x){
  ## Use vectorized ifelse to return the value or the missing value, NA
  ifelse(x == 0, x, ifelse(x ==100, x, NA))
}

## Construct the data frame.
df5 = data.frame(x = seq(-100,100))
df5$y = k(df4$x) ## Call g(x) with the vector df2$x

## Make the plot
suppressWarnings( # Suppress the warnings from attempts to plot NAs
  ggplot(df5, aes(x,y)) + geom_point(color = 'magenta', size = 2) + 
    xlim(-1,101) + # Limit the range of x values displayed
    xlab('x') + ylab('k(x)')
)

p = function(x){x^2 + 1}

# Create an array of x values from -100 to 100
df6 = data.frame(x = seq(-100, 100))
df6$y = p(df$x)

# Plot the function
ggplot(df6, aes(x,y)) + geom_line(color = 'magenta', size = 1) +
  xlab('x') + ylab('p(x)')