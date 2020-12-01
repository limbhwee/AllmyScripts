rm(list=ls())

#MCQs and assignments

my.analysis<-lm(log(Ozone)~Solar.R+Wind+Temp+
                  Solar.R:Wind+Solar.R:Temp+Wind:Temp,
                data=airquality[airquality>1,])
drop1(my.analysis,test="F")
my.analysis<-update(my.analysis,~.-Solar.R:Wind)
drop1(my.analysis,test="F")
my.analysis<-update(my.analysis,~.-Wind:Temp)
drop1(my.analysis,test="F")

p<- ggplot(data = diamonds)
p <- p + aes(x = carat, y = depth)
p <- p + geom_point()
p <- p + geom_density2d()
p

summary(diamonds)
depth.groups<-cut(diamonds$depth,breaks=40+(0:5)*8)
ggplot(diamonds) +
  aes(price, fill=depth.groups) +
  geom_density(alpha=.3)


paste("R session",1)
data.frame(x=1:3,y=4:6)+2           
data.frame(data1=rnorm(3), data2=rnorm(3))  
#note the above is about the same as data.frame(x=1:3,y=4:6) asking for 2 variables with 3 random numbers each
data.frame(x=1:3,y=c("A","B","C"))+2
my.data<- data.frame(data1=rnorm(3), data2=c("A","B","C"))     
#note the above is about the same as data.frame(x=1:3,y=c("A","B","C")) asking for 1 variables with 3 random numbers and another variable with characters
my.data
my.data + 2     
#note that in the demo, '2' can be added to 1st column of numbers although not shown in this example

my.standard<-function(x){(x-mean(x))/sd(x)}
set.seed(87)
y<- rnorm(100)*5+2
mean(y)
var(y)
z<- my.standard(y)
mean(z)
var(z)
plot(y,z)

my.standard(pi)
#above is to show that var needs to have more than 1 data point to work and since pi is only 1 data point NA is returned as var involves dividing the number of data points -1


f <- function(x){3*sin(x/2)+x}
f(0)
f(-1)
f(pi)

f<-function(x){3*sin(x/2)+x}
plot(f, -7,7)
curve(f,-7,7)

k1<-10
k2<-100000
my.data<-as.data.frame(matrix(rnorm(k1*k2),nrow=k1))
mean1<-numeric(k2)
mean2<-numeric(k2)
for(i in 1:k2){
  mean1[i]<-mean(my.data[,i])
}
time1<-as.numeric(Sys.time())
for(i in 1:k2){
  mean1[i]<-mean(my.data[,i])
}
time2<-as.numeric(Sys.time())
time3<-as.numeric(Sys.time())
mean2<-sapply(my.data,mean)
time4<-as.numeric(Sys.time())
(time2-time1)/(time4-time3)

#MCQs and assignments

paste('morning', 'evening slot')
#above is system defined function so just need to call or use
#user defined functions will be those created by user before call or use the function
#use via the use of function() and with a function name: functionname<- function()
#{ everything in the curly brackets will be the coding}

#function is to be called by passing 2 arguments/ numbers so that the function can be called by adding up these 2 arguments
func<- function(){
  c = a + b
}
# above curly bracket ends the function

#calling the function with the functionname after defining the arguments, returning and auto printing results
a<- 4
b<- 5
func()
#need to find out how come doesnt work the same way as kishan's demo

#below function did not specify a and b so the variables can be changed
func1<- function(a,b){
  return(a*b)
}
func1(11,22)
func1(1,22)
func1(12,2)
func1(1:5, 2) #a is between 1 to 5 whereas b is 2
func1(1:5, 1:5) #a and b are between 1 to 5 each

#below function did not specify a and b is default so the variables can be changed
func2<- function(a,b=3){
  return(a*b)
}
func2(1)
func2(1:5)
func2(1:5,1:5)#a and b are between 1 to 5 each, ignoring default b

#creation of random numbers for the plotting
x<- seq(0,6, length = 100)
x
y<- seq(2*x+3+rnorm(100))
y
plot(x,y)

#conditional statements
#branching and looping statements
#looping = iterate same statement number of times based on a condition
#variable can be vector, df

#vector is 1:5
1:5
#i will be 1 in 1st iteration, 2 in 2nd iteration and until 1 is 5 in 5th iteration
for (i in 1:5) {cat(i, "+", i, '=', i+i, '\n')
}
#cat  = concetenate different strings and also prints

#variable can be vector, df
df<- data.frame(a=1:2, b=2:3)
df
for (i in df) {cat('col sum =', sum(i), '\n')
}

#branching = jumping from statement to statement based on a condition
for (i in 1:5) {
  if (i==2)
  cat('even number', '\n')
  else
  cat('odd number', '\n')
}
#will go through from 1 to 5 and print out the branching statements; note that only 2 is considered even here even though 4 is also even number technically speaking

x<- 0
y<- abs(rnorm(1000))
i<- 0
y
while (x<3 & i<100) {
  i<- i+1
  t<- y[i]     #[] here is for the index
  x<- x+(t>2)
} 
rm(t)
i
x
#condition within the () and only when both conditions satisfied
#then will go within{} and will stop when the sub-conditions within {} not satisfy either of the initial conditions within () 
#then while loop stops

#apply family
#apply(object, margin, function)
vec<- c(seq(2,140,2))
vec

mat_vec<- matrix(vec, nrow=10, byrow=F)
mat_vec
class(mat_vec)

#[ rows, columns]
for (i in 1:10){
  row<- mat_vec[i,]
  max<-max(row)
  print(max)
}

#apply family function 
#object = list, vector, matrix
#margin = 1 is rows, 2 is columns
#function = function name
apply(mat_vec, 1, max)
#above is to find max of all rows via 1 statement

apply(mat_vec, 2, max)
#above is to find max of all columns via 1 statement

apply(mat_vec, 2, mean)
apply(mat_vec, 1, mean)
#above is to find mean of all columns and all rows via 1 statement

x<- c(1,2,3,4,NA, 4)
mean(x,na.rm=T)
#na.rm has the effect of removing the NA

apply(mat_vec,2, mean, na.rm=T)
#in case the matrix has NA also

