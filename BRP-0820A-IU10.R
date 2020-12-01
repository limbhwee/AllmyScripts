#MCQs and assignments

View(airquality)
#dependent variable being Ozone and independent are the rest of the variables
#need to do log as Solar.R, Wind, Temp and even Ozone are of different values so log to flatten the curve
my.analysis<-lm(log(Ozone)~Solar.R+Wind+Temp,data=airquality[airquality>1,])
#linear model gives a line which is the prediction and residual is the diff between the prediction line and actual
#straight line equation: y = mx+c 
#where m is the slope/ gradient (gradient of straight line is coefficient of x): m = change in Y/ change in X
#y = c is the value where the line cuts the y-axis, c is called the intercept on the y-axis when x=0

qqnorm(my.analysis$res)
sd.1<-sd(my.analysis$res)
sd.1
lines((-3):3,((-3):3)*sd.1,type="l",lwd=3,col="red")
#type = 'l' here means line

my.analysis<-lm(log(Ozone)~Solar.R+Wind+Temp+
                  Solar.R:Wind+Solar.R:Temp+Wind:Temp,
                data=airquality[airquality>1,])
drop1(my.analysis,test="F")
my.analysis<-update(my.analysis,~.-Solar.R:Wind)
drop1(my.analysis,test="F")
my.analysis<-update(my.analysis,~.-Wind:Temp)
drop1(my.analysis,test="F")

library(glm2)
data(crabs)
View(crabs)
head(crabs)
#creating new data of 6 rows with 2 columns
crab.data<-data.frame(satellite=1*(crabs$Satellites>0),width=crabs$Width)
my.analysis<-glm(satellite~width,family=binomial,data=crab.data)
my.analysis
my.linear.predictor<-data.frame(
  prediction=predict(my.analysis,se.fit=TRUE)$fit,
  lower=predict(my.analysis,se.fit=TRUE)$fit-
    1.96*predict(my.analysis,se.fit=TRUE)$se.fit,
  upper=predict(my.analysis,se.fit=TRUE)$fit+
    1.96*predict(my.analysis,se.fit=TRUE)$se.fit)

my.linear.predictor<-my.linear.predictor[order(crab.data$width),]
logistic<-function(x){exp(x)/(1+exp(x))}
my.predictor<-logistic(my.linear.predictor)
plot(sort(crab.data$width),my.predictor$prediction,type="l",
     xlab='width',ylab='p(satellite)')
lines(sort(crab.data$width),my.predictor$upper,type="l",lty=2)
lines(sort(crab.data$width),my.predictor$lower,type="l",lty=2) 
summary(crab.data$width)
my.cut<-cut(crab.data$width,breaks=20+(0:5)*3) 
my.means<-tapply(crab.data$satellite,my.cut,mean) 
lines(20+(0:4)*3+1.5,my.means,type="p",pch=16)

View(diamonds)
library(ggplot2)
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

#MCQs and assignments

#ggplots
#if cannot find help using just 1 "?" then use 2??
??ggplot

#if cannot find then open up via the library
library(ggplot2)

#if set working directory then no need entire path to file upload

Titanic<- read.csv('tita3.csv', stringsAsFactors = FALSE)
View(Titanic)

#changing to factors for those which are categorical
Titanic$Survived<- as.factor(Titanic$Survived)
Titanic$Sex<- as.factor(Titanic$Sex)
Titanic$Pclass<- as.factor(Titanic$Pclass)
Titanic$Embarked<- as.factor(Titanic$Embarked)

#plotting chart of survival Yes = 1/ No = 0
ggplot(Titanic, aes(x=Survived)) + geom_bar()
#need to specify which type of geom like bar, otherwise nothing comes out

#% of survival rate
prop.table(table(Titanic$Survived))

#beautification of plot
ggplot(Titanic, aes(x=Survived)) + geom_bar() + theme_light() + labs(y = "Passenger count", title = "Titanic Survival")

#2nd plot for survival by 2 columns (sex, survival)
ggplot(Titanic, aes(x=Sex, fill=Survived)) + geom_bar() + theme_bw() + labs(y = "Passenger count", title = 'Titanic Survival by Gender')
#more male than female din survive

#3rd plot for survival by 2 columns (Pclass, survival)
ggplot(Titanic, aes(x=Pclass, fill=Survived)) + geom_bar() + theme_bw() + labs(y = "Passenger count", title = 'Titanic Survival by Passenger Class')
#more 3rd class passengers din survive

#Plot for survival by 3 columns (Sex, Pclass, survival)
ggplot(Titanic, aes(x=Sex, fill=Survived)) + geom_bar() + theme_bw() + labs(y = "Passenger count", title = 'Titanic Survival by Gender in Passenger Class') + facet_wrap(~Pclass)
#facet wrap works better than facet grid?
#More female survive than male in all classes

#Plot for survival by 3 columns (Sex, Pclass, survival)
ggplot(Titanic, aes(x=Sex, fill=Survived)) + geom_bar() + theme_bw() + labs(y = "Passenger count", title = 'Titanic Survival by Gender in Passenger Class') + facet_grid(~Pclass)
#facet grid and facet wrap seem to give same visual

#histogram for age group
ggplot(Titanic, aes(x=Age)) + geom_histogram() + theme_bw() + labs(y = "Passenger count", title = 'Titanic Survival by Age')

#histogram for survival based on age group
ggplot(Titanic, aes(x=Age, fill=Survived)) + geom_histogram() + theme_bw() + labs(y = "Passenger count", title = 'Titanic Survival by Age')
#children < 10 din survive, adults age > 60 also din survive

#histogram good for seeing distribution

#boxplot for survival based on age group
ggplot(Titanic, aes(x=Survived, y=Age)) + geom_boxplot() + theme_bw() + labs(y = "Passenger count", title = 'Titanic Survival by Age')
#must specify y axis
#1st line of box is 25th percentile, black line is median 50th percentile, 3rd line which close the box is 75th percentile and the tail at the end is 100th percentile and whichever dots outside are the outliers
#typical boxplot used to find outliers and the range

#more columns the better insight
#survival based on 4 columns, age, sex, Pclass - 1 way
ggplot(Titanic, aes(x=Age, fill=Survived)) + geom_density() + theme_bw() + labs(y = "Passenger count", x = 'Survived', title = 'Titanic Survival by gender, class breakdown') + facet_wrap(Sex ~ Pclass)
#survival based on 4 columns, age, sex, Pclass - another way
ggplot(Titanic, aes(x=Age, fill=Survived)) + geom_density() + theme_bw() + labs(y = "Passenger count", x = 'Survived', title = 'Titanic Survival by class, gender breakdown') + facet_wrap(Pclass ~ Sex)

#survival based on 4 columns, age, sex, Pclass - histogram 1 way
ggplot(Titanic, aes(x=Age, fill=Survived)) + geom_histogram() + theme_bw() + labs(y = "Passenger count", x = 'Survived', title = 'Titanic Survival by class, gender breakdown') + facet_wrap(Pclass ~ Sex)
#survival based on 4 columns, age, sex, Pclass - histogram another way
ggplot(Titanic, aes(x=Age, fill=Survived)) + geom_histogram() + theme_bw() + labs(y = "Passenger count", x = 'Survived', title = 'Titanic Survival by class, gender breakdown') + facet_wrap(Sex ~ Pclass)

#linear regression
#e.g. to predict price (dependent variable), using 1 or more variables as factors (which are independent variables) to predict price
#e.g. predict height (dep) based on age of person (ind) 
#assumption: older the person, taller the person
#linear line = height increases when age increases
#height = a + age* b
#where a = intercept (from where we are measuring), b = slope
#b = slope (which measure change of height with respect to age in time-dimension e.g. months)
#every month older the person, his height increase by b

#height of baby based on age
x = seq(4,14,1)
x
y = c(2.25,3.5,4.6,4.83,6.9,8.8,8.4,8.3,10,7.5,9.9)
y
plot(y ~ x)

model<- lm(y ~ x)
summary(model)
#multiple R-squared and adjusted R-squared should be closer to 1, in AML, its called Coefficient of Determination

coefficients(model)
residuals(model)

plot(resid(model) ~ fitted(model), xlab = 'fitted values', ylab= 'residuals')

library(readxl)
ageht<- read_excel('ageandheight.xls')
View(ageht)
lmageht<- lm(height ~ age, data = ageht)
summary(lmageht)

#if age 20.5 months old, a or c = 64.92 (intercept), b or m = 0.635 (slope), standard error = 0.256 (not in use)
#height = b * age + a
#y = m * x + c is standard formula
64.92 + 20.5* 0.635

#need to get the values from summary to powerBI for visualisation

#if to take into account other factors e.g. number of siblings
#y = c + mx
#height = a + b * age + b1 * age1 + b2 * age2
#there can be many slopes but intercept is only one

data()

#Assignments

install.packages('R330')
library(R330)
data(wine.df)
View(wine.df)
lmwine<- lm(price ~ year+temp+h.rain+w.rain+h.rain:w.rain, data = wine.df)
summary(lmwine)

drop1(lmwine,test='F')

coef(lmwine)
coef(lmwine)[4]+400*coef(lmwine)[6]
coef(lmwine)[4]+800*coef(lmwine)[6]

?predict()
#For Q4
newset1<- data.frame(year=1985, temp=mean(wine.df$temp), h.rain=mean(wine.df$h.rain), w.rain=mean(wine.df$w.rain))
predict(lmwine, newdata = newset1)
#no need to put exp which is exponential if did not log; see Q8

lmwine1<- lm(log(price) ~ year+temp+h.rain+w.rain+h.rain:w.rain, data = wine.df)
summary(lmwine1)
drop1(lmwine1,test='F')
lmwine1<-update(lmwine1,~.-h.rain:w.rain)
summary(lmwine1)

#Q8
newset<- data.frame(year=1985, temp=mean(wine.df$temp), h.rain=mean(wine.df$h.rain), w.rain=mean(wine.df$w.rain))
exp(predict(lmwine1, newdata = newset))
#need to put exp which is exponential since did the log

qplot(hp, qsec, data=mtcars, geom=c("point","smooth"), method="lm")
qplot(hp, qsec, data=mtcars, geom=c("point"), method="lm")
qplot(hp, qsec, data=mtcars, geom=c("point","smooth"))
qplot(hp, qsec, data=mtcars)

hist(Temp, data=airquality, breaks=10)
hist(airquality$Temp, breaks=10)
qplot(Temp, data=airquality, binwidth=5)
qplot(airquality$Temp, breaks=5)

#Assignments