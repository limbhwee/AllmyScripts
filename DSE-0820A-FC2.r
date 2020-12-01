rm(list=ls())

#steps to data manipulation based on reiteration of lab 2 and more

#read file first
cars<- read.csv("automobilepricedataraw.csv", stringsAsFactors = FALSE, header=TRUE)
View(cars)
#see if any missing values or NA or ?

#see the structure to assess which ones need to convert to numeric
str(cars) 

cols = c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')

## Convert the character '?' to an NA
cars[, cols]<- lapply(cars[,cols], function(x) ifelse(x=='?', NA, x))     #via the function
#complete.cases does not work to replace ? so need to convert ? to NA then remove

## Convert character columns of those selected to numeric 
cars[,cols]<- lapply(cars[,cols], as.numeric)     #using as.numeric
#NA before being removed will be there even after cocered to as.numeric

## Remove rows with NAs
cars<- cars[complete.cases(cars), ]    #using complete.cases
#complete.cases does not work to replace ? so need to convert ? to NA then remove
#complete.cases also works with as.numeric so actually no need to convert to ? to NA then remove 

#see the structure again to confirm selected columns are numeric
str(cars) 

#filter by audi
cars_audi<- filter(cars, make=='audi')
cars_audi

#slice by rows 21 - 31 
cars_slice<- slice(cars, 20:30)
cars_slice

#randoming picking up 50% of cars_slice
cars_rand<- sample_frac(cars_slice, 0.5)
cars_rand

#select columns
cars_select<- select(cars_audi, drive.wheels, wheel.base, curb.weight,horsepower, price)
cars_select

#arrange by drive.wheels then price
cars_arrange<- arrange(cars_select, drive.wheels, price)
cars_arrange

#create new columns via mutate
cars_mutate<- mutate(cars_arrange, curb.weight.kg=curb.weight/2.205, weight.horsepower=curb.weight/ horsepower)
select(cars_mutate, curb.weight.kg, weight.horsepower)

#summarise data
summarise(cars_mutate, 
          mean.curb.weight=mean(curb.weight, na.rm=T), 
          sd.curb.weight=sd(curb.weight, na.rm=T), 
          max.curb.weight=max(curb.weight, na.rm=T), 
          min.curb.weight=min(curb.weight, na.rm=T))

#count cases by body.style and num.of.cylinders
count(cars, body.style, num.of.cylinders)

#pipe operators to combine filter, select and arrange in 1 code
cars_in2<- cars %>% filter(make=='audi') %>% 
  select(drive.wheels, wheel.base, curb.weight,engine.size, price) %>%
  arrange(drive.wheels, price)
cars_in2

#pipe operators to combine filter and select in 1 code
cars_in1<- cars %>% filter(make=='audi') %>% select(drive.wheels, wheel.base, curb.weight,engine.size, price)
cars_in1

#group_by drive.wheels to show the stats
cars %>% group_by(drive.wheels) %>% 
  summarise(count=n(), 
            mean.price=mean(price, na.rm=T), 
            sd.price=sd(price, na.rm=T), 
            max.price=max(price, na.rm=T), 
            min.price=min(price, na.rm=T))

#bar plot ned categorical data
options(repr.plot.width = 7, repr.plot.height = 5) #setting size of plots
ggplot(cars, aes(body.style)) + geom_bar()

#histogram need continuous data
ggplot(cars, aes(price)) +geom_histogram()

#boxplot
ggplot(cars, aes(x=factor(0), y=price)) +geom_boxplot()

ggplot(cars, aes(x=factor(fuel.type), y=price)) +geom_boxplot()+
  xlab('Fuel type') +ggtitle('Price by Fuel Type')

ggplot(cars, aes(x=fuel.type, y=price)) +geom_boxplot()+
  xlab('Fuel type') +ggtitle('Price by Fuel Type')
#boxplots for above and the one before are the same even without the factor()

#kernel density plots
ggplot(cars, aes(price))+geom_density()

ggplot(cars, aes(price)) + geom_density(adjust=1/5)

ggplot(cars, aes(price)) + geom_density(adjust=1/2)

ggplot(cars, aes(price)) + geom_density(adjust=1/8)


describe<- function(df, col){
  tmp = df[, col]
  sumry = summary(tmp)
  nms = names(sumry)
  nms = c(nms, 'std')
  out = c(sumry, sd(tmp))
  names(out) = nms
  out
}

describe(cars, 'price')

options(repos = c(CRAN = "http://cran.rstudio.com"))
install.packages('gridExtra')
plotstats = function(df, col, bins = 30){
  require(ggplot2)
  require(gridExtra)
  dat = as.factor('')
  ## Compute bin width
  bin.width = (max(df[, col]) - min(df[, col]))/ bins
  ## Plot a histogram
  p1 = ggplot(df, aes_string(col)) +
    geom_histogram(binwidth = bin.width)
  ## A simple boxplot
  p2 = ggplot(df, aes_string(dat, col)) +
    geom_boxplot() + coord_flip() + ylab('')
  ## Now stack the plots
  grid.arrange(p2, p1, nrow = 2)
}

plotstats(cars, 'price')

#Compute and Visualize Summary Statistics for horsepower
describe(cars, 'horsepower')
plotstats(cars, 'horsepower')
