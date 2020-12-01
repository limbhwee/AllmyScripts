#ungraded MCQs
x<- c(180.5,178.8,176.2,177.1,178.75,179.25,175.4,178.2,177.25,177.5,179.9,180.8,
      177.75,179.5,179.25,178.5,178.8,178.5,176.5,178.76,181.6)
mean(x)
median(x)
sd(x)
hist(x)

#graded MCQs
x<- c(172,174,176,172,172,173,176,172,177,174,176,175,176,169,175,
      174,174,174,175,173,171,171,175,175,173,175,175)
mean(x)
median(x)
mode(x)
# Create the function to get mode
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

result<- getmode(x)
result
hist(x)

x<- c(184,180,181,184,182,181,182,182,183,182,181,
      182,182,180,183,181,184,183,182,182,183,182,183,181,180,181,183)
sd(x)
hist(x)


#####Data and visualisation#####
#Galton data set
library(HistData)
df = GaltonFamilies
df

#bar charts
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

ggplot(df) + geom_bar( aes(factor(gender), fill = factor(gender))) +
  xlab('Gender of child')

#discrete quantitative data as category
## First need to drop the duplicates since there is one row per child and we only want 
## one count per family. Use the distinct function  from the dplyr package operating 
## on the family id column. 
library(dplyr)
temp = distinct(df, family, children)

## Make the bar chart on number of childern
ggplot(temp) + geom_bar( aes(factor(children)), fill = 'blue') +
  xlab('Number of children in family')

#histograms
ggplot(df) + geom_histogram( aes(father), fill = 'blue') +
  xlab('Height') +
  ggtitle('Heights of fathers')

#different bins
ggplot(df) + geom_histogram( aes(father), bins = 10, fill = 'blue') +
  xlab('Height') +
  ggtitle('Heights of fathers')

#pie charts
options(repr.plot.width=5, repr.plot.height=4)
ggplot(df, aes(x = factor(1), fill = factor(gender))) + 
  geom_bar(width = 1) +
  coord_polar(theta = 'y') +
  xlab('') + ylab(' ') +
  ggtitle('Proportion of male and female children')

#scatter plots
options(repr.plot.width=4, repr.plot.height=4.5)
ggplot(df, aes(x = midparentHeight, y = childHeight)) + 
  geom_point(alpha = 0.2, color = 'blue') +
  ylab('Heigth of the child') + xlab('Midpoint height of the parents') +
  ggtitle('Heigh of childern vs. \n midpoint height of parents')

#line charts
options(repr.plot.width=6, repr.plot.height=4)

plot(nhtemp, xlab = 'Time in years', ylab = 'Average temperature',
     main = 'Average annual temperature of New Haven CT')

#####Data and visualisation#####

#####Statistics fundamentals#####
#r mean
df1 = data.frame(name = c('Dan', 'Joann', 'Pedro', 'Rosie', 'Ethan', 'Vicky', 'Frederic'), 
                 Salary = c(50000,54000,50000,189000,55000,40000,59000))
mean(df1$Salary)

#r median
median(df1$Salary)

#using r to find mode
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

get_mode(df1$Salary)

#r table 
df2 = data.frame(name = c('Dan', 'Joann', 'Pedro', 'Rosie', 'Ethan', 'Vicky', 'Frederic'), 
                 Salary = c(50000,54000,50000,189000,59000,40000,59000))

table(df2$Salary)

#r min max
df3 = data.frame(Name = c('Dan', 'Joann', 'Pedro', 'Rosie', 'Ethan', 'Vicky', 'Frederic'),
                 Salary = c(50000,54000,50000,189000,55000,40000,59000))

print(paste('Min    = ', as.character(min(df3$Salary))))
print(paste('Mode   = ', as.character(get_mode(df3$Salary))))
print(paste('Median = ', as.character(median(df3$Salary))))
print(paste('Mean   = ', as.character(mean(df3$Salary))))
print(paste('max    = ', as.character(max(df3$Salary))))

#plot
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

ggplot(df3) + geom_histogram(aes(Salary), bins = 25, fill = 'blue', alpha = 0.4) +
  geom_vline(xintercept = mean(df3$Salary), color = 'red', linetype="dashed") +
  geom_vline(xintercept = median(df3$Salary), color = 'green', linetype="dashed")

#density plot
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

ggplot(df3, aes(x = Salary)) + 
  geom_histogram(aes(y=..density..), bins = 25, fill = 'blue', alpha = 0.4) +
  geom_density(adjust = 3, size = 0.5, color = 'orange') +
  geom_vline(xintercept = mean(df3$Salary), color = 'red', linetype="dashed") +
  geom_vline(xintercept = median(df3$Salary), color = 'green', linetype="dashed")

#distribution of hours
df4 = data.frame(Name = c('Dan', 'Joann', 'Pedro', 'Rosie', 'Ethan', 'Vicky', 'Frederic'),
                 Salary = c(41,40,36,30,35,39,40))

ggplot(df4, aes(x = Salary)) + 
  geom_histogram(aes(y=..density..), bins = 25, fill = 'blue', alpha = 0.4) +
  geom_density(adjust = 1, size = 0.5, color = 'orange') +
  geom_vline(xintercept = mean(df4$Salary), color = 'red', linetype="dashed") +
  geom_vline(xintercept = median(df4$Salary), color = 'green', linetype="dashed")

#distribution of grades
df5 = data.frame(Name = c('Dan', 'Joann', 'Pedro', 'Rosie', 'Ethan', 'Vicky', 'Frederic'),
                 Salary = c(50,50,46,95,50,5,57))

ggplot(df5, aes(x = Salary)) + 
  geom_histogram(aes(y=..density..), bins = 25, fill = 'blue', alpha = 0.4) +
  geom_density(adjust = 6, size = 0.5, color = 'orange') +
  geom_vline(xintercept = mean(df5$Salary), color = 'red', linetype="dashed") +
  geom_vline(xintercept = median(df5$Salary), color = 'green', linetype="dashed")

#skewness and kurtosis
library(e1071)

skew_kurt = function(col, df){
  print(paste(col, 'Skewness = ', as.character(skewness(df[,col]))))
  print(paste(col, 'Kurtosis = ', as.character(kurtosis(df[,col]))))
  p = ggplot(df, aes_string(x = col)) + 
    geom_histogram(aes(y=..density..), bins = 25, fill = 'blue', alpha = 0.4) +
    geom_density(adjust = 6, size = 0.5, color = 'orange') +
    geom_vline(xintercept = mean(df[,col]), color = 'red', linetype="dashed") +
    geom_vline(xintercept = median(df[,col]), color = 'green', linetype="dashed")
  print(p)
}

df6 = data.frame(Name = c('Dan', 'Joann', 'Pedro', 'Rosie', 'Ethan', 'Vicky', 'Frederic'),
                 Salary = c(50000,54000,50000,189000,55000,40000,59000),
                 Hours = c(41,40,36,30,35,39,40),
                 Grade = c(50,50,46,95,50,5,57))

skew_kurt('Salary', df6)
skew_kurt('Hours', df6)
skew_kurt('Grade', df6)

#galton data
library(HistData)
df_G = GaltonFamilies

ggplot(df_G, aes(x = father)) + 
  geom_histogram(aes(y=..density..), bins = 25, fill = 'blue', alpha = 0.4) +
  geom_density(adjust = 6, size = 0.5, color = 'orange') +
  geom_vline(xintercept = mean(df_G$father), color = 'magenta', linetype="dashed") +
  geom_vline(xintercept = median(df_G$father), color = 'green', linetype="dashed")

#r range
numcols = c('Salary', 'Hours', 'Grade')
for(col in numcols){
  print(paste(col, 'range:', as.character(max(df6[,col]) - min(df6[,col]))))
}

#quartile thresholds
quantile(df6$Hours, probs = c(0.25, 0.5, 0.75))

#box plots
options(repr.plot.width=4, repr.plot.height=5)
ggplot(df6, aes(x = 1, y = Hours)) +
  geom_boxplot() +
  ggtitle('Weekly Hours Distribution')

#outliers
ggplot(df6, aes(x = 1, y = Salary)) +
  geom_boxplot() +
  ggtitle('Salary Distribution')

#trimming outliers
df_trimmed = df6[df6$Salary < 100000,]

ggplot(df_trimmed, aes(x = 1, y = Salary)) +
  geom_boxplot() +
  ggtitle('Salary Distribution, Trimmed')

#distribution of final grades
ggplot(df6, aes(x = 1, y = Grade)) +
  geom_boxplot() +
  ggtitle('Grade Distribution')

#more data
df7 = data.frame(Name = c('Dan', 'Joann', 'Pedro', 'Rosie', 'Ethan', 'Vicky', 
                          'Frederic', 'Jimmie', 'Rhonda', 'Giovanni', 'Francesca', 
                          'Rajab', 'Naiyana', 'Kian', 'Jenny'),
                 Grade = c(50,50,46,95,50,5,57,42,26,72,78,60,40,17,85))

ggplot(df7, aes(x = 1, y = Grade)) +
  geom_boxplot() +
  ggtitle('Grade Distribution')

#r variance
var(df6$Grade)

#r sd
sd(df6$Grade)

#histogram of normal distribution
options(repr.plot.width=6, repr.plot.height=3)

## Data frame with 100,000 standard Normal values
df_normal = data.frame(Grade = rnorm(100000, mean = 0, sd = 1)) 

std = sd(df_normal$Grade)

## Plot the Normal data with the std lines 
ggplot(df_normal, aes(Grade)) + 
  geom_histogram(alpha = 0.3, color = 'blue', bins = 50) + 
  geom_vline(xintercept = std, color = 'magenta', linetype="dashed", size = 1) +
  geom_vline(xintercept = 2 * std, color = 'green', linetype="dashed", size = 1) + 
  geom_line(data = data.frame(x=c(std,-std),y=c(3800,3800)), aes(x,y), color = 'magenta',
            arrow = arrow(length=unit(0.30,"cm"), ends = "both"), size = 0.8) +
  annotate("text", x = 0, y = 4200, label = '1 std (68.26%)') +
  geom_vline(xintercept = -std, color = 'magenta', linetype="dashed", size = 1) +
  geom_vline(xintercept = -2 * std, color = 'green', linetype="dashed", size = 1) + 
  geom_line(data = data.frame(x=c(2*std,-2*std),y=c(1000,1000)), aes(x,y), color = 'green',
            arrow = arrow(length=unit(0.30,"cm"), ends = "both"), size = 0.8) +
  annotate("text", x = 0, y = 1300, label = '2 std (68.26%)')

#r summary statistics
summary(df6)

#####Statistics fundamentals#####

#####Comparing data#####
#visualise univariate data
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

## Create a data frame
df1 = data.frame(Name = c('Dan', 'Joann', 'Pedro', 'Rosie', 'Ethan', 'Vicky', 
                          'Frederic', 'Jimmie', 'Rhonda', 'Giovanni', 'Francesca', 
                          'Rajab', 'Naiyana', 'Kian', 'Jenny'),
                 Salary = c(50000,54000,50000,189000,55000,40000,59000,42000,47000,
                            78000,119000,95000,49000,29000,130000),
                 Hours = c(41,40,36,17,35,39,40,45,41,35,30,33,38,47,24),
                 Grade = c(50,50,46,95,50,5,57,42,26,72,78,60,40,17,85))

ggplot(df1, aes(x = 1, y = Grade)) +
  geom_boxplot() +
  ylab('Grade') +
  ggtitle('Grade Distribution')

#multivariate data
df1

#box plots
ggplot(df1) +
  geom_boxplot(aes(x = 'Grade', y = Grade))  +
  geom_boxplot(aes(x = 'Hours', y = Hours)) +
  geom_boxplot(aes(x = 'Salary', y = Salary)) +
  ggtitle('Distributions') + ylab('Value')

#r normalise function
## Simple MinMax function
min_max = function(x) (x - mean(x))/(max(x) - min(x))

## Create a data frame
df_min_max = df1

## Apply the MinMax function
df_min_max[,c('Grade','Hours','Salary')] = lapply(df1[,c('Grade','Hours','Salary')], min_max)

## PLot the result
ggplot(df_min_max) +
  geom_boxplot(aes(x = 'Grade', y = Grade))  +
  geom_boxplot(aes(x = 'Hours', y = Hours)) +
  geom_boxplot(aes(x = 'Salary', y = Salary)) +
  ggtitle('Distributions') + ylab('Value')

#bivariate data in scatter plots
ggplot(df1, aes(Grade, Salary)) +
  geom_point(color = 'blue', alpha = 0.5) +
  ggtitle('Salary vs. Grade')

#adding trendline
ggplot(df1, aes(Grade, Salary)) +
  geom_point(color = 'blue', alpha = 0.5) +
  geom_smooth(method='lm', se = FALSE) +
  ggtitle('Salary vs. Grade')

#r correlation coefficients
cor(df1[,c('Grade','Salary')])[1,2]

#correlation between grade and hours
cor(df1[,c('Grade','Hours')])[1,2]

ggplot(df1, aes(Hours, Grade)) +
  geom_point(color = 'blue', alpha = 0.5) +
  geom_smooth(method='lm', se = FALSE) +
  ggtitle('Grade vs. Hours')

#plotting least square regression line
df3 = data.frame(Name = c('Dan', 'Joann', 'Pedro', 'Rosie', 'Ethan', 'Vicky', 'Frederic'),
                 Study = c(1,0.75,0.6,2,1,0.2,1.2),
                 Grade = c(50,50,46,95,50,5,57),
                 fx = c(52.0159,40.9106,34.2480,96.4321,52.0149,16.4811,60.8983))

ggplot(df3) +
  geom_point(aes(Study, Grade), color = 'red', alpha = 0.5) +
  geom_line(aes(Study, fx), color = 'blue') +
  ggtitle('Study vs. Grade')

#LSE between grade and salary
## Compute the regression model
df1[,'x2'] = df1$Grade**2
df1[,'xy'] = df1$Grade * df1$Salary
x = sum(df1$Grade)
y = sum(df1$Salary)
x2 = sum(df1$x2)
xy = sum(df1$xy)
n = length(df1$Grade)
m = ((n*xy) - (x*y))/((n*x2)-(x**2))
b = (y - (m*x))/n
df1[,'fx'] = m*df1$Grade + b
df1[,'error'] = df1$fx - df1$Salary

print(paste('slope: ', as.character(m)))
print(paste('y-intercept: ', as.character(b)))

## Plot the data and the regression line
ggplot(df1) +
  geom_point(aes(Grade, Salary), color = 'red', alpha = 0.5) +
  geom_line(aes(Grade, fx), color = 'blue') +
  ggtitle('Salary vs. Grade')

## Finally print the data frame
df1

#using r lm function
## Remove columns from data frame
df1$fx = NULL
df1$error = NULL

## Compute the linear regression model
lm_mod = lm(Salary ~ Grade, data = df1)

## extract the intercept and slope form the model object
b = lm_mod$coefficient[1]
m = lm_mod$coefficient[2]
print(paste('slope: ', as.character(m)))
print(paste('y-intercept: ', as.character(b)))

## Use predict method to find predicted values
df1[,'fx'] = predict(lm_mod, newdata = df1)

## Print the data frame
df1

## Plot the data and the regression line
ggplot(df1) +
  geom_point(aes(Grade, Salary), color = 'red', alpha = 0.5) +
  geom_line(aes(Grade, fx), color = 'blue') +
  ggtitle('Salary vs. Grade')

#trimming outliers
df_trimmed = df1[df1$Grade > 10 &df1$Grade < 90,]

## Compute the linear regression model
lm_mod = lm(Salary ~ Grade, data = df_trimmed)

## extract the intercept and slope form the model object
b = lm_mod$coefficient[1]
m = lm_mod$coefficient[2]
print(paste('slope: ', as.character(m)))
print(paste('y-intercept: ', as.character(b)))

## Use predict method to find predicted values
df_trimmed[,'fx'] = predict(lm_mod, newdata = df_trimmed)

## Print the data frame
df1

## Plot the data and the regression line
ggplot(df_trimmed) +
  geom_point(aes(Grade, Salary), color = 'red', alpha = 0.5) +
  geom_line(aes(Grade, fx), color = 'blue') +
  ggtitle('Salary vs. Grade')

#####Comparing data#####

#####Probability#####
#independent events
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

set.seed(2133)
prob = 0.5 # probability of heads with a fair coin

## Create a data frame with the probabilities and the outcomes (H or  T)
coin_toss = data.frame(probs = runif(10000)) 
coin_toss[,'head_tail'] = ifelse(coin_toss$probs > prob, 'H', 'T')

## Create a bar plot of the outcomes
ggplot(coin_toss, aes(factor(head_tail))) + geom_bar(fill = 'blue')

## Compute
## Create a data frame with the probabilities and the outcomes (H or  T)
## for 3 tosses
set.seed(345)
toss_3 = data.frame(probs1 = runif(10000), probs2 = runif(10000), probs3 = runif(10000)) 
toss_3[,c('toss1', 'toss2', 'toss3')] = apply(toss_3[,c('probs1', 'probs2', 'probs3')], 1,
                                              function(x) ifelse(x > prob, 'T', 'H'))

## Find cases where all three tosses = H
toss_3[,'Heads_3'] = ifelse(toss_3$toss1 == 'H' & 
                              toss_3$toss2 == 'H' & 
                              toss_3$toss3 == 'H', 
                            TRUE, FALSE)   

## Print the proportion of runs of 3
print(paste('Percent runs of 3 =', as.character(sum(toss_3$Heads_3)/nrow(toss_3))))   

### finally, print the out comes of the sets of 3 tosses and the outcome
toss_3[,c('toss1', 'toss2', 'toss3', 'Heads_3')]   

## Compute the number of possibileies
n_trials = 3
possibilities = 2^n_trials # two possible outcomes per trial

## Create data frame
df_choose = data.frame(N_Heads = c(0,1,2,3))
df_choose[,'Successes'] = sapply(df_choose$N_Heads,
                                 function(x) choose(n_trials, x)/possibilities)    

ggplot(df_choose, aes(N_Heads, Successes)) + geom_point(size = 4) + 
  ylim(0,0.4) + xlab('Number of heads') + ylab('Probability')   

#using r dbinom function
n = 5
p = 0.25

df_binom = data.frame(N_Success = 0:n)
df_binom[,'Probability'] = sapply(df_binom$N_Success, function(x) dbinom(x,n,p))

ggplot(df_binom, aes(N_Success, Probability)) + geom_point(size = 4) + geom_line(color = 'blue') +
  ylim(0,0.4) + xlab('Number of Successes') + ylab('Probability')  

#binomial distribution
n = 100
p = 0.25

df_binom = data.frame(N_Success = 0:n)
df_binom[,'Probability'] = sapply(df_binom$N_Success, function(x) dbinom(x,n,p))

ggplot(df_binom, aes(N_Success, Probability)) + geom_line(color = 'blue') +
  xlab('Number of Successes') + ylab('Probability')    

#cal mean, var, sd
n = 100
size = 1000000
p = 0.25

mean(rbinom(size, n, p))
var(rbinom(size, n, p))
sd(rbinom(size, n, p))

#####Probability#####

#####Sampling distribution#####
#plot
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

## Create the data frame
df1 = data.frame(searches = c(0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0))

## Print some summary statistics
print(paste('Mean:', as.character(mean(df1$searches))))
print(paste('STD:', as.character(sd(df1$searches))))           

#plotting sampling distribution
## Create the data frame
df2 = data.frame(searches = c(0.1875,0.25,0.3125,0.1875,0.125,0.375,0.25,0.1875,0.3125,0.25,0.25,0.3125))

## Print some summary statistics
print(paste('Mean:', as.character(mean(df2$searches))))
print(paste('STD:', as.character(sd(df2$searches))))           

## Histogram of the sample
ggplot(df2, aes(searches)) + geom_histogram(fill = 'blue', bins = 10) +
  xlab('Searche results') + ylab('frequency')

## Histogram of the sample
ggplot(df1, aes(searches)) + geom_histogram(fill = 'blue', bins = 10) +
  xlab('Searche results') + ylab('frequency')

#simulated distribution
set.seed(18900)
n = 100
p = 0.25
n_samp = 10000
df3 = data.frame(p_hat = rbinom(n_samp, n, p)/n)

## Print some summary statistics
print(paste('Mean:', as.character(mean(df3$p_hat))))
print(paste('STD:', as.character(sd(df3$p_hat))))           

## Histogram of the sample means
ggplot(df3, aes(p_hat)) + geom_histogram(fill = 'blue', bins = 10) +
  xlab('Samples') + ylab('frequency') +
  ggtitle('Simulated Sampling Distribution')

# Compute the mean of the means and the standard errors
m = mean(df3$p_hat)
std = sd(df3$p_hat)
moe1 = m - (std * 2)
moe2 = m + (std * 2)

## Print the statistics
print(paste('Mean: ', as.character(m)))
print(paste('STD: ', as.character(std)))

## Histogram of the sample means with standard errors
ggplot(df3, aes(p_hat)) + geom_histogram(fill = 'blue', bins = 10) +
  geom_vline(xintercept = m, color = 'red', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = moe1, color = 'magenta', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = moe2, color = 'magenta', linetype="dashed", size = 0.8) +
  xlab('Samples') + ylab('frequency') +
  ggtitle('Simulated Sample Distribution')

#sampling distribution for continuous data
df4 = data.frame(meanweights = c(3.092742,
                                 3.7966726,
                                 2.98128,
                                 3.239479,
                                 3.36192,
                                 3.0229224,
                                 2.6782436,
                                 3.2832512,
                                 3.5734848,
                                 3.1646322,
                                 3.5734848,
                                 3.1646322))

## Print the statistics
print(paste('Mean of means: ', as.character(mean(df4$meanweights))))
print(paste('Std of means: ', as.character(sd(df4$meanweights))))

## Histogram of sample means
ggplot(df4, aes(meanweights)) + geom_histogram(fill = 'blue', bins = 6) +
  xlab('Mean estimates') + ylab('frequency') +
  ggtitle('Simulated Means of \n Sampling Distributions')

#simulation
set.seed(18900)
mu = 3.2
sigma = 1.2
n = 100
n_samp = 10000 # Population size
## Compute 100,000 somples from a normal distribution
Normals = rnorm(n_samp, mean = mu, sd = sigma)
## Data frame has a column with the means of 
df5 = data.frame(mean = sapply(1:n_samp, function(x) sum(sample(Normals, n, ))/n))

## Print some summary statistics
print(paste('Mean of means:', as.character(mean(df5$mean))))
print(paste('STD of means:', as.character(sd(df5$mean))))           

## Histogram of the sample means
ggplot(df5, aes(mean)) + geom_histogram(fill = 'blue', bins = 100) +
  xlab('Mean estimates') + ylab('frequency') +
  ggtitle('Simulated Means of \n Sampling Distribution')

# Compute the mean of the means and the standard errors
m = mean(df5$mean)
std = sd(df5$mean)
moe1 = m - (std * 2)
moe2 = m + (std * 2)

## Print the statistics
print(paste('Mean of means: ', as.character(m)))
print(paste('Std of means: ', as.character(std)))

## Histogram of the sample means with standard errors
ggplot(df5, aes(mean)) + geom_histogram(fill = 'blue', bins = 100) +
  geom_vline(xintercept = m, color = 'red', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = moe1, color = 'magenta', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = moe2, color = 'magenta', linetype="dashed", size = 0.8) +
  xlab('Mean estimates') + ylab('frequency') +
  ggtitle('Simulated Means of \n Sampling Distribution')

#####Sampling distribution#####

#####Hypothesis testing#####
#50 samples
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

## Simulate samples
set.seed(3456)
df1 = data.frame(samples = c(sample(-5:-1, size = 6, replace = TRUE),
                             sample(0:3, size = 38, replace = TRUE),
                             sample(4:5, size = 6, replace = TRUE)))

## Print the statistics
print(paste('Mean: ', as.character(mean(df1$samples))))
print(paste('Max: ', as.character(max(df1$samples)))) 
print(paste('Min: ', as.character(min(df1$samples))))       

## Histogram of the samples
ggplot(df1, aes(samples)) + geom_histogram(fill = 'blue', bins = 10) +
  xlab('Samples') + ylab('frequency') +
  ggtitle('Simulated Sampling Distribution')

#normal distribution
## Create a sampling population
set.seed(8789)
df2 = data.frame(pop = rnorm(100000, mean = 0.005, sd = 1.15))

## Histogram of the samples
ggplot(df2, aes(pop)) + geom_histogram(fill = 'blue', bins = 100) +
  geom_vline(xintercept = mean(df2$pop), color = 'yellow', linetype="dashed", size = 0.8) +
  xlab('Samples') + ylab('frequency') +
  ggtitle('Simulated Population Distribution')

## Perform the t test and extract statistics from the list
t_test = t.test(df2$pop, alternative = "greater") # One sample t-test
t_statistic = t_test$statistic
p_value = t_test$p.value

## Print some statistics
print(paste('t statistic: ', as.character(t_statistic)))
print(paste('p-value: ', as.character(p_value)))
upper_95 = qnorm(0.95, mean = 0.01, sd = 1.15)
cutoff = mean(df2$pop) + t_statistic * sd(df2$pop)

## Histogram of the samples
ggplot(df2, aes(pop)) + geom_histogram(fill = 'blue', bins = 100) +
  geom_vline(xintercept = mean(df2$pop), color = 'yellow', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = upper_95, color = 'red', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = cutoff, color = 'magenta', linetype="dashed", size = 0.8) +
  xlab('Samples') + ylab('frequency') +
  ggtitle('Simulated Population Distribution')

#2 tailed test
## Perform the t test and extract statistics from the list
t_test = t.test(df2$pop, alternative = "two.sided") # Two sample t-test
t_statistic = t_test$statistic
p_value = t_test$p.value

## Print some statistics
print(paste('t statistic: ', as.character(t_statistic)))
print(paste('p-value: ', as.character(p_value)))
upper_95 = qnorm(0.95, mean = 0.01, sd = 1.15)
lower_95 = qnorm(0.05, mean = 0.01, sd = 1.15)
upper_cutoff = mean(df2$pop) + t_statistic * sd(df2$pop)
lower_cutoff = mean(df2$pop) - t_statistic * sd(df2$pop)

## Histogram of the samples
ggplot(df2, aes(pop)) + geom_histogram(fill = 'blue', bins = 100) +
  geom_vline(xintercept = mean(df2$pop), color = 'yellow', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = upper_95, color = 'red', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = upper_cutoff, color = 'magenta', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = lower_95, color = 'red', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = lower_cutoff, color = 'magenta', linetype="dashed", size = 0.8) +
  xlab('Samples') + ylab('frequency') +
  ggtitle('Simulated Population Distribution')

#2 sample test
## Simulate the two samples and print statistics
set.seed(123)
two_sample = data.frame(nonMath = rnorm(100, mean = 66.0, sd = 1.5),
                        math = rnorm(100, mean = 66.8, sd = 1.5))
print(paste("non-math sample mean:", as.character(mean(two_sample$nonMath))))
print(paste("math sample mean:", as.character(mean(two_sample$math))))

## Two sample t-test
t_test = t.test(two_sample$nonMath, two_sample$math, alternative = "two.sided") # Two sample t-test
t_statistic = t_test$statistic
p_value = t_test$p.value

## Print some statistics
print(paste('t statistic: ', as.character(t_statistic)))
print(paste('p-value: ', as.character(p_value)))

## Sample difference of means
df3 = data.frame(diff_means = sapply(1:100000, 
                                     function(x) (mean(rnorm(100, mean = 66.8, sd = 1.5)) - 
                                                    mean(rnorm(100, mean = 66.0, sd = 1.5)))))
upper_95 = qnorm(0.95, mean = mean(df3$diff_means), sd = sd(df3$diff_means))
upper_cutoff = mean(df3$diff_means) + abs(t_statistic) * sd(df3$diff_means)

## Histogram of the difference of means
ggplot(df3, aes(diff_means)) + geom_histogram(fill = 'blue', bins = 100) +
  geom_vline(xintercept = mean(df3$diff_means), color = 'yellow', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = upper_95, color = 'red', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = upper_cutoff, color = 'magenta', linetype="dashed", size = 0.8) +
  xlab('Difference of means') + ylab('frequency') +
  ggtitle('Simulated difference of means')

#paired tests
## Simulate the two samples and print statistics
set.seed(123)
two_sample = data.frame(nonMath = rnorm(100, mean = 59.30, sd = 1.5),
                        math = rnorm(100, mean = 60.05, sd = 1.5))
print(paste("non-math sample mean:", as.character(mean(two_sample$nonMath))))
print(paste("math sample mean:", as.character(mean(two_sample$math))))

## Two sample t-test
t_test = t.test(two_sample$nonMath, two_sample$math, 
                alternative = "two.sided", paired = TRUE) # Two sample t-test
t_statistic = t_test$statistic
p_value = t_test$p.value

## Print some statistics
print(paste('t statistic: ', as.character(t_statistic)))
print(paste('p-value: ', as.character(p_value)))

## Sample difference of means
df3 = data.frame(diff_means = sapply(1:100000, 
                                     function(x) (mean(rnorm(100, mean = 60.05, sd = 1.5)) - 
                                                    mean(rnorm(100, mean = 59.30, sd = 1.5)))))
upper_95 = qnorm(0.95, mean = mean(df3$diff_means), sd = sd(df3$diff_means))
upper_cutoff = mean(df3$diff_means) + abs(t_statistic) * sd(df3$diff_means)

## Histogram of the difference of means
ggplot(df3, aes(diff_means)) + geom_histogram(fill = 'blue', bins = 100) +
  geom_vline(xintercept = mean(df3$diff_means), color = 'yellow', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = upper_95, color = 'red', linetype="dashed", size = 0.8) +
  geom_vline(xintercept = upper_cutoff, color = 'magenta', linetype="dashed", size = 0.8) +
  xlab('Difference of means') + ylab('frequency') +
  ggtitle('Simulated difference of means')

#####Hypothesis testing#####