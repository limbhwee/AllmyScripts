paste("morning", "evening slot")

##################### functions #################

### system defined 
##### created by system, we only have to call or use 

#### user defined 
###### created by user , we call or use the function 

##### creatin of function
## functionname <- function()
####{ coding in the function}

#################example 1#########
#### this function to be called by passing 2 arugments/numbers so that the function can add 2 numbers and dipslay result 
############# creatin of function
func <- function(){
  #c  = a + b 
  #c
  ## or 
  #return(c) ## passs the results back to the function call
  ##or 
  return(a+b)
  
}### close of the function 
###########end of the creation 

######calling of the function 
### call the function name 
###### main program 
a <- 111
b <- 112
func()

#################end of example 1#########




#################example 2#########
############# creatin of function without arguments passing
func1 <- function(){
  #c  = a + b 
  #c
  ## or 
  #return(c) ## passs the results back to the function call
  ##or 
  return(a*b)
  
}### close of the function 
###########end of the creation 

######calling of the function 
### call the function name 
###### main program 
a <- 11 
b <- 22
func1()
#################end of example 2#########



##############example 3#########
############# creatin of function with arguments passing
func1 <- function(a,b){
  #c  = a + b 
  #c
  ## or 
  #return(c) ## passs the results back to the function call
  ##or 
  return(a*b)
  
}### close of the function 
###########end of the creation 

######calling of the function 
### call the function name 
###### main program 
#a <- 11 
#b <- 22
func1(11,22)
func1(1,22)
func1(12,2)

########    end of example 3 #############




##############example 4#########
############# creatin of function with 2 arguments passing
func1 <- function(a,b){
  #c  = a + b 
  #c
  ## or 
  #return(c) ## passs the results back to the function call
  ##or 
  return(a*b)
  
}### close of the function 
###########end of the creation 

######calling of the function 
### call the function name with pasing of 2 arguments mandatory 
###### main program 
 
func1(1:5, 2) ### a varies 1,2,3,4,5 === b is always constant = 2 
########## result 2,4,6,8,10 
func1(1:5, 1:5) 
### a varies 1,2,3,4,5 = ### b varies 1,2,3,4,5 =
##a a a a a   ##b b b b b 
##1 2 3 4 5   ##1 2 3 4 5

########    end of example4 #############



##############example 5#########
############# creatin of function with arguments passing and also default value 
func1 <- function(a,b = 3){
  #c  = a + b 
  #c
  ## or 
  #return(c) ## passs the results back to the function call
  ##or 
  return(a*b)
  
}### close of the function 
###########end of the creation 

######calling of the function 
### call the function name 
###### main program 
#a <- 11 
#b <- 22
func1(1:5) ### a varies 1,2,3,4,5 === b is always constant = 2 
########## result 2,4,6,8,10 
func1(1:5, 1:5) 
### a varies 1,2,3,4,5 = ### b varies 1,2,3,4,5 =
##a a a a a   ##b b b b b 
##1 2 3 4 5   ##1 2 3 4 5

########    end of example4 #############



############# exampl5 for plot

x <- seq(0,6,length = 100)
x
y <- seq(2*x+3+rnorm(100))
y
###### sample genratin of numbers for x and y 
 plot(x,y)
 plot(sin,0,2*pi)
#############################end of functionssss##############################

 
 
######### conditional statements 
 ### branching 
 #### looping
 ######### iterate same statement number of times based on a condition

 
 
 
 
 

