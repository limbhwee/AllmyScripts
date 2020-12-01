 ## apply family 
#### apply(object, margin, function )


vec <- c(seq(2,140,2))
vec

mat_vec <- matrix(vec, nrow = 10 , byrow = FALSE )
mat_vec
class(mat_vec)

###[ rows , columns  ]

 

max(mat_vec[1,])
max(mat_vec[2,])
max(mat_vec[3,])
max(mat_vec[4,])
max(mat_vec[5,])
max(mat_vec[6,])
max(mat_vec[7,])
max(mat_vec[8,])
max(mat_vec[9,])
max(mat_vec[10,])



########### loop ing 
for(i in 1:10){
  row <- mat_vec[i,]
  max <- max(row)
  print(max)
  
}

#### apply family 
#### apply(object, margin, function )
### object - list, vector, matrix 
### margin  -1 = rows ,2 = columns 
## function = function name 


apply(mat_vec, 1, max) ### all rows = max of all rows 
apply(mat_vec, 2, max) ### all   = max of all columns 
apply(mat_vec, 2, mean)
apply(mat_vec, 1, mean)
### all   = meanof all columns 

x <- c(1,2,3,4,NA,4)
mean(x, na.rm = TRUE)
x



