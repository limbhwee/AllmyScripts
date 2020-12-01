#####Vectors#####
#visualise vector v
library(ggplot2)
library(NISTunits) # Libary to work between degrees and radians
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

## data frame for origin and end of vector
v = data.frame(x = c(0,2), y = c(0,1))

## Plot the vector
ggplot(v, aes(x,y)) + 
  geom_line(arrow = arrow(length=unit(0.30,"cm")), color = 'red', size = 1) +
  xlim(-5,5) + ylim(-5,5)

#magnitude of v
vMag = sqrt(v$x[2]^2 + v$y[2]^2)
vMag

#vector v coerced to matrix
vMag = norm(as.matrix(v), '2')
vMag

#calculate direction
vTan = v[2,2]/v[2,1]
cat(paste('tan     = ', as.character(vTan), '\n'))
arc_vTan = atan(vTan)
cat(paste('arc tan = ', as.character(NISTradianTOdeg(arc_vTan))))

cat(paste('atan for v = ', as.character(NISTradianTOdeg(atan(v[2,2]/v[2,1]))), '\n'))

## Another data frame s
s = data.frame(x = c(0,-3), y = c(0,2))

## Print the arc tan of s
cat(paste('atan for v = ', as.character(180 + NISTradianTOdeg(atan(s[2,2]/s[2,1])))))

#vector addition
## Plot the both vectors
ggplot() + 
  geom_line(data = v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'red', size = 1) +
  geom_line(data = s, aes(x,y), arrow = arrow(length=unit(0.30,"cm"), ends = 'first'), 
            color = 'blue', size = 1) +
  xlim(-5,5) + ylim(-5,5)

z = v + s
print(z[2,])

#plot z
## Plot with result vector
ggplot() + 
  geom_line(data = v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'red', size = 1) +
  geom_line(data = s, aes(x,y), arrow = arrow(length=unit(0.30,"cm"), ends = 'first'), 
            color = 'blue', size = 1) +
  geom_line(data = z, aes(x,y), arrow = arrow(length=unit(0.30,"cm"), ends = 'first'), 
            color = 'green', size = 1) +
  xlim(-5,5) + ylim(-5,5)

#####Vectors#####

#####Multiplications#####
#scalar multiplication
library(ggplot2)
library(NISTunits) # Libary to work netween degrees and radians
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

## data frame for origin and end of vector
v = data.frame(x = c(0,2), y = c(0,1))

## Element-wise multiply the vector by 2
w = 2 * v

## Plot the vector
ggplot() + 
  geom_line(data = v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'red', size = 1) + 
  geom_line(data = w, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), size = 0.5) +
  xlim(-5,5) + ylim(-5,5)

#scalar division
b = v / 2
print(b[2,])

# Plot b
## Plot the vector
ggplot() + 
  geom_line(data = v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'red') + 
  geom_line(data = b, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), size = 1) +
  xlim(-5,5) + ylim(-5,5)

#dot product to get scalar result
## Another data frame s
s = data.frame(x = c(0,-3), y = c(0,2))

dot_v_s = sum(as.matrix(v) %*% as.matrix(t(s)))
dot_v_s

#cosine 
norm_v = norm(v, '2')
norm_s = norm(s, '2')

cos_v_s = dot_v_s/(norm_v*norm_s)
NISTradianTOdeg(acos(cos_v_s))

#cross product
## Need the cross function from the pracma package
install.packages('pracma')
library(pracma)

## Define the two vectors
p = c(2,3,1)
q = c(1,2,-2)

## Outer cross product of two vectors
cat('\nCross product of vectors')
cross(p,q)

#####Multiplications#####

#####Matrices#####
A = matrix(c(1,4,2,5,3,6), nrow = 2, ncol =3)
A

B = array(c(1,4,2,5,3,6,7,8,9,1,2,5), c(3,4))
B

## Create the second matrix
B = matrix(c(6,3,5,2,4,1), nrow = 2, ncol = 3)
A + B

#subtraction
A - B

#negative matrix
C = matrix(c(-5,-3,-1,1,3,5), ncol = 3, nrow = 2)

C
-C

#matrix transposition
A
t(A)

#####Matrices#####

#####More Matrices#####
#scalar multiplication
A = matrix(c(1,4,2,5,3,6), nrow = 2, ncol =3)
A
2 * A

#dot product multiplication
B = matrix(c(9,7,5,8,6,4), nrow = 3, ncol = 2)
A
B
A %*% B

#order of operation in dot product is impt
A = matrix(c(2,6,4,8), ncol = 2, nrow = 2)
B = matrix(c(1,5,3,7), ncol = 2, nrow = 2)

A
B
A %*% B
B %*% A

#Identity Matrices
I = diag(3)
I
A = matrix(c(1,4,7,2,5,8,3,6,9), nrow = 3, ncol = 3)
A
A %*% I
I %*% A

#matrix division
#inverse
B = matrix(c(6,1,2,2), nrow = 2, ncol = 2)
B
solve(B)

#inverse for 3x3
B = matrix(c(4,6,2,2,2,2,2,4,8), nrow = 3, ncol = 3)
B
invB = solve(B)
B %*% invB

#multiplying with inverse matrix same as division
A = matrix(c(1,3,2,4), nrow = 2, ncol = 2)
A
B = matrix(c(6,1,2,2), nrow = 2, ncol = 2)
B
A %*% solve(B)

#solving systems of equations
A = matrix(c(2,6,4,2), nrow = 2, ncol = 2)
A
B = c(18,34)
B
solve(A,B)

#####More Matrices#####

#####eigenvalues and eigenvectors#####
#linear transformation
v = c(1,2)
v
A = matrix(c(2,5,3,2), ncol = 2, nrow = 2)
A
A %*% v

#2d vector to 2d vector
A = matrix(c(2,5,1,3,2,1), nrow = 3, ncol = 2)
A
A %*% v

#transformation in magnitude and amplitude-> magnitude change
library(ggplot2)
library(repr)
options(repr.plot.width=3.5, repr.plot.height=3) # Set the initial plot area dimensions

v = c(1,0)
v

A = matrix(c(2,0,0,2), ncol = 2, nrow = 2)
A
t = A %*% v
t

## Create a data frame with v and t
v = data.frame(x=c(0,v[1]), y=c(0,v[2]))
t = data.frame(x=c(0,t[1]), y=c(0,t[2]))


## Plot the vector
ggplot() + 
  geom_line(data = v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'red', size = 1) + 
  geom_line(data = t, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), size = 0.5) +
  xlim(-5,5) + ylim(-5,5)

#amplitude change
v = c(1,0)
v

A = matrix(c(0,1,-1,0), ncol = 2, nrow = 2)
A
t = A %*% v
t

## Create a data frame with v and t
v = data.frame(x=c(0,v[1]), y=c(0,v[2]))
t = data.frame(x=c(0,t[1]), y=c(0,t[2]))


## Plot the vector
ggplot() + 
  geom_line(data = v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'orange', size = 1) + 
  geom_line(data = t, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'blue', size = 1) +
  xlim(-5,5) + ylim(-5,5)

#length and amplitude change
v = c(1,0)
v

A = matrix(c(2,1,1,2), ncol = 2, nrow = 2)
A
t = A %*% v
t

## Create a data frame with v and t
v = data.frame(x=c(0,v[1]), y=c(0,v[2]))
t = data.frame(x=c(0,t[1]), y=c(0,t[2]))


## Plot the vector
ggplot() + 
  geom_line(data = v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'orange', size = 1) + 
  geom_line(data = t, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'blue', size = 1) +
  xlim(-5,5) + ylim(-5,5)

#Afine transformation in R
v = c(1,0)
v

A = matrix(c(5,3,2,1), ncol = 2, nrow = 2)
A
b = c(-2,-6)
b 
t = A %*% v + b
t

## Create a data frame with v and t
v = data.frame(x=c(0,v[1]), y=c(0,v[2]))
t = data.frame(x=c(0,t[1]), y=c(0,t[2]))


## Plot the vector
ggplot() + 
  geom_line(data = v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'orange', size = 1) + 
  geom_line(data = t, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'blue', size = 1) +
  xlim(-5,5) + ylim(-5,5)

#scalar instead of matrix to transform since affect magnitude only
v = c(1,0)
v

A = matrix(c(2,0,0,2), ncol = 2, nrow = 2)
A

t1 = A %*% v
t1
t2 = 2 * v
t2

## Create a data frame with v, t1, and t2
v = data.frame(x=c(0,v[1]), y=c(0,v[2]))
t1 = data.frame(x=c(0,t1[1]), y=c(0,t1[2]))
t2 = data.frame(x=c(0,t2[1]), y=c(0,t2[2]))


## Plot t1
ggplot() + 
  geom_line(data = v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'orange', size = 1) + 
  geom_line(data = t1, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'blue') +
  xlim(-5,5) + ylim(-5,5)

## Plot t2
ggplot() + 
  geom_line(data = v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'orange', size = 1) + 
  geom_line(data = t2, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'blue') +
  xlim(-5,5) + ylim(-5,5)

#using R to calculate eigenvector and eigenvalue
A = matrix(c(2,0,0,3), ncol = 2, nrow = 2)
A

eigens = eigen(A)
eigens$val
eigens$vec

cat('Matrix A \n')
A
cat('---------- \n')
cat(paste('First eigenvalue = ', as.character(eigens$val[1]), '\n'))
cat('First eigenvector:')
eigens$vec[,1]
cat(paste('Second eigenvalue = ', as.character(eigens$val[2]), '\n'))
cat('Second eigenvector:')
eigens$vec[,2]
cat('first eigenvalue * eigenvetor \n')
v1 = eigens$val[1] * eigens$vec[,1]
v1
cat('matrix %*% first eigenvalue')
A %*% eigens$vec[,1]
cat('second eigenvalue * eigenvetor \n')
v2 = eigens$val[2] * eigens$vec[,2]
v2
cat('matrix %*% second eigenvalue')
A %*% eigens$vec[,2]

#visualisation
## Data frames for the first eigen vector plot
w1 = data.frame(x = c(0,eigens$vec[1,1]), y = c(0,eigens$vec[2,1]))
v1 = data.frame(x = c(0,v1[1]), y = c(0,v1[2]))

## Plot the multplications with first vector
ggplot() + 
  geom_line(data = w1, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'red', size = 1) + 
  geom_line(data = v1, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), size = 0.5) +
  xlim(-5,5) + ylim(-5,5)

## Data frames for the second eigen vector plot
w2 = data.frame(x = c(0,eigens$vec[1,2]), y = c(0,eigens$vec[2,2]))
v2 = data.frame(x = c(0,v2[1]), y = c(0,v2[2]))

## Plot the multplications with first vector
ggplot() + 
  geom_line(data = w2, aes(x,y), arrow = arrow(length=unit(0.30,"cm"), ends = "first"), color = 'red', size = 1) + 
  geom_line(data = v2, aes(x,y), arrow = arrow(length=unit(0.30,"cm"), ends = "first"), size = 0.5) +
  xlim(-5,5) + ylim(-5,5)

#determining the eigenvector-eigenvalue pairs
A = matrix(c(2,0,0,2), nrow = 2, ncol = 2)
A

eigens = eigen(A)
eigens$val
eigens$vec

#visualisation
cat('Matrix A \n')
A
cat('---------- \n')
cat(paste('First eigenvalue = ', as.character(eigens$val[1]), '\n'))
cat('First eigenvector:')
eigens$vec[,1]
cat(paste('Second eigenvalue = ', as.character(eigens$val[2]), '\n'))
cat('Second eigenvector:')
eigens$vec[,2]
cat('first eigenvalue * eigenvetor \n')
v1 = eigens$val[1] * eigens$vec[,1]
v1
cat('matrix %*% first eigenvalue')
A %*% eigens$vec[,1]
cat('second eigenvalue * eigenvetor \n')
v2 = eigens$val[2] * eigens$vec[,2]
v2
cat('matrix %*% second eigenvalue')
A %*% eigens$vec[,2]

## Data frames for the first eigen vector plot
w1 = data.frame(x = c(0,eigens$vec[1,1]), y = c(0,eigens$vec[2,1]))
v1 = data.frame(x = c(0,v1[1]), y = c(0,v1[2]))

## Plot the multplications with first vector
ggplot() + 
  geom_line(data = w1, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'red', size = 1) + 
  geom_line(data = v1, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), size = 0.5) +
  xlim(-5,5) + ylim(-5,5)

## Data frames for the second eigen vector plot
w2 = data.frame(x = c(0,eigens$vec[1,2]), y = c(0,eigens$vec[2,2]))
v2 = data.frame(x = c(0,v2[1]), y = c(0,v2[2]))

## Plot the multplications with first vector
ggplot() + 
  geom_line(data = w2, aes(x,y), arrow = arrow(length=unit(0.30,"cm"), ends = "first"), color = 'red', size = 1) + 
  geom_line(data = v2, aes(x,y), arrow = arrow(length=unit(0.30,"cm"), ends = "first"), size = 0.5) +
  xlim(-5,5) + ylim(-5,5)

#another matrix example
A = matrix(c(2,1,1,2), nrow = 2, ncol = 2)
A

eigens = eigen(A)
eigens$val
eigens$vec

#visualisation
cat('Matrix A \n')
A
cat('---------- \n')
cat(paste('First eigenvalue = ', as.character(eigens$val[1]), '\n'))
cat('First eigenvector:')
eigens$vec[,1]
cat(paste('Second eigenvalue = ', as.character(eigens$val[2]), '\n'))
cat('Second eigenvector:')
eigens$vec[,2]
cat('first eigenvalue * eigenvetor \n')
v1 = eigens$val[1] * eigens$vec[,1]
v1
cat('matrix %*% first eigenvalue')
A %*% eigens$vec[,1]
cat('second eigenvalue * eigenvetor \n')
v2 = eigens$val[2] * eigens$vec[,2]
v2
cat('matrix %*% second eigenvalue')
A %*% eigens$vec[,2]

## Data frames for the first eigen vector plot
w1 = data.frame(x = c(0,eigens$vec[1,1]), y = c(0,eigens$vec[2,1]))
v1 = data.frame(x = c(0,v1[1]), y = c(0,v1[2]))

## Plot the multplications with first vector
ggplot() + 
  geom_line(data = w1, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), color = 'red', size = 1) + 
  geom_line(data = v1, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), size = 0.5) +
  xlim(-5,5) + ylim(-5,5)

## Data frames for the second eigen vector plot
w2 = data.frame(x = c(0,eigens$vec[1,2]), y = c(0,eigens$vec[2,2]))
v2 = data.frame(x = c(0,v2[1]), y = c(0,v2[2]))

## Plot the multplications with first vector
ggplot() + 
  geom_line(data = w2, aes(x,y), arrow = arrow(length=unit(0.30,"cm"), ends = "first"), color = 'red', size = 1) + 
  geom_line(data = v2, aes(x,y), arrow = arrow(length=unit(0.30,"cm"), ends = "first"), size = 0.5) +
  xlim(-5,5) + ylim(-5,5)

#eigendecomposition
A = matrix(c(3,1,2,0), nrow =2, ncol =2)
eigens = eigen(A)
Q = eigens$vec
Q

#finding lambda
L = diag(eigens$val)
L

#inverse of Q
Qinv = solve(Q)
Qinv

#transformation example
v = c(1,3)
v
t = A %*% v
t

## Data frames for the vector plot
df_v = data.frame(x = c(0,v[1]), y = c(0,v[2]))
df_t = data.frame(x = c(0,t[1]), y = c(0,t[2]))

## Plot the vectors
ggplot() + 
  geom_line(data = df_t, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), 
            color = 'blue', size = 1) + 
  geom_line(data = df_v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), 
            color = 'orange', size = 1) +
  xlim(-10,10) + ylim(-10,10)

#decomposition of same example
t = Q %*% L %*% Qinv %*% v
t

## Data frame for the vector plot
df_t = data.frame(x = c(0,t[1]), y = c(0,t[2]))

## Plot the vectors
ggplot() + 
  geom_line(data = df_t, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), 
            color = 'blue', size = 1) + 
  geom_line(data = df_v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), 
            color = 'orange', size = 1) +
  xlim(-10,10) + ylim(-10,10)

## Matrix multiplication for intermediate steps
t1 = Qinv %*% v
t1
t2 = L %*% t1
t2
t3 = Q %*% t2
t3

## Data frames for the vector plot
df_t1 = data.frame(x = c(0,t1[1]), y = c(0,t1[2]))
df_t2 = data.frame(x = c(0,t2[1]), y = c(0,t2[2]))
df_t3 = data.frame(x = c(0,t3[1]), y = c(0,t3[2]))

## Plot the vectors
ggplot() + 
  geom_line(data = df_v, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), 
            color = 'orange', size = 1) +
  geom_line(data = df_t1, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), 
            color = 'red', size = 1) + 
  geom_line(data = df_t2, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), 
            color = 'magenta', size = 1) + 
  geom_line(data = df_t3, aes(x,y), arrow = arrow(length=unit(0.30,"cm")), 
            color = 'blue', size = 1) +
  xlim(-10,10) + ylim(-10,10)    

#rank of a matrix
A = matrix(c(1,4,2,3), nrow =2, ncol =2)
eigens_A = eigen(A)
L = diag(eigens_A$val)
L

B = matrix(c(3,2,1,-3,-2,-1,6,4,2), nrow =3, ncol =3)
eigens_B = eigen(B)
L = diag(eigens_B$val)
L

#inverse of square full rank matrix
Linv = solve(diag(eigens_A$val))
Linv
Q = eigens_A$vec
Q
Qinv = solve(Q)
Qinv

Ainv = Q %*% Linv %*% Qinv
Ainv

solve(Ainv)

Ainv %*% A
A %*% Ainv

#####eigenvalues and eigenvectors#####