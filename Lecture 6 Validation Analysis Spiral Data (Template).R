rm(list = ls())
# A Function for creating continuous colour palette. I.e., colour gradient bins and
# associated colours:
library(colorspace)
color.gradient = function(x, colors=c('magenta','white','lightblue'), colsteps=50)
{
  colpal = colorRampPalette(colors)
  return( colpal(colsteps)[ findInterval(x, seq(min(x),max(x), length=colsteps)) ] )
}

# Read in the data and populate a design-matrix + response matrix:
dat = read.table('Spiral_Data_2022.txt',h = T)
X   = as.matrix(dat[,-3])
Y   = 

# Plot the data to see the pattern:
cols = c('red','blue')
plot(, pch = 16)


# Specify some appropriate activation functions:
# Hidden layers
sig1 = function(z)
{
  
}

# Output layer
sig2 = function(z)
{
  
}

# Write a function that evaluates a neural network. 
# Takes arguments:
# X     - Design matrix
# Y     - Response matrix
# theta - A vector of parameters
# m     - Number of nodes on the hidden layer
# nu    - A regularisation parameter 

neural_net = function(X,Y,theta,m,nu)
{
   # Infer dimensions:
   N = dim(X)[1]
   p = dim(X)[2]
   q = dim(Y)[2]
   
   dims = c(p, m, q) #specificy dimensions of the different layers

   # Populate weight and bias matrices:
   index = 1:(dims[1]*dims[2])
   W1    = matrix(theta[index],dims[1], dims[2])
   index = 
   W2    = matrix(,)
   index = 
   b1    = matrix(,)
   index =
   b2    = matrix()
   
   ones = 
   # Evaluate the updating equation in matrix form
   #for(i in 1:N)
   #{
     A0 = 
     A1 = 
     A2 = 
   #}
     pi_hat = 
     error = 
     
   # Evaluate an appropriate objective function and return some predictions:
   E1 = 
   E2 = 
   # Return a list of relevant objects:
   return(list(A2 = A2,A1 = A1, E1 = E1, E2 = E2))
}

# Create an objective function and evaluate it at a random coordinate in the 
# parameter space:
nu  = 0
m   = 

p     = 
q     = 
npars = p*m+m*q+m+q #input layer, hidden layer, biases on hidden m biases on output
theta_rand = 


nu = 0
obj_pen = function(pars)
{
  res_model = 
  return(
}
obj_pen(theta_rand)

# Fit the neural network using a standard optimizer in R:
res_opt = nlm(obj_pen,theta_rand, iterlim = 1000)
res_opt

# Draw a response curve over the 2D input space to see
# what pattern the neural network predicts
M  =
x1 = 
x2 = 
xx1 = 
xx2 = 

abline()

XX = 
YY = 
res_fitted = 

plot()
points(X[,2]~X[,1], col = cols[Y+1], pch = 16)


#===========================================================
# Validation Analysis
#===========================================================

#N   = 
#set = 
#X_train = 
#Y_train =
#X_val = 
#Y_val =
