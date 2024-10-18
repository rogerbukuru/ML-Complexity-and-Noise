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
Y   = matrix((dat$Y==1)*1,ncol = 1)

# Plot the data to see the pattern:
cols = c('red','blue')
plot(X[,2]~X[,1],col = cols[Y+1] ,pch = 16)


# Specify some appropriate activation functions:
# Hidden layers
sig1 = function(z)
{
  tanh(z)
}

# Output layer
sig2 = function(z)
{
  1/(1+exp(-z))
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
   
   dims = c(p,m,q)

   # Populate weight and bias matrices:
   index = 1:(dims[1]*dims[2])
   W1    = matrix(theta[index],dims[1],dims[2])
   index = max(index)+1:(dims[2]*dims[3])
   W2    = matrix(theta[index],dims[2],dims[3])
   index = max(index)+1:(dims[2])
   b1    = matrix(theta[index],dims[2],1)
   index = max(index)+1:(dims[3])
   b2    = matrix(theta[index],dims[3],1)
   
   ones = matrix(1,1,N)
   # Evaluate the updating equation in matrix form
   #for(i in 1:N)
   #{
     A0 = t(X)
     A1 = sig1(t(W1)%*%A0+b1%*%ones)
     A2 = sig2(t(W2)%*%A1+b2%*%ones)
   #}
     pi_hat = t(A2)
     error = rep(0,N)
     wh0   = which(Y==0)
     wh1   = which(Y==1)
     error[wh0] = log(1-pi_hat[wh0])
     error[wh1] = log(pi_hat[wh1])
     #-sum(Y*log(pi_hat)-(1-Y)*log(1-pi_hat))
     
   # Evaluate an appropriate objective function and return some predictions:
   E1 = -sum(error)
   E2 = E1/N+nu*(sum(W1^2)+sum(W2^2))/N
   # Return a list of relevant objects:
   return(list(A2 = A2,A1 = A1, E1 = E1, E2 = E2))
}

# Create an objective function and evaluate it at a random coordinate in the 
# parameter space:
nu  = 0.1
m   = 9

p     = dim(X)[2] 
q     = dim(Y)[2]
npars = p*m+m*q+m+q
theta_rand = runif(npars,-1,1)

res_model = neural_net(X,Y,theta_rand,m,nu)
res_model

nu = 0.1
obj_pen = function(pars)
{
  res_model = neural_net(X,Y,pars,m,nu)
  return(res_model$E2)
}
obj_pen(theta_rand)

# Fit the neural network using a standard optimizer in R:
res_opt = nlm(obj_pen,theta_rand, iterlim = 1000)
#res_opt$estimate

# Draw a response curve over the 2D input space to see
# what pattern the neural network predicts
M  = 100
x1 = seq(-4,4,length = M)
x2 = seq(-4,4,length = M)
xx1 = rep(x1,M)
xx2 = rep(x2,each = M)

abline(v = x1,h= x2)
points(xx2~xx1,pch = 16, col = 'purple',cex = 2)

XX = cbind(xx1,xx2) 
YY = matrix(1,M^2,1)
res_fitted = neural_net(XX,YY,res_opt$estimate,m,nu)

plot(XX[,2]~XX[,1], pch = 16, col = color.gradient(res_fitted$A2),cex = 1/2)
points(X[,2]~X[,1], col = cols[Y+1], pch = 16)


#===========================================================
# Validation Analysis
#===========================================================

N   = dim(X)[1]
set.seed(2024)
set     = sample(1:N,0.8*N,replace = FALSE)
X_train = as.matrix(X[set,])
Y_train = as.matrix(Y[set,])
X_val = as.matrix(X[-set,])
Y_val = as.matrix(Y[-set,])

#NOW REDEFINE PEN OBJ USING ONLY TRAINING DATA!!!

obj_pen = function(pars)
{
  res_model = neural_net(X_train,Y_train,pars,m,nu)
  return(res_model$E2)
}


n_nus  = 10
nu_seq = exp(seq(-5,0,length = n_nus))
E_val  = rep(NA,n_nus)

for(i in 1:n_nus)
{
  nu         = nu_seq[i]
  res_opt    = nlm(obj_pen,theta_rand, iterlim = 1000)

  # Extract Validation Error:
  res_val  = neural_net(X_val,Y_val,res_opt$estimate,m,0)
  E_val[i] = res_val$E1
   
  # Response Curve:
  res_fitted = neural_net(XX,YY,res_opt$estimate,m,nu)
  plot(XX[,2]~XX[,1], pch = 16, col = color.gradient(res_fitted$A2),cex = 1/2,
  	main = substitute(nu==a,list(a = nu)))
  points(X[,2]~X[,1], col = cols[Y+1], pch = 16)
 
}

plot(E_val~nu_seq,type ='b' )


