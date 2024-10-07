rm(list = ls())
library(ggplot2)
library(scales)
# Read in the data:
set.seed(2022)
dat = read.table('Shapes_Images_2022.txt',h= T)

# See what it looks like:
head(dat)
tail(dat)

# Separate predictors and responses
X = as.matrix(dat[,-1])
Y = matrix(dat[,1],ncol = 1)

# Plot images by colour-coding pixel intensities using viridis colour palette
# 
par(mfrow = c(4,4))
for(i in 1:16){
image(matrix(X[i,],10,10,byrow = TRUE),col = viridis_pal()(50) )
}

# quartz() # windows()
# Write a function which evaluates the multi-class perceptron learning algorithm
# Takes parameters:
# X       - predictors
# Y       - response
# iterlim - Max no. of iterations
# plt     - Logical par. Set TRUE to plot weight-images for each class. 

MPLA = function(X, Y,iterlim = 1000, plt = FALSE)
{
  # Dimensions and initialize
  N =
  k = 
  X = 
  Y = 
  W =
  
  # Predict based on the present parameter set, and check # of misclassifications:
  Yhat  = 
  
  miss  =
  error = 
  count = 
  error
  
  Wmin     = 
  Yhat_min = 
  # While all not perfectly classified and limit not exceeded 
  while()
  {
    # Pick a misclassified  observation and perturb:
    count = count +1
    
    pick  = 
    
    W
    W
    
    Yhat         = 
    
    miss         = 
    error[count] = 
    # Keep the best h in your pocket:

   
    
  }

  # Return the error trajectory, par estimate and predictions
  return(list(error = error, W = Wmin, Yhat = Yhat_min, count = count))
}

res = MPLA(X,Y,10000,FALSE)

#par(mfrow = c(1,1))
#plot(res$error,type = 's')


res
