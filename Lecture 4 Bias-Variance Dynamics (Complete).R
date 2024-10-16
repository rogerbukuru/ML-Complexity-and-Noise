rm(list=ls())
# Set up a target function/pattern:
f = function(x){sin(pi*x)}

# Write a function that generates a data set of size N 
# with pattern f. Return a list of predictors (x) and responses (y).
dgp = function(N,f,sig2)
{
   x = runif(N,-1,1)
   e = rnorm(N,0,sqrt(sig2))
   y = f(x)+e
   return(list(y = y, x = x))
}
dat = dgp(2,f,0)
dat


# Plot a single realization of the data and overlay the target function:
plot(dat$y~dat$x,xlim = c(-1,1), ylim = c(-1,1),col  = 'red',cex = 2)
xx = seq(-1,1,1/100)
lines(f(xx)~xx)

mod_h0 = lm(y~1,data = dat)
pred_h0 = predict(mod_h0,data.frame(x = xx))
mod_h1 = lm(y~x,data = dat)
pred_h1 = predict(mod_h1,data.frame(x = xx))
lines(pred_h0~xx, col = 'blue',lwd = 2)
lines(pred_h1~xx, col = 'purple', lwd = 2)

bias_var = function(N = 2, order = 1, sig2 = 0,M = 5000, dx = 1/100)
{
	x_lat = seq(-1,1,dx)
	N_dx  = length(x_lat)
	g_bar = rep(0,N_dx)
	G_d   = matrix(0,M,N_dx)
	
	errors = rep(0,M)
	for(i in 1:M)
	{
		res_data = dgp(N,f,sig2)
		if(order == 0)
		{
			 res_model = lm(y~1,data = res_data)
		}
		if(order > 0)
		{
			 res_model = lm(y~poly(x,order),data = res_data)
		}
		g_D     = predict(res_model,data.frame(x = x_lat))
		g_bar   = g_bar + g_D # Calculating the sum part in the averaging
		G_d[i,] = g_D
		
		dat_OOS  = dgp(N,f,sig2)
		yhat_OOS = predict(res_model,data.frame(x=dat_OOS$x))
		errors[i] = mean((yhat_OOS-dat_OOS$y)^2)
		
	}
	g_bar = g_bar/M # Finally calculate the average

	test_error = mean(errors)
	phi_X = 1/2
	bias2 = sum((g_bar-f(x_lat))[-N_dx]^2*phi_X*dx)
	
	ones = matrix(1,M,1)
	var_at_x = colSums((G_d-ones%*%g_bar)^2)/M
	var      = sum(var_at_x[-N_dx]*phi_X*dx)
	
	return(list(bias2 = bias2,var = var, both = bias2+var, test_error =test_error, g_bar = g_bar))	
}

rbind(bias_var(N=3, order = 0, sig2 = 0.1),
	    bias_var(N=3, order = 1, sig2 = 0.1),
	    bias_var(N=3, order = 2, sig2 = 0.1))

#lines(res$g_bar~xx,lty = 2,lwd = 2,col = 'magenta')
#res$bias2

#res1 = bias_var(N=5, order = 1, sig2 = 0)
#res2 = bias_var(N=5, order = 2, sig2 = 0)
#res3 = bias_var(N=5, order = 3, sig2 = 0)

#plot(dat$y~dat$x,xlim = c(-1,1), ylim = c(-1,1),col  = 'red',cex = 2)
#xx = seq(-1,1,1/100)
#lines(f(xx)~xx)
#lines(res1$g_bar~xx,lty = 2, col = 'magenta',lwd = 2)
#lines(res2$g_bar~xx,lty = 2, col = 'darkgreen', lwd = 2)
#lines(res3$g_bar~xx,lty = 2, col = 'red', lwd = 2)
