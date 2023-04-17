# Sensitivity analysis

# Firstly, we use the Morris method which was provided from `sensitivity` package.
library(sensitivity)
library(reshape2)

## Define the testing parameters (44)
factors <- c(  "BP",
               "fup",
               "Kon_cardiolipin",
               "Koff_cardiolipin",
               "Kon_DNA",
               "Koff_DNA",
               "Kon_mtDNA",
               "Koff_mtDNA",
               "CL_renal",
               "CL_hepatic")

## Parameter uncertainty (set up the uncertainty for each parameter). Here we set the 10% variation in the testing parameters.
LL <- 0.9 # 10% lower limit
UL <- 1.1 # 10% upper limit
# Define the lower and upper limits that will be the input to the morris function
binf <- c(parameters["BP"]*LL, 
          parameters["fup"]*LL,
          parameters["Kon_cardiolipin"]*LL,
          parameters["Koff_cardiolipin"]*LL,
          parameters["Kon_DNA"]*LL,
          parameters["Koff_DNA"]*LL,
          parameters["Kon_mtDNA"]*LL,
          parameters["Koff_mtDNA"]*LL,
          parameters["CL_renal"]*LL,
          parameters["CL_hepatic"]*LL)
bsup <-  c(parameters["BP"]*UL, 
           parameters["fup"]*UL,
           parameters["Kon_cardiolipin"]*UL,
           parameters["Koff_cardiolipin"]*UL,
           parameters["Kon_DNA"]*UL,
           parameters["Koff_DNA"]*UL,
           parameters["Kon_mtDNA"]*UL,
           parameters["Koff_mtDNA"]*UL,
           parameters["CL_renal"]*UL,
           parameters["CL_hepatic"]*UL)

# Check the convergence of the sensitivity index, 
# set up the sequence of the sampling number. 
# After that, we can apply the for-loop to do the morris sensitivity analysis and link with numerical analysis from `deSolve::ode` function. 
# In this case, I examine the parameter sensitivity under the exposure time at 8-hour (stop point of the first exposure) and investigate the parameter sensitivity. Also, we only focused on the parameter effect on venous blood concentration.


sample <- seq(from = 1, to = 2, by = 1)
for (i in 1:length(sample)) {
  set.seed(12345)
  x <- morris(model = NULL, factors = factors, r = sample[i],
              design = list(type = "oat", levels = 6, grid.jump = 3), 
              binf = binf, bsup = bsup, scale = TRUE)
  
  for (iteration in 1:nrow(x$X)) { 
    parameters["BP"] = x$X[iteration,"BP"]
    parameters["fup"] = x$X[iteration,"fup"]
    parameters["Kon_cardiolipin"] = x$X[iteration,"Kon_cardiolipin"]
    parameters["Koff_cardiolipin"] = x$X[iteration,"Koff_cardiolipin"]
    parameters["Kon_DNA"] = x$X[iteration,"Kon_DNA"]
    parameters["Koff_DNA"] = x$X[iteration,"Koff_DNA"]
    parameters["Kon_mtDNA"] = x$X[iteration,"Kon_mtDNA"]
    parameters["Koff_mtDNA"] = x$X[iteration,"Koff_mtDNA"]
    parameters["CL_renal"] = x$X[iteration,"CL_renal"]
    parameters["CL_hepatic"] = x$X[iteration,"CL_hepatic"]
    
    # We focus on time at 24 hour
    t_end <- 24 #[h] time of the end of simulation
    times <- seq(0, t_end, by = 1) #time of simulation
    
    # Integrate
    tmp = ode(times = times, func = PBPKModel, y = state, parms = parameters)
    if (iteration == 1) { # initialize
      results = tmp[2,-1]
      sampled.parms = c(parameters["BP"], 
                        parameters["fup"],
                        parameters["Kon_cardiolipin"],
                        parameters["Koff_cardiolipin"],
                        parameters["Kon_DNA"],
                        parameters["Koff_DNA"],
                        parameters["Kon_mtDNA"],
                        parameters["Koff_mtDNA"],
                        parameters["CL_renal"],
                        parameters["CL_hepatic"])
    } else { # accumulate
      results = rbind(results, tmp[2,-1])
      sampled.parms = rbind(sampled.parms,
                            c(parameters["BP"], 
                              parameters["fup"],
                              parameters["Kon_cardiolipin"],
                              parameters["Koff_cardiolipin"],
                              parameters["Kon_DNA"],
                              parameters["Koff_DNA"],
                              parameters["Kon_mtDNA"],
                              parameters["Koff_mtDNA"],
                              parameters["CL_renal"],
                              parameters["CL_hepatic"]))
    }
  }
  
  # 
  tell(x, results[,"PL"]) # We focus on parameter effect on venous blood concentration
  
  if (i == 1){
    X <- apply(abs(x$ee), 2, mean)  
  } else {
    X <- rbind(X, apply(abs(x$ee), 2, mean))
  }
  print(paste("finished iteration",i))
}

## Number of repetitions and parameter sensitivity


# Manipulate the output result for plotting
row.names(X) <- sample
meltX <- reshape::melt(X, as.is = TRUE)
ee_lim <- c(min(abs(x$ee)), max(abs(x$ee)))
library(viridis) # use viridis to create distinct colors 
par(mar=c(4,4,2,1))
plot(sample, subset(meltX, X2 == factors[1])[1,], type="b", col=viridis_pal()(12)[1],
     frame.plot = FALSE, 
     xlab="number of repetitions",
     ylab=expression(paste(mu,"*")), ylim=ee_lim, xlim = c(1,2))
for( i in 2:4){
  lines(sample, subset(meltX, X2 == factors[i])[2,], type="b", col=viridis_pal()(12)[i])
}
text(2, subset(meltX, X1 == 2)[2,], factors, pos = 4)

plot(meltX$X2,meltX$value)

## Mean OAT sensitivities and standard deviation

## This visualization provides the information to examine the parameter sensitivity and the relationships 
##between model parameters and its related output. According to the output result, we can find the relationship 
##between the parameter values and model outputs are between linear and monotonic.

plot(x, xlim=ee_lim, main ="number of repetitions = 2")
abline(0,1) # non-linear and/or non-monotonic
abline(0,0.5, lty = 2) # monotonic
abline(0,0.1, lty = 3) # almost linear
legend("bottomright", legend = c("non-linear and/or non-monotonic",
                                 "monotonic",
                                 "linear"), lty = c(1:3))
par(mfrow = c(2,2))
for (i in 1:4){
  plot(x$X[,i],x$y,main=x$factors[i],xlab="",ylab="predict conc.")
}


