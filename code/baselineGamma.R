
# basic fit for step length for comparison - justifies why we're doing an HMM
# no easy setup to fit a von mises distribution

require(MASS)

stepData = formatted_CodData$step[2:(nEntries-1)] # omit NA entries (first and last entry)

gammaFit = fitdistr(stepData, "gamma")




xfit = seq(min(stepData), max(stepData), length = 100) 
yfit = dgamma(xfit, rate = gammaFit$estimate[2], shape = gammaFit$estimate[1]) 

# histogram with the gamma fit as a line on top
hist(stepData, freq = FALSE, xlab = "Step Size", main = "Histogram of Step Size")
lines(xfit, yfit, col = "black", lwd = 2)

# perform fit using a single state using moveHMM package
# gives equivalent a single fit and gives a consistent gamma fit to the first part
# also gives a von mises fit which is not as easy to do on its own
set.seed(1)
nStates = 1

mu0 = runif(nStates,0,40)
sigma0 = runif(nStates,0,10)
stepPar0 = c(mu0,sigma0)
angleMean0 = runif(nStates, -pi, pi)
kappa0 = runif(nStates,0,5)
anglePar0 = c(angleMean0,kappa0)

singleStateFit = fitHMM(data=formatted_CodData,nbStates=nStates,stepPar0=stepPar0,anglePar0=anglePar0,formula=~1)
plot(singleStateFit)
plotPR(singleStateFit)

AIC(singleStateFit) # 2715.794