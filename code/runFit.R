

# this code is to be run after formatted_CodData is generated by baseDriver


set.seed(1) # set the rng for repeatability

nruns = 100
mles0 =  vector("numeric", length=nruns) # 2 states
mles1 = vector("numeric", length=nruns) # 3 states
mles2 = vector("numeric", length=nruns) # 3 states, fit to vertical movement
mles3 = vector("numeric", length=nruns) # 3 states, fit to vertical movement with quadratic term

m0List = vector("list", length = nruns)
m1List = vector("list", length = nruns)
m2List = vector("list", length = nruns)
m3List = vector("list", length = nruns)

nStates=3
for (foo in 1:nruns){
  # randomize starting values
  mu0 = runif(nStates,0,40)
  sigma0 = runif(nStates,0,10)
  stepPar0 = c(mu0,sigma0)
  angleMean0 = runif(nStates, -pi, pi)
  kappa0 = runif(nStates,0,5)
  anglePar0 = c(angleMean0,kappa0)
  
  tryCatch(
    {
      m0 = fitHMM(data=formatted_CodData,nbStates=2,stepPar0=stepPar0[c(1,2,4,5)],anglePar0=anglePar0[c(1,2,4,5)],formula=~1)
      m1 = fitHMM(data=formatted_CodData,nbStates=nStates,stepPar0=stepPar0,anglePar0=anglePar0,formula=~1)
      m2 = fitHMM(data=formatted_CodData,nbStates=nStates,stepPar0=stepPar0,
                            anglePar0=anglePar0, formula = ~vertical)
      m3 = fitHMM(data=formatted_CodData,nbStates=nStates,stepPar0=stepPar0,
               anglePar0=anglePar0, formula = ~vertical+I(vertical^2))
      m0MLE = -m0$mod$minimum
      m1MLE = -m1$mod$minimum
      m2MLE = -m2$mod$minimum
      m3MLE = -m3$mod$minimum
    },
    error = function(err) {
      # may get non-finite results - error display works, but other values are not assigned as expected
      print((paste("MY_ERROR:  ",err)))
      m0MLE = 0
      m1MLE = 0
      m2MLE = 0
      m3MLE = 0
      m0 = NA
      m1 = NA
      m2 = NA
      m3 = NA
    }
  )
  mles0[foo] = m0MLE
  mles1[foo] = m1MLE
  mles2[foo] = m2MLE
  mles3[foo] = m3MLE
  m0List[[foo]] = m0
  m1List[[foo]] = m1
  m2List[[foo]] = m2
  m3List[[foo]] = m3
}

table(round(mles0, digits = -1))
table(round(mles1, digits = -1))
table(round(mles2, digits = -1))
table(round(mles3, digits = -1))

# locate best performers in each group (last is not necessarily best)
m0Ind = which.max(mles0)
m1Ind = which.max(mles1)
m2Ind = which.max(mles2)
m3Ind = which.max(mles3)

m0Best = m0List[[m0Ind]]
m1Best = m1List[[m1Ind]]
m2Best = m2List[[m2Ind]]
m3Best = m3List[[m3Ind]]

# compare AIC of representatives of each model
print(AIC(m0Best,m1Best,m2Best,m3Best))

# m1 has the best result of the 3 state models

# general visualizations
plot(m1Best, animals = 1, ask = F)

# residuals and qq plots
plotPR(m1Best)

# states vs time
plotStates(m1Best)

# view 2 state model for visual comparison
plot(m0Best, ask=F)
