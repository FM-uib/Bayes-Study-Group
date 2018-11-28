## 10.5.1

sink("anova.2w.Ch10.txt")
cat("
    model {
    
    # Priors
    alpha ~ dnorm(0,0.001) ## intercept
    sigma ~ dunif(0, 100)
    beta.pop[1] <- 0 ## zero effect for intercept
    for(k in 2:5) {
    beta.pop[k] ~ dnorm(0, 0.001)
    }
    
    beta.elev[1] <- 0
    beta.elev[2] ~ dnorm(0, 0.001)
    beta.elev[3] ~ dnorm(0, 0.001)
    
    # Likelihood
    for (i in 1:n) {
    wing[i] ~ dnorm(mean[i], tau) 
    mean[i] <- alpha + beta.pop[pop[i]] + beta.elev[elev[i]]
    }
    
    # Derived quantities
    tau <- 1/ (sigma * sigma)
    
    }
    ",fill=TRUE)
sink()

##10.5.2
sink("anova.2w.Interactive.Ch10.txt")
cat("
    model {
    
    # Priors
    sigma ~ dunif(0, 100)
    for(i in 1:n.pop){
    for(j in 1:n.elv){
    group.mean[i,j] ~ dnorm(0, 0.0001)
    }
    }
    
    # Likelihood
    for (i in 1:n) {
    wing[i] ~ dnorm(mean[i], tau) 
    mean[i] <- group.mean[pop[i], elev[i]]
    }
    
    # Derived quantities
    tau <- 1/ (sigma * sigma)
    
    }
    ",fill=TRUE)
sink()