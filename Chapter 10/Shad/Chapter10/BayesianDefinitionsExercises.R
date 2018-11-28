## text file for rjags exercise1
sink("Exercise1.Ch10.txt")
cat("
    model {
    
    # Priors
    sigma ~ dunif(0, 100)
    for(i in 1:n.region){
    for(j in 1:n.hab){
    group.mean[i,j] ~ dnorm(0, 0.0001)
    }
    }
    
    # Likelihood
    for (i in 1:n) {
    mass[i] ~ dnorm(mean[i], tau)
    mean[i] <- group.mean[region[i], hab[i]]
    }
    
    # Derived quantities
    tau <- 1/ (sigma * sigma)
    
    }
    ",fill=TRUE)
sink()

## text file for rjags exercise2
sink("Exercise2.Ch10.txt")
cat("
    model {

    # Priors
    sigma ~ dunif(0, 100)
    for(i in 1:n.region){
      for(j in 1:n.landuse){
        group.mean[i,j] ~ dnorm(0, 0.0001)
      }
    }

    # Likelihood
    for (i in 1:n) {
    mean.density[i] ~ dnorm(mean[i], tau)
    mean[i] <- group.mean[region[i], landuse[i]]
    }

    # Derived quantities
    tau <- 1/ (sigma * sigma)

     for(j in 1:8){
     diff.density[j] <- group.mean[j,1] - group.mean[j,2]
      }


    }
    ",fill=TRUE)
sink()