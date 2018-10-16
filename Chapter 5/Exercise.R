library(here)
library(rjags)
library(ggplot2)
theme_set(theme_classic())

# Generate two samples of body mass measurements of male peregrines
y10 <- rnorm(n = 10, mean = 600, sd = 30) # Sample of 10 birds
y1000 <- rnorm(n = 1000, mean = 600, sd = 30) # Sample of 1000 birds

model<-"
    model {
    # Likelihood
    for(i in 1:nobs){
    mass[i] ~ dnorm(population.mean, precision) # Normal parameterized by precision
    }

    # Priors
    population.mean ~ dunif(500,590)		
    precision <- 1 / population.variance	# Precision = 1/variance
    population.variance <- population.sd * population.sd
    population.sd ~ dunif(0,100)
    }
    "
model.spec <- textConnection(model)


# Exercises

# 1 Informative Priors:
# We have knowledge that the weight of falcons varies between 500 and 590
# experiment with the prior for the standard deviation

# Bundle data
data <- list(mass = y1000, nobs = length(y1000))

# Function to generate starting values
inits <- list(population.mean = rnorm(1,550), 
              population.sd = runif(1, 1, 30))

# Parameters to be monitored (= to estimate)
params <- c("population.mean", 
            "population.sd", 
            "population.variance")

# Compiling and Initiating the model
jagsModel <- jags.model(file= model.spec,
                        data=data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100)
Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000)
plot(Samples)
summary(Samples)

# plotting different posteriors
load(file = here("Chapter 5","Exercise_1"))

#density between wide and narrow mean prior
ggplot(data = mean.550.wide.prior, aes(x = population.mean))+geom_density(fill = "grey90", alpha = .50)+
  geom_density(data = mean.550.narrow.prior, aes(x = population.mean), fill = "grey70", alpha = .)

# density with mean of the data outside of prior
ggplot(data = mean.600.wide.prior, aes(x = population.mean))+ geom_density(fill = "grey80")

# 2 Run the model with the small and large dataset and explain the differences

y10 <- rnorm(n = 10, mean = 600, sd = 30) # Sample of 10 birds
y1000 <- rnorm(n = 1000, mean = 600, sd = 30) # Sample of 1000 birds

model<-"
    model {
    # Likelihood
    for(i in 1:nobs){
    mass[i] ~ dnorm(population.mean, precision) # Normal parameterized by precision
    }

    # Priors
    population.mean ~ dunif(0,1000)		
    precision <- 1 / population.variance	# Precision = 1/variance
    population.variance <- population.sd * population.sd
    population.sd ~ dunif(0,100)
    }
    "
model.spec <- textConnection(model)

data1 <- list(mass = y1000, nobs = length(y1000))
data2 <- list(mass = y10, nobs = length(y10))

inits <- list(population.mean = rnorm(1,550), 
              population.sd = runif(1, 1, 30))

params <- c("population.mean", 
            "population.sd", 
            "population.variance")

jagsModel <- jags.model(file= model.spec,
                        data=data1,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100)
Samples1 <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000)

load(file = here("Chapter 5", "Exercise_2.rda"))

plot(Samples1)
summary(Samples1)
plot(Samples2)
summary(Samples2)

xlim = c(450, 750)
par(mfrow = c(2,1))
hist(y10, col = 'grey ', xlim = xlim, main = 'Body mass (g) of 10 male peregrines')
hist(y1000, col = 'grey', xlim = xlim, main = ' Body mass (g) of 1000 male peregrines')
