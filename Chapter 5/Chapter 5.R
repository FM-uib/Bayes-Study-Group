library(here)
library(rjags)

### 5.2. Data generation
# Generate two samples of body mass measurements of male peregrines
y10 <- rnorm(n = 10, mean = 600, sd = 30) # Sample of 10 birds
y1000 <- rnorm(n = 1000, mean = 600, sd = 30) # Sample of 1000 birds

# Plot data
xlim = c(450, 750)
par(mfrow = c(2,1))
hist(y10, col = 'grey ', xlim = xlim, main = 'Body mass (g) of 10 male peregrines')
hist(y1000, col = 'grey', xlim = xlim, main = ' Body mass (g) of 1000 male peregrines')

### 5.3. Analysis using R
summary(lm(y10 ~ 1))

### 5.4. Analysis using rJAGS

# Save BUGS description of the model

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


# Package all the stuff to be handed over to WinBUGS
# Bundle data
data <- list(mass = y1000, nobs = length(y1000))

# Function to generate starting values
inits <- function(){
  list(population.mean = rnorm(1,580), population.sd = runif(1, 1, 30))}

# Parameters to be monitored (= to estimate)
params <- c("population.mean", "population.sd", "population.variance")

# Compiling and Initiating the model
# Drawing Posterior to be discarded (burn-in/adapt)
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
