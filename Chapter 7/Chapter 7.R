library(here)
library(rjags)
# 7.1. T-test with equal variances

# 7.1.2. Data generation
source(here("functions","data_gen.R"))

# means parameteriztion
n1 <- 60				# Number of females
n2 <- 40				# Number of males
mu1 <- 105				# Population mean of females
mu2 <- 77.5				# Population mean of males
sigma <- 2.75				# Average population SD of both

n <- n1+n2				# Total sample size
y1 <- rnorm(n1, mu1, sigma)		# Data for females
y2 <- rnorm(n2, mu2, sigma)		# Date for males
y <- c(y1, y2)				# Aggregate both data sets
x <- rep(c(0,1), c(n1, n2))		# Indicator for male
boxplot(y ~ x, col = "grey", xlab = "Male", ylab = "Wingspan (cm)", las = 1)

# effects parameterization
n <- n1+n2				# Total sample size
alpha <- mu1				# Mean for females serves as the intercept
beta <- mu2-mu1				# Beta is the difference male-female
E.y <- alpha + beta*x			# Expectation
y.obs <- rnorm(n = n, mean = E.y, sd = sigma)	# Add random variation
boxplot(y.obs ~ x, col = "grey", xlab = "Male", ylab = "Wingspan (cm)", las = 1)

### 7.1.3. Analysis using R
fit1 <- lm(y ~ x)			# Analysis of first data set
fit2 <- lm(y.obs ~ x)			# Analysis of second data set
summary(fit1)
summary(fit2)

anova(fit1)
anova(fit2)

model.matrix(fit1)
model.matrix(fit2)

### 7.1.4. Analysis using rJAGS
# Define BUGS model

sink(paste(here(),"Chapter 7","ttest.txt", sep = "/"))
cat("
    model {

    # Likelihood
    for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau) 
    mu[i] <- mu1 + delta *x[i]
    #residual[i] <- y[i] - mu[i]		# Define residuals
    }

    # Priors
    mu1 ~ dnorm(0,0.001)		# expected mean, female wingspan	
    delta ~ dnorm(0,0.001)			# difference, male from female wingspan
    tau <- 1/ (sigma * sigma) # Precision = 1/variance
    sigma ~ dunif(0, 50)      # standard deviation
    
    # Derived quantities: one of the greatest things about a Bayesian analysis
    mu2 <- mu1 + delta			# male wingspan
    }
    ",fill=TRUE)
sink()

# Bundle data
data <- falcons.ws()
dat <- data[c("x", "y", "n")]

# Inits function
inits <- function() list(mu1 = rnorm(1), delta = rnorm(1), sigma = rlnorm(1)) # try uniform mu1 = rnorm(1), 

# Parameters to estimate
params <- c("mu1","mu2", "delta", "sigma")

# Set up Model
jagsModel <- jags.model(file = "Chapter 7/ttest.txt",
                        data = dat,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100) # Burn-in

Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000) #Draws from posterior
plot(Samples)
summary(Samples)

### 7.2. T-test with unequal variances

### 7.2.2. Data generation
data <- falcons.ws(sigma1 = 3, sigma2 = 2.5)

boxplot(data$y ~ data$x, col = "grey", xlab = "Male", ylab = "Wingspan (cm)", las = 1)


### 7.2.3. Analysis using R
t.test(data$y ~ data$x)


### 7.2.4. Analysis using rJAGS
# Define BUGS model
sink(paste(here(),"Chapter 7","h.ttest.txt", sep = "/"))
cat("
    model {
    # Likelihood
    for (i in 1:n1) {
    y1[i] ~ dnorm(mu1, tau1) # mean of female wingspan 
    }
    
    for (i in 1:n2) {
    y2[i] ~ dnorm(mu2, tau2) # mean of male wingspan
    }
    
    # Priors
    mu1 ~ dnorm(0,0.001)
    mu2 ~ dnorm(0,0.001)
    tau1 <- 1 / ( sigma1 * sigma1)
    sigma1 ~ dunif(0, 10) 
    tau2 <- 1 / ( sigma2 * sigma2)
    sigma2 ~ dunif(0, 10)
    
    # Derived quantities
    delta <- mu2 - mu1 # Difference in means
    }
    ",fill=TRUE)
sink()

# Bundle data
dat <- data[c("y1", "y2", "n1", "n2")] 

# Inits function
inits <- function() list(mu1 = rnorm(1), mu2 = rnorm(1), sigma1 = rlnorm(1), sigma2 = rlnorm(1))

# Parameters to estimate
params <- c("mu1","mu2", "delta", "sigma1", "sigma2")

# Set up Model
jagsModel <- jags.model(file = "Chapter 7/h.ttest.txt",
                        data = dat,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 500) # Burn-in

Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 2000) #Draws from posterior
plot(Samples)
summary(Samples)

# Questions:
# Why log transformed initial values for sigma
# We don't want negative sd