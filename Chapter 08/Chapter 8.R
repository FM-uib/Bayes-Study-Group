library(here)
library(rjags)

### 8.2. Data generation
n <- 16					# Number of years
a = 40					# Intercept
b = -1.5				# Slope
sigma2 = 25				# Residual variance

x <- 1:16 				# Values of covariate year
eps <- rnorm(n, mean = 0, sd = sqrt(sigma2))
y <- a + b*x + eps			# Assemble data set
plot((x+1989), y, xlab = "Year", las = 1, ylab = "Prop. occupied (%)", cex = 1.2)

### 8.3. Analysis using R
print(summary(lm(y ~ I(x+1989))))
abline(lm(y~ I(x+1989)), col = "blue", lwd = 2)


### 8.4. Analysis using WinBUGS

### 8.4.1. Fitting the model
# Write model
sink(here("models","linreg.txt"))
cat("
    model {
    
    # Priors
    alpha ~ dnorm(0,0.001)
    beta ~ dnorm(0,0.001)
    sigma ~ dunif(0, 100)
    
    # Likelihood
    for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau) 
    mu[i] <- alpha + beta*x[i]
    }
    
    # Derived quantities
    tau <- 1/ (sigma * sigma)
    p.decline <- 1-step(beta)		# Probability of decline
    
    # Assess model fit using a sums-of-squares-type discrepancy
    for (i in 1:n) {
    residual[i] <- y[i]-mu[i]		# Residuals for observed data
    predicted[i] <- mu[i]		# Predicted values
    sq[i] <- pow(residual[i], 2)	# Squared residuals for observed data
    
    # Generate replicate data and compute fit stats for them
    y.new[i] ~ dnorm(mu[i], tau) # one new data set at each MCMC iteration
    sq.new[i] <- pow(y.new[i]-predicted[i], 2)	# Squared residuals for new data
    }
    fit <- sum(sq[])			# Sum of squared residuals for actual data set
    fit.new <- sum(sq.new[])		# Sum of squared residuals for new data set
    test <- step(fit.new - fit)		# Test whether new data set more extreme
    bpvalue <- mean(test)			# Bayesian p-value
    }
    ",fill=TRUE)
sink()

# Bundle Data

data <- list(x = x, y = y, n = n)

# Inits function
inits <- function() list(alpha=rnorm(1), beta=rnorm(1), sigma = rlnorm(1)) 

# Parameters to estimate
params <- c("alpha","beta", "p.decline", "sigma", "fit", "fit.new", "bpvalue", "residual", "predicted")

# Set up Model
jagsModel <- jags.model(file = here("models","linreg.txt"),
                        data = data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100) # Burn-in

Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000) #Draws from posterior
plot(Samples)
summary(Samples)

mcmc <- as.data.frame(do.call(rbind, Samples))

means<-as.data.frame(t(mcmc[,c(7:38)] %>% summarise_all(funs(mean))))
means$var <- c(rep("predicted", 16),rep("residual", 16))
