# Chapter 6

# normal distribution examples

# Specification in BUGs : 
# x ~dnorm(mean,tau)#notetau=1/variance

n <- 100000  # Sample Size
mu <- mean <- 600  # Body mass of pelegrines
sd <- st.dev <- 30  # SD of body mass of pelegrines

sample <- rnorm(n = n, mean = mu, sd = sd)
hist(sample,col="grey")

# uniform distribution

# Specification in BUGs : 
# x ~dunif(lower,upper)

n <- 100000 # Samplesize
a <- lower.limit <- 0
b <- upper.limit <- 10

sample <- runif(n = n, min = a, max = b)
hist(sample, col = "grey")

# bionomial distribution

# Specification in BUGs :
# x ~dbin(p,N) # Note order of parameters!
# x ~dbern(p)

n <- 100000 # Samplesize
N <- 16 # Number of individuals that flip the coin
p <- 0.8 # Probability of being counted (seen), dead or a male
sample <- rbinom(n = n, size = N, prob= p)
hist(sample, col = "grey")

# poisson distribution

# Specification in BUGs : 
# x ~dpois(lambda)

n <- 100000 # Samplesize
lambda <- 5 #Average#individualspersample,density
sample <- rpois(n = n, lambda = lambda)
plot(table(sample), lwd = 3, ylab = "Frequency")

######################################################################

# 6.3 Snake Example

# Dataset
mass <- c(6,8,5,7,9,11)
pop <- factor(c(1,1,2,2,3,3))
region <- factor(c(1,1,1,1,2,2))
hab <- factor(c(1,2,3,1,2,3))
svl <- c(40, 45,39,50,52,57)

# 6.3.1. The model of the mean
lm(mass ~ 1)

model.matrix(mass~1)

# Model of the mean in Jags
model <- "
model {
  for(i in 1:nobs){
  # Likelihood
  mass[i] ~ dnorm(mean, tau)
  }
  # Priors
  mean ~ dunif(0,100)
  tau <- 1 / vari #
  vari <- sd * sd
  sd ~ dunif(0,10)
}"
model.spec <- textConnection(model)

Data <- list(mass = mass, nobs = length(mass))
inits.fn <- list(mean = runif(0,100), sd = runif(0,10))
jagsModel <- jags.model(file= model.spec,
                        data=Data,
                        init = inits.fn,
                        n.chains = 3, 
                        n.adapt = 100)
para.names <- c("mean", "vari", "sd")
Samples <- coda.samples(jagsModel, 
                        variable.names = para.names, 
                        n.iter = 1000)
plot(Samples)
summary(Samples)

# 6.3.2. t-Test
lm(mass ~ region)

model.matrix(~region)

lm(mass~region)

model.matrix(~region-1)

lm(mass~region-1)

# 6.3.3. Simple linear regression
lm(mass ~ svl)

model.matrix(~svl)

lm(mass~svl)

model.matrix(~svl-1)

lm(mass~svl-1)

# 6.3.4. One-way analysis of variance (one-way ANOVA)
lm(mass ~ pop)

model.matrix(~pop)

model.matrix(~pop-1)

lm(mass~pop)				# Effects parameterization (R default)

lm(mass~pop-1)				# Means parameterization


# 6.3.5. Two-way analysis of variance (two-way ANOVA)
lm(mass ~ region + hab)

model.matrix(~region + hab)

lm(mass ~ region * hab)

model.matrix(~region * hab)

lm(mass ~ region * hab-1-region-hab)

model.matrix(~ region * hab-1-region-hab)


# 6.3.6. Analysis of covariance (ANCOVA)
lm(mass ~ pop + svl)			# Additive model
lm(mass ~ pop * svl)			# Interactive model
lm(mass ~ pop + svl + pop:svl) 	# Same, R's way of specifying the interaction term

model.matrix(lm(mass ~ pop + svl))	# Additive model

model.matrix(lm(mass ~ pop * svl))	# Interactive model

model.matrix(lm(mass ~ pop + svl-1))	# Additive model

model.matrix(lm(mass ~ (pop * svl - 1 - svl))) # Interactive model

lm(mass ~ pop + svl)

fm <- lm(mass ~ pop + svl)		# Refit model
plot(svl, mass, col = c(rep("red", 2), rep("blue", 2), rep("green", 2)))
abline(fm$coef[1], fm$coef[4], col = "red")
abline(fm$coef[1]+ fm$coef[2], fm$coef[4], col = "blue")
abline(fm$coef[1]+ fm$coef[3], fm$coef[4], col = "green")

lm(mass ~ pop * svl)

fm <- lm(mass ~ pop * svl)		# Refit model
plot(svl, mass, col = c(rep("red", 2), rep("blue", 2), rep("green", 2)))
abline(fm$coef[1], fm$coef[4], col = "red")
abline(fm$coef[1]+ fm$coef[2], fm$coef[4] + fm$coef[5], col = "blue")
abline(fm$coef[1]+ fm$coef[3], fm$coef[4] + fm$coef[6], col = "green")

# mean parameterization
lm(mass ~ pop + svl-1)

lm(mass ~ pop * svl-1 - svl)