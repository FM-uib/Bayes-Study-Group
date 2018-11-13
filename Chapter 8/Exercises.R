---
title: "R Notebook"
output: html_notebook
---
```{r, message=FALSE, warning=FALSE}
library(here)
library(rjags)
library(ggplot2)
theme_set(theme_classic())
```

## 1. Convert the ANOVA to an ANCOVA (analysis of covariance):
Within the fixed-effects ANOVA, add the effect on SVL of a continuous measure of
habitat quality that varies by individual snake (perhaps individuals in
better habitat are larger and more competitive). You may either recreate a
new dataset that contains such an effect or simply create a habitat covariate
(e.g., by drawing random numbers) and add it as a covariate into the
previous analysis.

The following Code creates a new dataset with a covariate habitat cover. I have chosen an additive model with a fairly strong relationship between cover and SVL in addition to the population effects.

```{r}
ngroups <- 5				                  # Number of populations
nsample <- 10			                	  # Number of snakes in each
pop.means <- c(50, 40, 45, 55, 60) 	  # Population mean SVL
sigma <- 3				                    # Residual sd
beta <- 20

n <- ngroups * nsample 			          # Total number of data points
eps <- rnorm(n, 0, sigma)	           	# Residuals 
x <- rep(1:5, rep(nsample, ngroups)) 	# Indicator for population
hab.cover <- runif(n, 0, 1)           # Draw random covers

means <- rep(pop.means, rep(nsample, ngroups))

X <- as.matrix(model.matrix(~ as.factor(x) + hab.cover -1)) # Create design matrix
y <- as.numeric(X %*% as.matrix(c(pop.means, beta)) + eps) 
plot(hab.cover,y, col = x)

```

The next plot shows that the populations still have quite different means

```{r}
boxplot(y~x, col="grey", xlab="Population", ylab="SVL", main="", las = 1)
```

Write the BUGS model. We just added the effect of habitate cover into the likelihood and defined a prior.
I opted to choose a wide uniform prior for beta. 

```{r}
model<-"
model {

# Likelihood
 for (i in 1:50) {
    y[i] ~ dnorm(mean[i], tau) 
    mean[i] <- pop.mean[x[i]] + beta * hab.cover[i]
 }

# Priors
 for (i in 1:5){
    pop.mean[i] ~ dunif(0, 100)
 }
 sigma ~ dunif(0, 100)
 beta ~ dunif(-100,100)

# Derived quantities
 tau <- 1 / ( sigma * sigma)

}
"
model.spec <- textConnection(model)
```

Next up, Bundle data and run mcmc sampler

```{r, include=FALSE}
data <- list("y"= y, "x" = x, "hab.cover" = hab.cover)

inits <- function(){ list(pop.mean = runif(5, 0, 100), sigma = runif(1, 0, 100), beta = runif(1, -100, 100) )}

params <- c("pop.mean", "sigma", "beta")

jagsModel <- jags.model(file= textConnection(model),
                        data=data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100)
Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000)
```

The model gives us five estimates for relationship between cover and SVL, reproducing our input fairly well. We did not add separate relationships between cover and SVL for each population, which would only be a couple of minor changes.

```{r, message=FALSE, warning=FALSE}
mcmc <- do.call(rbind, Samples)
plotdata <- data.frame(hab.cover = hab.cover, 
           y=y, x=as.factor(x), slope = colMeans(mcmc)[1], 
           intercept = rep(colMeans(mcmc)[2:6],rep(10,5)))

ggplot(plotdata)+geom_point(aes(x = hab.cover, y = y, color = x), size = 3)+
  geom_abline(aes(intercept = intercept, slope = slope, color = x), size = 1.5)

```


```{r}
rbind(colMeans(mcmc)[2:6], pop.means)
```

##2. Watch shrinkage happen: 
Population mean estimates under the random- effects ANOVA are shrunk toward the 
grand mean when compared with those under a fixed-effects model. 
First, watch this shrinkage by fitting a fixed-effects ANOVA to the random-effects
data simulated in Section 9.3.1.
Second, discard the data from 8 out of 10 snakes in one population and see
what happens to the estimate of that population mean.

Generate random effects Data

```{r, include=FALSE}
rm(list = ls())
set.seed(101)
npop <- 10				# Number of populations: now choose 10 rather than 5
nsample <- 12				# Number of snakes in each
n <- npop * nsample			# Total number of data points

pop.grand.mean <- 50			# Grand mean SVL
pop.sd <- 5				# sd of population effects about mean
pop.means <- rnorm(n = npop, mean = pop.grand.mean, sd = pop.sd)
sigma <- 3				# Residual sd
eps <- rnorm(n, 0, sigma) 		# Draw residuals

x <- rep(1:npop, rep(nsample, npop))
X <- as.matrix(model.matrix(~ as.factor(x)-1))
y <- as.numeric(X %*% as.matrix(pop.means) + eps)

fixed.model<-"
model {

# Likelihood
 for (i in 1:n) {
    y[i] ~ dnorm(mean[i], tau) 
    mean[i] <- pop.mean[x[i]]
 }

# Priors
 for (i in 1:npop){			# Implicitly define alpha as a vector
    pop.mean[i] ~ dunif(0, 100)
 }
 sigma ~ dunif(0, 100)

# Derived quantities
 tau <- 1 / ( sigma * sigma)
}
"
data <- list("y"= y, "x" = x, "n" = n, "npop" = npop)

inits <- function(){ list(pop.mean = runif(10, 0, 100), sigma = runif(1, 0, 100) )}

params <- c("pop.mean", "sigma")

jagsModel <- jags.model(file = textConnection(fixed.model),
                        data = data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100)
Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000)
rdata.fmodel <- do.call(rbind, Samples)

random.model<-"
model {
# Likelihood
 for (i in 1:n) {
    y[i] ~ dnorm(mean[i], tau.res)
    mean[i] <- pop.mean[x[i]]
 }

# Priors and some derived things
for (i in 1:npop){
    pop.mean[i] ~ dnorm(mu, tau.group) 	# Prior for population means
    effe[i] <- pop.mean[i] - mu 	# Population effects as derived quant’s
 }
 mu ~ dunif(0,100)			# Hyperprior for grand mean svl
 sigma.group ~ dunif(0, 10)		# Hyperprior for sd of population effects
 sigma.res ~ dunif(0, 10)		# Prior for residual sd

# Derived quantities
 tau.group <- 1 / (sigma.group * sigma.group)
 tau.res <- 1 / (sigma.res * sigma.res)
}
"

data <- list("y"=y, "x"=x, "npop"= npop, "n" = n)

# Inits function
inits <- function(){ list(mu = runif(1, 0, 100), sigma.group = runif(1, 0, 10), sigma.res = runif(1, 0, 10) )}

# Params to estimate
params <- c("pop.mean", "mu", "sigma.group", "sigma.res")

jagsModel <- jags.model(file= textConnection(random.model),
                        data=data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100)
Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000)
rdata.rmodel <- do.call(rbind,Samples)
```

```{r}
plotdata <- data.frame(means = c(colMeans(rdata.rmodel)[2:11],colMeans(rdata.fmodel)[1:10]),
                      pop = rep(1:10,2),
                      col = rep(c("random", "fixed"), c(10,10)))
bigdata <- ggplot(plotdata) + geom_point(aes(x = pop, y = means, color = col), size = 3) + geom_hline(yintercept = colMeans(rdata.rmodel)[1])

```

Discard 8 out of 10 datapoints

```{r}
ind <- seq(1, 120, by = 6)
y <- y[ind]
x <- x[ind]
#boxplot(y ~ x, col = "grey", xlab = "Population", ylab = "SVL", main = "", las = 1)
#abline(h = pop.grand.mean)
```

We rerun the analyses with a smaller samplesize.

```{r, include=FALSE}
data <- list("y"= y, "x" = x, "n" = 20, "npop" = npop)

inits <- function(){ list(pop.mean = runif(10, 0, 100), sigma = runif(1, 0, 100) )}

params <- c("pop.mean", "sigma")

jagsModel <- jags.model(file= textConnection(fixed.model),
                        data=data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100)
Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000)
small.rdata.fmodel <- do.call(rbind, Samples)

data <- list("y" = y, "x" = x, "npop" = npop, "n" = 20)

inits <- function(){ list(mu = runif(1, 0, 100), sigma.group = runif(1, 0, 10), sigma.res = runif(1, 0, 10) )}

params <- c("pop.mean", "mu", "sigma.group", "sigma.res")

jagsModel <- jags.model(file= textConnection(random.model),
                        data=data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100)
Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000)
small.rdata.rmodel <- do.call(rbind,Samples)
plotdata2 <- data.frame(means = c(colMeans(small.rdata.rmodel)[2:11],
                                  colMeans(small.rdata.fmodel)[1:10]),
                      pop = rep(1:10,2),
                      col = rep(c("random", "fixed"), c(10,10)))
smalldata <- ggplot(plotdata2) + geom_point(aes(x = pop, y = means, color = col), size = 3) + geom_hline(yintercept = colMeans(small.rdata.rmodel)[1])
```

The population mean estimates have moved towards the grand mean. Overall our estimates are much more noisy, as seen in the grand mean estimate. 

```{r}
bigdata
```
```{r}
smalldata
```

##3. Swiss haredata: 
Compare mean observed population density among all
surveyed sites when treating years as replicates. Do this once assuming that
these populations corresponded to fixed effects; then repeat the analysis
assuming they are random effects.

Load hare dataset.
I decide to treat the regions as sites, purely because the number of sites is 54 and I want to keep it simple for this. The density is averaged over the the 17 years.

```{r}
hares <- read.table(file = here("data","hares.data.txt"), header = TRUE)


hares. <- hares %>%
  filter(!is.na(mean.density))%>% # filter out NAs
  group_by(region, year) %>% 
  summarize(mean.density = mean(mean.density))

ggplot(hares., aes(x = region, y = mean.density)) + geom_boxplot()

```


```{r}
data <- list("y"= hares.$mean.density, 
             "x" = hares.$region, 
             "n" = nrow(hares.), 
             "npop" = length(levels(hares.$region)))

inits <- function(){ list(pop.mean = runif(8, 0, 100), sigma = runif(1, 0, 100) )}

params <- c("pop.mean", "sigma")

jagsModel <- jags.model(file= textConnection(fixed.model),
                        data=data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100)
Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000)
haresdata.fmodel <- do.call(rbind, Samples)

inits <- function(){ list(mu = runif(1, 0, 100), sigma.group = runif(1, 0, 10), sigma.res = runif(1, 0, 10) )}

params <- c("pop.mean", "mu", "sigma.group", "sigma.res")

jagsModel <- jags.model(file= textConnection(random.model),
                        data=data,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 100)
Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 1000)
haresdata.rmodel <- do.call(rbind,Samples)

hares.plot <- data.frame(means = c(colMeans(haresdata.rmodel)[2:9],
                                  colMeans(haresdata.fmodel)[1:8]),
                      region = rep(1:8,2),
                      col = rep(c("random", "fixed"), c(8,8)))
hares.plot.gg <- ggplot(hares.plot) + geom_boxplot(aes(x = region, y = means), size = 3) + geom_hline(yintercept = colMeans(haresdata.rmodel)[1])
```

```{r}
hares.plot.gg

boxplot(hares$mean.density~hares$year, col="grey", xlab="Year", ylab="Mean counts", main="", las = 1)
abline(h = mean(hares$mean.density, na.rm = TRUE))
```

