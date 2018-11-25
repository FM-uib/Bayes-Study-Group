library(rjags)
library(here)
library(dplyr)

# Exercise
source(here("functions","data_gen.R")) # load Dataset generator
source(here("functions","CI.R")) # load CI calc

# 1. Adapt code to test difference in variance

file.show(here("models","v.ttest.txt")) # model for testing Variance

data <- falcons.ws(sigma1 = 3, sigma2 = 2.4) # create falcon dataset with unequal variance

dat <- data[c("y1", "y2", "n1", "n2")]

# parameters to monitor
params <- c("mu1","mu2", "delta", "delta.s", "sigma1", "sigma2")

# Inits function
inits <- function() list(mu1 = rnorm(1), mu2 = rnorm(1), sigma1 = rlnorm(1), sigma2 = rlnorm(1))

jagsModel <- jags.model(file = here("models","v.ttest.txt"),
                        data = dat,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 500) # Burn-in

Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 2000) #Draws from posterior
plot(Samples)
summary(Samples)
# posterior overlaps with zero
# not overwhelming evidence that variance is different.
# posterior overlaps with 0

###############################################################################################

# 2. ttest on data with same mean but different variance

data <- falcons.ws(mu1 = 90, mu2 = 90, sigma1 = 5, sigma2 = 2)

boxplot(data$y ~ data$x, col = "grey", xlab = "Male", ylab = "Wingspan (cm)", las = 1)

dat <- data[c("y","x","n")]

# Inits function
inits <- function() list(mu1 = rnorm(1), delta = rnorm(1), sigma = runif(1))

# Parameters to estimate
params <- c("mu1","mu2", "delta", "sigma")

# Set up Model
jagsModel <- jags.model(file = here("models","ttest.txt"),
                        data = dat,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 500) # Burn-in

Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 2000) #Draws from posterior
plot(Samples)
summary(Samples)
# There does not seem to be any sensitivity to heterogeneous variances.
# model assumes variance is the same. data was generated with 2 and 5,
# sigma after sampling is 4.

###############################################################################################

# 3. Fit a t-test to the mean density in arable and grassland sites. assume unequal variances
# test for a difference in variances

hares <- read.table(file = here("data","hares.data.txt"), header = TRUE)

# removes NAs, mean over the different years. alternatively only take last X years.
# to avoid pseudoreplication
hares.ymean <- hares %>%
  filter(!is.na(mean.density)) %>% # filter out NAs
  group_by(site) %>% 
  summarize(mean.density = mean(mean.density), # mean yearly density
            landuse = first(landuse), n = n()) # carry over landuse

arable <- subset(hares.ymean, landuse == "arable")
grass <- subset(hares.ymean, landuse == "grass")

# not grouped by site and not averaged over years
#arable <- subset(hares, landuse == "arable" & !is.na(mean.density))
#grass <- subset(hares, landuse == "grass" & !is.na(mean.density))

dat <- list(y1 = arable$mean.density, 
             y2 = grass$mean.density, 
             n1 = nrow(arable), 
             n2 = nrow(grass))
inits <- function() list(mu1 = rnorm(1), mu2 = rnorm(1), sigma1 = runif(1), sigma2 = runif(1))
  
params <- c("mu1","mu2", "delta", "delta.s", "sigma1", "sigma2")

jagsModel <- jags.model(file = "models/v.ttest.txt",
                        data = dat,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 500) # Burn-in

Samples <- coda.samples(jagsModel, 
                        variable.names = params, 
                        n.iter = 2000) #Draws from posterior
plot(Samples)
summary(Samples)
CI(Samples, "delta.s")
CI(Samples, "delta")

# significant difference in means and variance.
# posterior touches 0, but 99% is over 0 for mean
# and 96% is over 0 for variance

#########################################################################################

# 4. fit a t-test to the mean.density in arable and grassland.
# introduce a log linear regression of variance over elevation

file.show(here("models","vreg.ttest.txt"))

hares.ymean <- hares %>%
  filter(!is.na(mean.density) & !is.na(elevation)) %>% # filter out NAs
  group_by(site) %>% 
  summarize(mean.density = mean(mean.density), # mean yearly density
            landuse = first(landuse), # carry over landuse
            elevation = mean(elevation), n = n()) # mean elevation

arable <- subset(hares.ymean, landuse == "arable")
grass <- subset(hares.ymean, landuse == "grass")

# not grouped by site and not averaged over years
# arable <- subset(hares, landuse == "arable" & !is.na(mean.density) & !is.na(elevation))
# grass <- subset(hares, landuse == "grass" & !is.na(mean.density)  & !is.na(elevation))

dat <- list(y1 = arable$mean.density, 
            y2 = grass$mean.density, 
            n1 = nrow(arable), 
            n2 = nrow(grass),
            e1 = ((arable$elevation - mean(arable$elevation)) / sd(arable$elevation)),
            e2 = ((grass$elevation - mean(grass$elevation)) / sd(grass$elevation)))

inits <-function() list(mu1 = rnorm(1), mu2 = rnorm(1),
              a1 = rnorm(1), a2 = rnorm(1),
              b1 = rnorm(1), b2 = rnorm(1))

params2 <- c("mu1", "mu2", "a1", "b1","a2", "b2", "delta", "delta.s")

jagsModel <- jags.model(file = here("models","vreg.ttest.txt"),
                        data = dat,
                        init = inits,
                        n.chains = 3, 
                        n.adapt = 2000) # Burn-in

Samples <- coda.samples(jagsModel, 
                        variable.names = params2, 
                        n.iter = 6000) #Draws from posterior
plot(Samples)
summary(Samples)

# the mean density in arable plots is 4.71 and 2.98 in grass plots d = 1.72

1-CI(Samples, "b1")
1-CI(Samples, "b2")
# in addition there seems to be a negative relationship between variance and
# elevation. in arable  ~88% of posterior < 0 and in grass only ~65%. There seems to
# be a difference in how variance changes with elevation, much stronger negative 
# relationship in arable than grassland plots. 