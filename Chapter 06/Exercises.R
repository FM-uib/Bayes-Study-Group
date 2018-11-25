library(rjags)
library(ggplot2)
theme_set(theme_classic())

w# Exercise
# Extend the dataset to remove perfect fit.
data <- data.frame(mass = c(6,8,11, 5,7,11, 6,9,11),
                   pop = factor(c(1,1,1, 2,2,2, 3,3,3)),
                   svl = c(40, 45,53, 39,50,61, 41,52,57))
ggplot(data = data,aes(svl, mass, color = pop))+geom_point(size = 4)

# Main effects model 
# effect parameterization
X <- model.matrix(~ pop + svl, data = data)
# mean parameterization
Xm <- model.matrix(~ pop + svl -1, data = data)

fm <- lm(mass ~ Xm-1, data = data)

plot.df <- data.frame(intercept = fm$coefficients[1:3],
                      slope = rep(fm$coefficients[4], 3),
                      pop = levels(data$pop))
M.eff <- ggplot(data = data,aes(svl, mass, color = pop))+geom_point()+
  geom_abline(data = plot.df , aes(intercept = intercept, slope = slope, color = pop))

# Interaction effects model
# effect parameterization
X <- model.matrix(~ pop * svl, data = data)
# mean parameterization
Xm <- model.matrix(~ pop * svl - 1 - svl, data = data)

fm <- lm(mass ~ Xm-1, data = data)
plot.df <- data.frame(intercept = fm$coefficients[1:3],
                      slope = fm$coefficients[4:6],
                      pop = levels(data$pop))
I.eff <- ggplot(data = data,aes(svl, mass, color = pop))+geom_point(size = 4)+
  geom_abline(data = plot.df , aes(intercept = intercept, slope = slope, color = pop))

#################################################################################
#Solution from Companion website:

# Dataset
mass <- c(6,8,5,7,9,11)
pop <- factor(c(1,1,2,2,3,3))
svl <- c(40,45,39,50,52,57)

fm <- lm(mass ~ pop*svl)			# Fully interactive model which uses up all d.f.
summary(fm)

mass <- c(6,8,5,7,9,11)
pop <- factor(c(1,1,2,2,3,3))
svl <- c(40,45,39,50,52,57)

#  Here is a quick solution to build the custom design matrix:
DM <- model.matrix(~ svl * pop - 1 - svl)	# Build a design matrix to start with
DM[5:6,4] <- DM[5:6,6]				# Write the last two elements in col. 6 into col 4
DM <- DM[,-6]					# Delete column 6

# Fit that custom design matrix (have to subtract intercept to override R's treatment parameterisation):
fm <- lm(mass ~ DM-1)
summary(fm)

plot(svl, mass, col = c(rep("red", 2), rep("blue", 2), rep("green", 2)))
abline(fm$coef[1], fm$coef[4], col = "red")
abline(fm$coef[2], fm$coef[5], col = "blue")
abline(fm$coef[3], fm$coef[4], col = "green")
