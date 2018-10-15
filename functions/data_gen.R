falcons.ws <- function(n1 = 60, n2 = 40, mu1 = 105, mu2 = 77.5, sigma1 = 2.75, sigma2 = 2.75){
  n <- n1+n2				# Total sample size
  y1 <- rnorm(n1, mu1, sigma1)		# Data for females
  y2 <- rnorm(n2, mu2, sigma2)		# Date for males
  y <- c(y1, y2)				# Aggregate both data sets
  x <- rep(c(0,1), c(n1, n2))		# Indicator for male
  return(list(x = x, y = y, y1 = y1, y2 = y2, n = n, n1 = n1, n2 = n2))
}