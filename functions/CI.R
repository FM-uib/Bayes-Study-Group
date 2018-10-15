CI <- function(Samples, param){
  # Returns proportion of samples of a given param that are larger than 0
  foo <- mean(mean(Samples[[1]][,param]>0),mean(Samples[[2]][,param]>0), mean(Samples[[3]][,param]>0))
  return(foo)
}