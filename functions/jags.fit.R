jags.fit <- function(model, data, init, params, burn.in, iter, chains = 3){
  jagsModel <- jags.model(file = model,
                          data = data,
                          init = init,
                          n.chains = chains,
                          n.adapt = burn.in)
  Samples <- coda.samples(jagsModel,
                          variable.names = params,
                          n.iter = iter)
  return(Samples)
}