# 1) Decaying radius function
dcy_rds <- function(rds, cur_iter, tm_cnst) {
  ret <- rds * exp(-cur_iter / tm_cnst)
  return(ret)
}

# 2) Decaying learning rate
dcy_lrng_rt <- function(lr, cur_iter, n_iter) {
  ret <- lr * exp(-cur_iter / n_iter)
  return(ret)
}

# 3) A function to calculate influence over neighboring neurons
inflnc <- function(dstnc, rds) {
  ret <- exp(-(dstnc^2) / (2 * (rds^2)))
  return(ret)
}