#This is the R Script that contains functions to decay radius, learning rate and calculate influence.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1) Function to decay the radius exponentially over time. 
# rds is the initial radius that is passed.
# cur_iter represents the current iteration.
# tm_cnst is the time constant that is calculated before the 

dcy_rds <- function(rds, cur_iter, tm_cnst) {
  ret <- rds * exp(-cur_iter / tm_cnst)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2) Function to decay the learning rate.
# lr is the current learning rate.
# cur_iter is the current iteration
# n_iter is the number of iterations.

dcy_lrng_rt <- function(lr, cur_iter, n_iter) {
  ret <- lr * exp(-cur_iter / n_iter)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3) A function to calculate influence over neighboring neurons
#dstnc is the lateral distance.
#rds is the current neighbourhood radius.

inflnc <- function(dstnc, rds) {
  ret <- exp(-(dstnc^2) / (2 * (rds^2)))
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
