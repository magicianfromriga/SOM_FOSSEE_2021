# Function to create a Self Organizing Map
library(dplyr)
SOM <- function(x, gr) {
  n_iter <- 400 # Defining number of iterations
  intl_lr <- 0.05 # Defining initial learning rate
  intl_rds <- 1 # Defining initial radius
  tm_cnst <- n_iter / log(intl_rds) # Initializing time constant
  for (i in 1:n_iter) # Looping through for training
  {
    dt <- x[sample(1:nrow(x), size = 1, replace = T), ] # Selecting random input row from given data set
    new_rds <- dcy_rds(intl_rds, i, tm_cnst) # Decaying radius
    new_lr <- max(dcy_lrng_rt(intl_lr, i, n_iter), 0.01) # Decaying learning rate
    index <- BMU(dt, gr) # Finding best matching unit for given input row
    for (j in 1:nrow(gr))
    {
      w <- gr[j, ]
      ltrl_dist <- ssd(c(index / (sqrt(nrow(gr))+1), index %% (sqrt(nrow(gr))+1)), c(j / (sqrt(nrow(gr))+1), j %% (sqrt(nrow(gr))+1))) # Distance between returned BMU and current neuron
      if (ltrl_dist <= new_rds) # If w_dist is less than radius
        {
          inf <- inflnc(ltrl_dist, new_rds) # Influencing other nodes
          new_w <- w + (new_lr * inf * (dt - w)) # Calculating new weights
          for (k in 1:3)
          {
            gr[j, k] <- new_w[1,k,with=FALSE] # Updating new weights
          }
        }
    }
  }
  return(gr)
}
