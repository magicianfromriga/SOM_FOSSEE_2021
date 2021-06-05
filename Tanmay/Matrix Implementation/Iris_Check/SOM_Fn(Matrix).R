#R Script to create a Self Organising Map.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Importing the Library

library(dplyr)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Fastest implmentation of a self organising map.
#x is the input and gr is the SOM grid that will be updated iteratively.

SOM <- function(x, gr) {
  n_iter <- 150 # Defining number of iterations
  intl_lr <- 0.05 # Defining initial learning rate
  intl_rds <- 3 # Defining initial radius
  tm_cnst <- n_iter / log(intl_rds) # Initializing time constant
  ltrl_dst_points=expand.grid(1:sqrt(nrow(gr)),1:sqrt(nrow(gr)))#Initialising physical locations of neurons to figure out lateral distance.
  rows=sqrt(nrow(gr)) #The square grid is used here - so taking the number of rows as square root of number of entries in the grid.
  n_epochs=10 #Defining the number of epochs.
  for(ne in 1:n_epochs)
  {
    print(ne)
    old_grid=gr
    for (i in 1:n_iter) # Looping through for training
    {
      dt <- as.vector(unlist(x[sample(1:nrow(x), size = 1, replace = F), ])) # Selecting random input row from given data set
      new_rds <- dcy_rds(intl_rds, i, tm_cnst) # Decaying radius
      new_lr <- max(dcy_lrng_rt(intl_lr, i, n_iter), 0.01) # Decaying learning rate
      index_temp <- BMU_Vectorised(dt, gr) # Finding best matching unit for given input row
      index_new=c((as.integer(index_temp/rows))+1,(index_temp%%rows)+1) #Converting a 1D co-ordinate to a 2D co-ordinate for finding lateral distance on the map.
      ltrl_dist=sqrt(rowSums(sweep(ltrl_dst_points,2,index_new)^2)) #Finding Euclidean distance between the given best matching units and all units on the map.
      rn=which(ltrl_dist<=new_rds) #Finding neurons that are within the radius of the winning unit.
      inf=inflnc(ltrl_dist[rn],new_rds) #Calculating the influence of the winning neuron on neighbours.
      diff_grid=(sweep(gr[rn,],2,dt))*-1 #A temporary matrix that stores the difference between the data point and the weights of the winning neuron & neighbours.
      updt_weights=new_lr*inf*diff_grid #The updating operation on the winning and neighbouring neurons.
      gr[rn,]=gr[rn,]+updt_weights #Now updating those grid entries that are either the winning neuron or its neighbours.
      if(isTRUE(all.equal(old_grid,gr)))
      {
        print(i)
        print("Converged")
      }
    }
  }
  return(gr) #Returning the updated SOM weights.
}
