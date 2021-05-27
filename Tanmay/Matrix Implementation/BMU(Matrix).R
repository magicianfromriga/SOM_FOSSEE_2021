#R Script to find the winning neuron / Best Matching unit of a SOM grid

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Slow Implementation - Function to return the winning neuron.
#x is a single row of data and gr is the grid

BMU <- function(x, gr) { 
  dst <- 0 #Variable to store the distance measure between the given row of data and a neuron in the grid.
  min_dist <- 10000000 #Variable to store the minimum distance. 
  min_ind <- -1 # Variable to store the location of the most similar neuron.
  for (e in 1:nrow(gr)) # Iterating through grid
  {
    dst <- ssd(x, gr[e, ]) # Calcluating distance between weights of the current neuron and the data row.
    #Now performing comparision to check if this distance is less than the current minimum. 
    #If this is true then the min_dist and min_ind are updated.
    if (dst < min_dist) {
      min_dist <- dst # Updating min distance for winning unit
      min_ind <- e # Updating winning neuron
    }
  }
  #Returning the winning neuron which is zero indexed.
  return(min_ind-1)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Fastest BMU Implementation using vectorisation.
#x is a single row of data and gr is the grid

BMU_Vectorised <- function(x, gr) { 
  dist_mtrx=rowSums(sweep(grid,2,x)^2) #Calculating the distance of this row from all the neurons using matrix operations.
  min_ind=which.min(dist_mtrx) #Finding the location of the neuron with the minimum distance.
  return (min_ind-1) #Returning the zero-indexed value of the winning neuron.
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
