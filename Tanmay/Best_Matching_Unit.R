library(profvis)
# Function to return winning neuron
BMU <- function(x, gr) { #x is a single row of data and gr is the grid
  dst <- 0
  min_dist <- 10000000 # Setting high min dist value
  min_ind <- -1 # Setting random min_ind value
  for (e in 1:nrow(gr)) # Iterating through grid
  {
    dst <- ssd(x, gr[e, ]) # SSD distance
    if (dst < min_dist) {
      min_dist <- dst # Updating min distance for winning unit
      min_ind <- e # Updating winning neuron
    }
  }
  return(min_ind)
}
#x is a single row of data and gr is the grid
BMU_Vectorised <- function(x, gr) { 
temp=as.vector(unlist(x))
dist_mtrx=rowSums(sweep(grid,2,temp)^2)
min_ind=which.min(dist_mtrx)
return (min_ind)
}

profvis(
for(i in 1:400)
{
  BMU(data[i,],grid)
}
)
profvis(
  for(i in 1:400)
  {
    BMU_Vectorised(data[i,],grid)
  }
)

