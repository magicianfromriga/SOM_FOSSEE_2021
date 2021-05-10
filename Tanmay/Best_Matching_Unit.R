# Function to return winning neuron
BMU <- function(x, gr) {
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