#This is the R Script for loading the data and creating the grid.


library(ggplot2)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1) Reading the data and scaling it

data <- read.csv("binary.csv", header = T)
X <- scale(data[, -1])
data <- X

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2) A function to return the sum of squared distance between x and y.

euclidean_distance <- function(x, y) {
  ret <- sqrt(sum((x - y)^2))
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3) Function to create a SOM grid.
# n is the number of neurons.
# p is the number of columns in the original dataframe.
# data table is used for faster computation.

create_grid <- function(x,y,p) {
  ret <- matrix(data = rnorm(x * y * p), nrow = x * y, ncol = p)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Creating a 4*4 grid using the function defined above.
set.seed(222)
grid <- create_grid(4,4,3)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------


#This is the R Script that contains functions to decay radius, learning rate and calculate influence.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1) Function to decay the radius exponentially over time. 
# rds is the initial radius that is passed.
# cur_iter represents the current iteration.
# time_constant is the time constant that is calculated before the 

decay_radius_function <- function(radius, current_iteration, time_constant) {
  ret <- radius * exp(-current_iteration / time_constant)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2) Function to decay the learning rate.
# lr is the current learning rate.
# cur_iter is the current iteration
# n_iteration is the number of iterations.

decay_learning_rate <- function(learning_rate, current_iteration, n_iteration) {
  ret <- learning_rate * exp(-current_iteration / n_iteration)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3) A function to calculate influence over neighboring neurons
#dstnc is the lateral distance.
#rds is the current neighbourhood radius.

influence_calculation <- function(distance, radius) {
  ret <- exp(-(distance^2) / (2 * (radius^2)))
  return(ret)
}


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#R Script to find the winning neuron / Best Matching unit of a SOM grid

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Simple BMU Implementation - Function to return the winning neuron.
# x is a single row of data and input_grid is the grid

BMU <- function(x, input_grid) { 
  distance <- 0
  min_distance <- 10000000 # Setting high min dist value
  min_ind <- -1 # Setting random min_ind value
  for (e in 1:nrow(input_grid)) # Iterating through grid
  {
    distance <- euclidean_distance(x, input_grid[e, ]) # euclidean_distance distance
    if (distance < min_distance) {
      min_distance <- distance # Updating min distance for winning unit
      min_ind <- e # Updating winning neuron
    }
  }
  return(min_ind-1) #returns index of BMU
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Fastest BMU Implementation using vectorisation.
#x is a single row of data and input_grid is the grid

BMU_Vectorised <- function(x, input_grid) { 
  dist_mtrx=rowSums(sweep(input_grid,2,x)^2) #Calculating the distance of this row from all the neurons using matrix operations.
  min_ind=which.min(dist_mtrx) #Finding the location of the neuron with the minimum distance.
  return (min_ind-1) #Returning the zero-indexed value of the winning neuron.
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#R Script to create a Self Organising Map.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Importing the Library

library(dplyr)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Fastest implmentation of a self organising map.
#x is the input and input_grid is the SOM grid that will be updated iteratively.

SOM <- function(x, input_grid) {
  breaker <- 0
  n_iteration <- nrow(x) # Defining number of iterations
  initial_learning_rate <- 0.05 # Defining initial learning rate
  initial_radius <- 3 # Defining initial radius
  time_constant <- n_iteration / log(initial_radius) # Initializing time constant
  lateral_distance_points=expand.grid(1:sqrt(nrow(input_grid)),1:sqrt(nrow(input_grid)))#Initialising physical locations of neurons to figure out lateral distance.
  rows=sqrt(nrow(input_grid)) #The square grid is used here - so taking the number of rows as square root of number of entries in the grid.
  n_epochs=40 #Defining the number of epochs.
  new_radius <- initial_radius
  l <- c()
  for(ne in 1:n_epochs)
  {
    extra <- ((ne-1)*400)
    for (i in 1:n_iteration) # Looping through for training
    {
      old_grid=input_grid
      curr_i <- extra + i
      sample_input_row <- as.vector(unlist(x[sample(1:nrow(x), size = 1, replace = F), ])) # Selecting random input row from given data set
      new_radius <- decay_radius_function(initial_radius, curr_i, time_constant) # Decaying radius
      new_learning_rate <- decay_learning_rate(initial_learning_rate,curr_i, n_iteration) # Decaying learning rate
      index_temp <- BMU_Vectorised(sample_input_row, input_grid) # Finding best matching unit for given input row
      index_new=c((as.integer(index_temp/rows)+1),(index_temp%%rows)+1) #Converting a 1D co-ordinate to a 2D co-ordinate for finding lateral distance on the map.
      lateral_distance=sqrt(abs(rowSums(sweep(lateral_distance_points,2,index_new)^2))) #Finding Euclidean distance between the given best matching units and all units on the map.
      rn=which(lateral_distance<=new_radius) #Finding neurons that are within the radius of the winning unit.
      inf=influence_calculation(lateral_distance[rn],new_radius)#Calculating the influence of the winning neuron on neighbours.
      if(length(rn)!=1) #Updating multiple rows if neighbourhood is large
      {
        #Calculating the influence of the winning neuron on neighbours.
        diff_grid=(sweep(input_grid[rn,],2,sample_input_row))*-1 #A temporary matrix that stores the difference between the data point and the weights of the winning neuron & neighbours.
        updated_weights=new_learning_rate*inf*diff_grid #The updating operation on the winning and neighbouring neurons.
        input_grid[rn,]=input_grid[rn,]+updated_weights #Now updating those grid entries that are either the winning neuron or its neighbours.
      }
      else #Updating only winning neuron.
      {
        diff_row=(input_grid[rn,]-sample_input_row)*-1 #A temporary matrix that stores the difference between the data point and the weights of the winning neuron & neighbours.
        updated_weights=new_learning_rate*inf*diff_row #The updating operation on the winning and neighbouring neurons.
        input_grid[rn,]=input_grid[rn,]+updated_weights #Now updating those grid entries that are either the winning neuron or its neighbours.
      }
      l <- c(l,euclidean_distance(old_grid,input_grid))
      if(isTRUE(all.equal(old_grid,input_grid)))
      {
        print(curr_i)
        print("Converged")
        breaker <- 1
        break
      }
    }
    if(breaker ==1)
    {
      break
    }
  }
  return(list(input_grid,l)) #Returning the updated SOM weights.
}
y <- SOM(data,grid)

gridSOM <- y[1]
gridSOM
l <- y[2]

t=1:length(l[[1]])
plot(t,l[[1]])
l=unlist(l)
l[which.min(unlist(l))]
#rm(list=ls())

drawGrid<- function(weight,dimension){
  
  # Converting to a matrix
  weight<-as.matrix(weight, ncol = ncol(weight))
  
  norm.matrix<-NULL
  
  # Calculation of the norm
  for(i in 1:length(weight[,1])){
    a<-norm(weight[i,], type = "2")
    norm.matrix<-rbind(norm.matrix,a)
  }
  
  ## Mapping to range 400 to 700
  input_start<-min(norm.matrix)
  input_end<-max(norm.matrix)
  output_start<-400
  output_end<-700
  
  
  ## Calculating wavelength based on norm
  color<-NULL
  for(i in 1:length(norm.matrix)){
    input = norm.matrix[i]
    output = output_start + ((output_end - output_start) / (input_end - input_start)) * (input - input_start)
    color<-rbind(color,output)
  }
  
  # Getting the colors (hex values) from the wavelength
  color.rgb<-w_length2rgb(color)
  
  
  # Plotting the grid
  dim<-max(dimension)+1
  plot(1:dim, type = "n")
  
  for (i in 1:dimension[1]) {
    for(j in 1:dimension[2]){
      #draw.circle(i*2,j*6, radius =.5, col = color.rgb[i*dimension[1]+j - dimension[1]])
      rect(i,j,i+1,j+1, col = color.rgb[i*dimension[1]+j - dimension[1]])
    }
  }
} 
library(photobiology)
gridSOM=matrix(unlist(gridSOM),ncol=3)
drawGrid(gridSOM,c(4,4))
