#This is the R Script for loading the data and creating the grid.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1) Reading the data and scaling it

data <- read.csv("binary.csv", header = T)
X <- scale(data[, -1])
data <- X

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2) A function to return the sum of squared distance between x and y.

ssd <- function(x, y) {
  ret <- sum((x - y)^2)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3) Function to create a SOM grid.
# x is number of rows, y is number of columns.
# p is the number of features in the original dataframe.
# data table is used for faster computation.

crt_gr <- function(x,y,p) {
  ret <- matrix(data = rnorm(x*y* p), nrow = x*y, ncol = p)
  return(ret)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#Creating a 5*5 grid using the function defined above.
set.seed(222)
grid <- crt_gr(5,5,3)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
