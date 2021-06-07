library(profvis)
source("Data_Distance(Matrix).R")
source("Decaying(Matrix).R")
source("BMU(Matrix).R")
source("SOM_Fn(Matrix).R")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Creating a 5*5 grid using the function defined above.
set.seed(222)
grid <- crt_gr(5,5,3)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
gridSOM=SOM(data,grid) #Using previously written code to find the Self Organising Map
profvis(
  {
    gridSOM=SOM(data,grid) 
  }
)
