library(profvis)
source("Data_Distance(Matrix).R")
source("Decaying(Matrix).R")
source("BMU(Matrix).R")
source("SOM_Fn(Matrix).R")
gridSOM=SOM(data,grid) #Using previously written code to find the Self Organising Map
profvis(
  {
    gridSOM=SOM(data,grid) 
  }
)
