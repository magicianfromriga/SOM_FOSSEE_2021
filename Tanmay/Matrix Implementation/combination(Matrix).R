library(profvis)
library(photobiology)
#library(rgdal)
#library(sf)
#library(dplyr)
source("Data_Distance(Matrix).R")
source("Decaying(Matrix).R")
source("BMU(Matrix).R")
source("SOM_Fn(Matrix).R")
source("Plotting.R")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Creating a 5*5 grid using the function defined above.

# Binary.csv college admission data

  #data <- read.csv("binary.csv", header = T)
  #X <- scale(data[, -1])

# iris data

data<-scale(iris[,-5])
X <- scale(data[, ])
data <- X


set.seed(222)
grid <- crt_gr(5,5,4)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
gridSOM=SOM(data,grid) 

drawGrid(gridSOM,c(5,5)) # weights,dimensions




##-----------------------
#Using previously written code to find the Self Organising Map
profvis(
  {
    gridSOM=SOM(data,grid) 
  }
)


## ############################

# Covid related mapping work


# covid3<-cbind(runif(200, 5000,10000),
#               runif(200, 4000, 5000),
#               runif(200, 8000, 10000))
# 
# covid2<-cbind(runif(200, 2000,5000),
#               runif(200, 2000, 3000),
#               runif(200, 5000, 6000))
# 
# covid1<-cbind(runif(192, 1000, 3000),
#               runif(192, 2000, 2500),
#               runif(192, 1000, 1500))
# 
# covid<-rbind(covid1,covid2, covid3)
# 
# 
# data<- covid


#grid <- crt_gr(40,40,3)
#drawGrid(gridSOM,c(40,40))

# india.district2<-st_read("stanford-sh819zz8121-geojson.json")
# 
# length(unique(india.district2$laa))
# name<-unique(india.district2$laa)
# 
# non.duplicate.names<-which(duplicated(india.district2$laa)==FALSE)
# 
# 
# india.district2<-cbind(india.district2, covid)
# 
# df<-india.district2[non.duplicate.names,]
# df<-merge(df,as.data.frame(covid))
# 
# 
# plot(df[,c(12,11)])
# 
# plot(st_geometry(df), border = 'blue',axes = TRUE)
# plot(st_geometry(st_centroid(df)), pch = 3, col = 'red', add = TRUE)
# 
# for (i in 60:100) {
#   for(j in 7:47){
#     #draw.circle(i*2,j*6, radius =.5, col = color.rgb[i*dimension[1]+j - dimension[1]])
#     rect(i,j,i+1,j+1)
#   }
# }
