source("Data_Distance.R")
source("Decaying.R")
source("Best_Matching_Unit.R")
source("SOM_Fn.R")
gridSOM=SOM(data,grid)
gridSOM
library(kohonen)
library(vegan)
library(pmclust)
g <- somgrid(xdim = 4, ydim = 4, topo = "rectangular" )
map <- som(as.matrix(data),
           grid = g, rlen=1000,
           alpha = c(0.05, 0.01),
           radius = 1)
gridSOM
codes=map$codes
z=unlist(codes)
mtrx_cd=matrix(nrow=16,ncol=3)
mtrx_cd[,1]=z[1:16]
mtrx_cd[,2]=z[17:32]
mtrx_cd[,3]=z[33:48]
all.equal(gridSOM,mtrx_cd)
