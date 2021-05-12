source("combination.R")
#Initialise Centroids randomly
intls_cntrds=function(x)
{
  centroids=matrix(rnorm(x*3),ncol=3)
  return(centroids)
}
#Euclidean Distance as the distance function
dstnc_fn <- function(x, y) {
  ret <- sqrt(sum((x - y)^2))
  return(ret)
}
#Function to find the closest centroid
clst_cntrd=function(x,centroids)
{
  min_ind=-1
  min_dist=1000000
  for(i in 1:nrow(centroids))
  {
    sprtn=dstnc_fn(x,centroids[i,])
    if(sprtn<min_dist)
    {
      min_ind=i
      min_dist=sprtn
    }
  }
  return(min_ind)
}
#Function to update the centroids
updt_cntrds=function(grps,input,centroids)
{
  new_cntrds=matrix(data=0,nrow=nrow(centroids),ncol=ncol(centroids))
  nmbr=rep(0,nrow(centroids))
  for(i in 1:nrow(input))
  {
    new_cntrds[grps[i],]=new_cntrds[grps[i],]+input[i,]
    nmbr[grps[i]]=nmbr[grps[i]]+1
  }
  for(j in 1:nrow(new_cntrds))
  {
    new_cntrds[j,]=new_cntrds[j,]/nmbr[j]
  }
  return(new_cntrds)
}
#Initialising original centroids
orig_cntrds=intls_cntrds(3)
#Kmeans from scratch function that combines all the previous functions and gives the location of the cluster centroids along with the groups
kmeans_from_scratch=function(inp,count)
{
  n_iter=100 #Number of iterations
  groups=c() #Groups (here there are 3)
  for(iterations in 1:n_iter)
  {
    for(i in 1:nrow(inp))
    {
      groups[i]=clst_cntrd(inp[i,],orig_cntrds) #Finding the closest cluster centroid to that point and assigning that index to groups
    }
    new_cntrds=updt_cntrds(groups,inp,orig_cntrds) #Updating centroids
    if(isTRUE(all.equal(new_cntrds,orig_cntrds))) #If there is no change stop loop 
    {
      break
    }
      
    else
    {
      orig_cntrds=new_cntrds #Else update the original centroids and repeat
    }
  }
  return(list(new_cntrds,groups))
}
clstr=kmeans_from_scratch(gridSOM,3) #Getting the cluster centroids for the grid
clstr
