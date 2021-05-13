rm(list = ls())

set.seed(11)
#Matrix of input vector X
r1<-cbind(runif(500),runif(500, 4,6))
r2<-cbind(runif(500),runif(500, 0,2))
r3<-cbind(runif(500, 3,4), runif(500,4,6))
r4<-cbind(runif(500, 3,4), runif(500,0,2))

#rnorm(500, 4,1),rnorm(500,4,1)
# r1<-runif(2000,1,255)
# r2<-runif(2000,1,255)
# r3<-runif(2000,1,255)

data<-matrix(rbind(r1,r2,r3,r4), ncol = 2)

#data<-matrix(cbind(r1,r2,r3), ncol = 3, byrow = T)

weight<-matrix(cbind(runif(49,1,3),runif(49,2,4)), ncol = 2)
#weight<-scale(weight)

#weight<-matrix(cbind(runif(400,1,255),runif(400,1,255), runif(400,1,255)), ncol = 3, byrow = T)

plot(data, xlim=c(0,4),col ="red")
points(weight,col ="blue", pch = 3)
##--------------------------------------------
#Neuron Grid
neuron.grid<-NULL
##The grid is of 10X10
dimension<-c(7,7) # c(rows, column)
##Grid making matrix inside list
for(i in 1:dimension[1]){
  v<-NULL

  for(j in 1:dimension[2]){
    t<-weight[(i*dimension[1]+j-dimension[1]),]
    t<-matrix(c(t,c(i,j)), ncol=2, byrow = T)
    
    if(j==1)
      v<-list(t)
    else
      v<-append(v,list(t))
  }
  
  if(i==1)
    neuron.grid<-list(v)
  else
    neuron.grid<-append(neuron.grid,list(v))
}
##-------------------------------------------
euclidean<-function(x,y){
  sum<-0
  for(i in 1:length(x)){
    sum<- sum + (x[i]-y[i])^2
  }
  return(sqrt(sum))
}
##---------------------------------------------
BMU<-function(s){
  min<-Inf
  for(i in 1:dimension[1]){
    for(j in 1:dimension[2]){
      distance = euclidean(data[s,],neuron.grid[[i]][[j]][1,])
      
      if(distance<min){
        min<-distance 
        winning_neuron<-c(i,j) #(row,column)
      }
    }
  }
  return(winning_neuron)
}
#-----------------------------------------------
# #iterating over weights
# initial_eta = 2
# initial_sig = 1
# total_itr = 5000
# tconst = 1000

# for iterating over neurons and
initial_eta = 1.6
initial_sig = 2
total_itr = 3000
tconst = 1000
#-----------------------------------------------
# training  part

for( time in 1:total_itr){
  
  eta = initial_eta*exp(-time/tconst)
  sig = initial_sig*exp(-time/tconst)
  
  if(eta<0.01)
    eta=0.01
  
  # Random input from continuous input space
  s<-floor(runif(1,1,dim(data)[1]))
  winning_neuron=BMU(s)
  
  for (i in 1:dimension[1]) {
    for(j in 1:dimension[2]){
      lateral_distance <- euclidean(neuron.grid[[winning_neuron[1]]][[winning_neuron[2]]][2,],neuron.grid[[i]][[j]][2,])
      
      if(lateral_distance<=sig){
        influence = exp(-(lateral_distance^2)/2*(sig^2))
        
        for(k in 1:length(weight[1,]))
          neuron.grid[[i]][[j]][1,k]<-neuron.grid[[i]][[j]][1,k]+eta * influence *(data[s,k]-neuron.grid[[i]][[j]][1,k])
      }
    }
  }
}

##--------------------------------------------------------
#result plot
#plot(data, col="red", pch = 21)

# get wights back
w<-NULL
for (i in 1:dimension[1]) {
  for(j in 1:dimension[2]){
    l<- cbind(neuron.grid[[i]][[j]][1,1],neuron.grid[[i]][[j]][1,2])
    w<-rbind(w,l)
  }
}
#3--------------------------------------------------------
#result plot
plot(data, col="red", pch = 1)

points(w[,1],w[,2], pch =16, col="blue")

##-------------------------------------------------------
#converting w to a matirx
w<-matrix(w, ncol = 2)


#calculation norm / euclidean distance of each vector
norm.matrix<-NULL
for(i in 1:length(w[,1])){
  a<-norm(w[i,], type = "2")
  norm.matrix<-rbind(norm.matrix,a)
}
##-------------------------------------------------------
summary(norm.matrix)
input_start<-min(norm.matrix)
input_end<-max(norm.matrix)
output_start<-360
output_end<-720

color<-NULL
for(i in 1:length(norm.matrix)){
  input = norm.matrix[i]
  output = output_start + ((output_end - output_start) / (input_end - input_start)) * (input - input_start)
  color<-rbind(color,output)
}

##-----------------------------------------------------
library(photobiology)
color.rgb<-w_length2rgb(color)
##-------------------------------------------------------

##  !!--DO NOT  USE--!!

# #Neuron map
# plot(0,0, xlim = c(0,12), ylim = c(0,12))
# 
# plotgrid<-function(){
#   for(i in 1:dimension[1]){
#     for (j in 1:dimension[2]) {
#       x<-neuron.grid[[i]][[j]][2,1]
#       y<-neuron.grid[[i]][[j]][2,2]
#       points(x,y,col= "blue", #color.rgb,
#              pch=21)
#       #points(i,j, col =color.rgb, pch =21)
#     }
#   }
#   #plot(x,y, pch = 21)
# }
# plotgrid()

##-------------------------------------------------------

# cluster<-kmeans(weight, 4, nstart = 10)
# 
# print(cluster)

# ##------------------------------------------------------
# library(grid) # plot circles
library(plotrix) # to convert col hex to col names
# color.names<-sapply(color.rgb,color.id)
# 
# ##-----------------------------------------------------
## drawing neurons
plot(1:45,type="n", xlim = c(0,20), ylim = c(0,50))
for (i in 1:dimension[1]) {
  for(j in 1:dimension[2]){
    draw.circle(i*2,j*6, radius =.5, col = color.rgb[i*dimension[1]+j - dimension[1]])
  }
}


# 
# 





#draw.circle(2,4,1)
#draw.circle(5,6,1)
##------------------------------------------------------
#color map
# red<-w[,1]
# blue<-w[,2]
# d<-cbind.data.frame(red, blue) 
# 
# detail<-summary(d)
# 
# map<-function(x, output_start =0,output_end= 255){
#   input_start = min(x)
#   input_end = max(x)
#   
#   r<-apply(x, 1:2, function(input) output_start + ((output_end - output_start) / (input_end - input_start)) * (input - input_start))
#   
#   
#   return(r)
# }
# 
# 
# d<- within(d, mix <- rgb(green=0, red=red, blue=blue, maxColorValue=100))
# 
# ggplot(d, aes(x=red, y=blue)) + 
#   geom_tile(aes(fill=mix), color="white") + 
#   scale_fill_identity()

##---------------------------------------------------------
# #density map
# library(viridis)
# library(ggplot2)
# df<-as.data.frame(w)
# 
# plot(df)
# ggplot(df, aes(`V1`, `V2`, fill = density)) +
#   geom_tile() + scale_fill_continuous(type='viridis')

##------------------------------------------------------
## Test plot
# plot(1:5,seq(1,10,length=5),type="n",xlab="",ylab="",main="Test draw.circle")
# draw.circle(2,4,c(1,0.66,0.33),border="purple",
#             col=c("#ff00ff","#ff77ff","#ffccff"),lty=1,lwd=1)
# draw.circle(2.5,8,0.6,border="red",lty=3,lwd=3)
# draw.circle(4,3,0.7,border="green",col="yellow",lty=1,
#             density=5,angle=30,lwd=10)
# draw.circle(3.5,8,0.8,border="blue",lty=2,lwd=2)
