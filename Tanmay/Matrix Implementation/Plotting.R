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
