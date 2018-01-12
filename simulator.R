simulate_uniform<-function(n,xy_image){
  
  #This function returns a set of locations drawn at random from
  #a uniform distribution within specified bounds.
  
  return(data.frame(x=runif(n,min=0,max=xy_image),y=runif(n,min=0,max=xy_image)))
  
}

create_clusters<-function(simulated_data,molecules_per_cluster,cluster_size){
  
  #This function takes a set of locations and forms clusters around them.
  #The clusters are square and a uniform distribution of molecules within each
  #cluster is applied.
  
  simulated_data<-simulated_data[rep(seq_len(nrow(simulated_data)), each=molecules_per_cluster),]
  
  simulated_data$x<-simulated_data$x+runif(nrow(simulated_data),min=-cluster_size/2,max=cluster_size/2)
  simulated_data$y<-simulated_data$y+runif(nrow(simulated_data),min=-cluster_size/2,max=cluster_size/2)
  
  return(simulated_data)
  
}

create_error<-function(simulated_data,precision){
  
  #This function simulates localisation error by adding offests drawn at random from a normal distribution.
  
  simulated_data$x<-simulated_data$x+rnorm(nrow(simulated_data),sd=precision)
  simulated_data$y<-simulated_data$y+rnorm(nrow(simulated_data),sd=precision)
  
  return(simulated_data)
  
}

crop<-function(simulated_data,image_xy){
  
  #This function deletes points outside the square respresenting the boundary of the simulated data.
  
  simulated_data<-simulated_data[simulated_data$x>=0 & 
                                   simulated_data$x<=image_xy & 
                                   simulated_data$y>=0 & 
                                   simulated_data$y<=image_xy,]
  
  return(simulated_data)
  
}