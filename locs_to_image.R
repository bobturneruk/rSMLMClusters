## ---- locs_to_image

locs_to_image<-function(loc_x,loc_y,pixel_size_nm)
{
  
  library(raster)
  
  #find the minimum and maximum x and y coordinates
  xmin<-min(loc_x)
  xmax<-max(loc_x)
  
  ymin<-min(loc_y)
  ymax<-max(loc_y)
  
  #create a blank image of the appropriate dimensions and bin size (termed resolution)
  image<-raster(xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax,resolution=pixel_size_nm)
  
  xy<-data.frame(x_nm=loc_x,y_nm=loc_y)
  
  #build the 2d histogram
  #Explicitly setting the background to 0 means that pixels (bins) containing
  #no localisations will have a 0 rather than an NA. This helps with plotting.
  #Pixel values are the number of localisations in the bin.
  image<-rasterize(xy,image,fun=function(x,...)length(x),background=0)
  
  #Convert the image (2d histogram in table format) to a data frame.
  image_df<-as.data.frame(image, xy=TRUE)
  names(image_df)<-c("x_nm","y_nm","count")
  
  return(image_df)
  
}