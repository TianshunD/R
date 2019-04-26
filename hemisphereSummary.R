hemisphereSummary <- function(df,projargs="+proj=longlat +datum=WGS84")
{
  #Load the libraries "sp" and "foreach" within the function
  library("sp")
  library("foreach")
  hemi_CRS <- CRS(projargs)
  hemi_SpatialPoints <- SpatialPoints(coords=df,proj4string=hemi_CRS)
  #Create two vectors to accept the spatial points' EWhemisphere and NShemisphere values
  EWhemisphere<-NULL
  NShemisphere<-NULL
  registerDoSEQ()
  foreach(i=1:length(hemi_SpatialPoints), .packages="sp", .combine=c) %do%
  {
    if(hemi_SpatialPoints[i]$xpos>180)
      EWhemisphere[i]<-"W"
    else EWhemisphere[i]<-"E"
    if(hemi_SpatialPoints[i]$ypos<0)
      NShemisphere[i]<-"S"
    else NShemisphere[i]<-"N"
  }
  data<-data.frame(EWhemisphere=EWhemisphere,NShemisphere=NShemisphere) 
  outHemisphere <- SpatialPointsDataFrame(coords=hemi_SpatialPoints,data=data,proj4string=hemi_CRS)
  return(outHemisphere)
}
stopCluster(myCluster)


for(i in 1:length(df[,1]))
{
  if(df[i,1]>180)
    EWhemisphere[i]<-'W'
  else EWhemisphere[i]<-'E'
  if(df[i,2]<0)
    NShemisphere[i]<-'S'
  else NShemisphere[i]<-'N'
}

install.packages("sp")
library("sp")
hemisphereSummary <- function(df,projargs="+proj=longlat +datum=WGS84")
{
  #Load the libraries "sp" and "foreach" within the function
  library("sp")
  library("foreach")
  hemi_CRS <- CRS(projargs)
  hemi_SpatialPoints <- SpatialPoints(coords=df,proj4string=hemi_CRS)
  #Create two vectors to accept the spatial points' EWhemisphere and NShemisphere values
  hemisphere<-as.matrix(df)
  colnames(hemisphere)<-c("EWhemisphere","NShemisphere")
  registerDoSEQ()
  foreach(i=1:length(hemi_SpatialPoints), .packages="sp", .combine="rbind") %do%
  {
    sphere<-NULL
    if(hemi_SpatialPoints[i]$xpos>180)
      sphere[1]<-"W"
    else sphere[1]<-"E"
    if(hemi_SpatialPoints[i]$ypos<0)
      sphere[2]<-"S"
    else sphere[2]<-"N"
    hemisphere[i,]<-sphere
  }
  hemisphere <- as.data.frame(hemisphere) 
  outHemisphere <- SpatialPointsDataFrame(coords=hemi_SpatialPoints,data=hemisphere,proj4string=hemi_CRS)
  return(outHemisphere)
}
