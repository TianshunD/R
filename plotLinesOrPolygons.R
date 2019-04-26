#	Tianshun Deng
#	Assignment #6
plotLinesOrPolygons <- function(lines_vector,whichPlot="lines",filename="lines.pdf")
{
  library(sp)                      
  library(rgdal)
  library(maptools)
  llCRS <- CRS("+proj=longlat +ellps=WGS84")
  auck_shore <- MapGen2SL("auckland_mapgen.dat",llCRS)     
  lns <- slot(auck_shore, "lines") 
  table(sapply(lns,function(x) length(slot(x,"Lines")))) 
  islands_auck <- sapply(lns,function(x)
  {
    crds <- slot(slot(x,"Lines")[[1]],"coords")	         
    identical(crds[1,],crds[nrow(crds),])
  })
  
  # Pull out the subset subset out all Lines objects which CAN be polygons
  islands_sl <- auck_shore[islands_auck] 
  # Pull out the subset subset out all Lines objects which CANNOT be polygons
  islands_np <- auck_shore[islands_auck==FALSE]            
  
  if(whichPlot == "lines")
  {
    pdf(filename)
    plot(islands_np,main=c("Lines"),col="red",axes=TRUE)
    dev.off()
    return(islands_np)
  }
  if(whichPlot == "polygons")
  {
    list_of_Lines <- islands_sl@lines                 
    
    # First, convert each individual Line to a Polygon, then to a Polygons, then to a list of Polygons:
    list_of_Polygons <- lapply(list_of_Lines,function(x)       
    {
      # Convert the first (and only) Line in the Lines to a Polygon:
      single_Line <- x@Lines[[1]]@coords
      single_Line_ID <- x@ID
      single_Polygon <- Polygon(coords=single_Line)
      single_Polygons <- Polygons(list(single_Polygon),ID=single_Line_ID)
    })
    islands_sp <- SpatialPolygons(list_of_Polygons, proj4string=llCRS)     
    pdf(filename)
    plot(islands_sp,main=c("Polygons"),col="red",axes=TRUE)
    dev.off()
    return(islands_sp)
  }
}





