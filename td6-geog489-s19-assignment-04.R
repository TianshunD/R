#Tianshun Deng
#Assignment 4

plotTableFromDisk <- function(dataFile="geneData.csv", outpdf="mypdf.pdf", outcor="mycor.csv")
{
  z1 <- read.csv(dataFile)
  minr <- min(z1)
  maxr <- max(z1)
  pdf(outpdf)
  data1 <- combn(colnames(z1),2)
  for (i in 1:ncol(data1))
  {
    inputx <- unlist(z1[data1[1,i]], use.names = FALSE)
    inputy <- unlist(z1[data1[2,i]], use.names = FALSE)
    plot(inputx,inputy,xlab = data1[1,i],ylab=data1[2,i],xlim=c(minr,maxr),ylim=c(minr,maxr),main =paste(data1[1,i],"vs.",data1[2,i]),pch=2,col="red",cex=1.5)
    lmout <- lm(inputy ~ inputx)      
    abline(lmout,col="blue")
  }
  dev.off()
  
  xcol <- NULL
  ycol <- NULL
  corre <- NULL
  for(i in 1:ncol(data1))       
  {
    xcol[i] <- data1[1,i]
    ycol[i] <- data1[2,i]
    corre[i] <- cor(z1[data1[1,i]],z1[data1[2,i]])   
    final <- as.matrix(xcol)              
    final <- cbind(final,ycol) 
    final <- cbind(final,corre)
    colnames(final) <- c("xcol", "ycol", "correlation")
    write.csv(final, file = outcor, row.names = FALSE)      
  }
}
