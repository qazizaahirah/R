require(ggplot2)
KSTest <- function(path, filenames,title,legends,plotpath){
  path1 <-paste(path, filenames[[1]], sep = "")
  path2 <-paste(path, filenames[[2]], sep = "")
  df <- read.csv(path1, sep = "\t", header = FALSE)
  df2 <-read.csv(path2, sep = "\t", header= FALSE )
  x<-df$V2
  x2<-df2$V2
  testp <- ks.test(x, x2)
  ecdfx = ecdf(x)
  ecdfx2 = ecdf(x2)

  jpeg(file = plotpath)
  plot(ecdfx, xlim = range(c(x, x2)), col= "red", main = title,xlab="X-axis label", ylab="y-axix label")
  legend(1,1, legend=c(legends[1],legends[2]),lty=c(1,1),col=c("red","blue"))
  plot(ecdfx2, add = TRUE, col= 'blue')
  dev.off()
  return(testp)
}
