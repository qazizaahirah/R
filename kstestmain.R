
path1 = ''
path2 = ''
df1 <- read.csv(path1, sep = "\t", header = FALSE)
x<-df1$V2
df2 <- read.csv(path2, sep = "\t", header = FALSE)
x2<-df2$V2
pvalues <- ks.test(x, x2)
ecdfx = ecdf(x)
ecdfx2 = ecdf(x2)
jpeg(file = outputPath)
plot(ecdfx, xlim = range(c(x, x2)), col= "red", main = title,xlab="EntropyValues", ylab="Cumulative Probability")
legend(0.7,0.7, legend=c('Bound','Unbound'),lty=c(1,1),col=c("red","blue"))
plot(ecdfx2, add = TRUE, col= 'blue')
dev.off()
#source('/home/qazi/mydata/LabWork/code/ResidueCentrality/pairwise.ks.test.R')
#myans<-pairwise.ks.test(x,x2,paired = TRUE)
#print (pvalues)