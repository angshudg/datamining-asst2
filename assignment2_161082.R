data <- read.csv("~/testdata_161082.csv")  #enter address of test data here
c1=-10
c2=58
c3=-11.4
d1=-1.1
d2=-2.1
l3=0.6
datamat <- t(as.matrix(data[,1:3]))
X<-rbind(log(datamat[3,]+l3, 3), datamat[1,]^d1, datamat[2,]^d2)
C<-cbind(c1,c2,c3)
data2 <- cbind(data, t(C%*%X))
names(data2)[length(data2)] <- "y"
class <- array(dim = length(data2[,1]))
datatest <- cbind(data2, class)
  ###########BLACKBOX#############################################################
  datatest$class[which(datatest$y < -1120.728363)]=7
  datatest$class[which(-1120.728363 <= datatest$y & datatest$y < -865.7810480)]=3
  datatest$class[which(-865.7810480 <= datatest$y & datatest$y < -608.4167773)]=6
  datatest$class[which(-608.4167773 <= datatest$y & datatest$y < -415.9984890)]=0
  datatest$class[which(-415.9984890 <= datatest$y & datatest$y < -215.9360978)]=4
  datatest$class[which(-215.9360978 <= datatest$y & datatest$y < -7.738935471)]=8
  datatest$class[which(-7.738935471 <= datatest$y & datatest$y <  142.1156540)]=1
  datatest$class[which( 142.1156540 <= datatest$y & datatest$y <  344.4154821)]=9
  datatest$class[which( 344.4154821 <= datatest$y & datatest$y <  520.1873298)]=2
  datatest$class[which( 520.1873298 <= datatest$y)]=5
  ################################################################################
write.table(datatest, "161082.csv", sep = ",")  
View(datatest)
  