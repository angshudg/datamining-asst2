################################################
####                                        ####
####  THIS CODE DOES NOT CLASSIFY ANYTHING  ####
####                                        ####
################################################

lRange = seq(0.1, 1.0, by=0.1)
cRange = c(seq(-100, -10, by=0.1), seq(10, 100, by=0.1))
dRange = c(seq(-3.0, -1.1, by=0.1), seq(1.1, 3.0, by=0.1))
netmed <- function(data){
  unq<-unique(data$class)
  arr<-array(dim = length(unq))
  for(i in unq){
    arr[which(unq==i)]<-median(data[which(data$class==i),5])
  }
  return(sd(arr))
}

data <- read.csv("~/data.csv")
c1=-10
c2=58
c3=-11.4
d1=-1.1
d2=-2.1
#d3=1.8
l3=0.6
#l2=0.1
tot2 <<- 990
medVar2<<-100000
#c1Range=seq(54,80,by=0.1)

#for(c2 in seq(57,59,by=0.01)){
datamat <- t(as.matrix(data[,1:3]))
X<-rbind(log(datamat[3,]+l3, 3), datamat[1,]^d1, datamat[2,]^d2)
C<-cbind(c1,c2,c3)
data2 <- cbind(data, t(C%*%X))
names(data2)[5] <- "y"
s <- 0:9
tot=0
for(i in s){
  for(j in s[which(s!=i)]){
    mx <- max(data2[which(data2$class==j),5])
    mn <- min(data2[which(data2$class==j),5])
    tot <- tot+sum(mn<data2[which(data2$class==i),5] & data2[which(data2$class==i),5]<mx)
    #print(tot)
    #print(paste(""))
    #print(mn<data2[which(data2$class==i),5] & data2[which(data2$class==i),5]<mx)
    medVar <- netmed(data2)
  }
}
if((tot<=tot2 & medVar<=medVar2)|(tot<tot2)){
  tot2<<-tot
  medVar2<<-medVar
  c1.f<<-c1
  c2.f<<-c2
  c3.f<<-c3
  l3.f<<-l3
  d1.f<<-d1
  d2.f<<-d2
  print(paste("New optima: c1=",c1,"c2=",c2,"c3=",c3,"d1=",d1,"d2=",d2,"l3=",l3,"tot",tot,"medVar",medVar))
}  
#}