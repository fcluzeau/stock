varianceGain<-function(portefe="AC.PA ACA.PA", from = "2013-01-01", to=Sys.time()){
portefeu<-unlist(strsplit(portefe, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
n<-length(portefeu);
var<-numeric(n);
gaint<-numeric(n);
mydata<-matrix(nrow=n, ncol=3);

for(i in 1:n){
var[i]<-round(getVariance(portefeu[i], from, to),2);
mydat<-yahoodata(portefeu[i], from, to);
  ase1 <- mydat$Close[1];
  ase2<- mydat$Close[m];
  gain<- (ase1-ase2)/ase2;
  gain<-round(100*gain,2);
  gaint[i]<-gain;
  mydata[i,1]<-portefeu[i];
  mydata[i,2]<-as.numeric(var[i]);
  mydata[i,3]<-as.numeric(gaint[i]);
}

mydata<-data.frame(mydata);
colnames(mydata)<-c("Name","var","gain");
head(mydata);

p <- ggplot(data=mydata, aes(x=gain, y=var))+coord_cartesian(ylim=c(0, 20))+geom_point(aes(x, y, size = 0))
}
 
