varianceGain<-function(portefe="AC.PA ACA.PA", from = "2013-01-01", to=Sys.time()){
portefeu<-unlist(strsplit(portefe, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
n<-length(portefeu);
var<-numeric(n);
gaint<-numeric(n);
mydata<-matrix(nrow=n, ncol=3);

for(i in 1:n){
var[i]<-getVariance(portefeu[i], from, to);
mydat<-yahoodata(portefeu[i], from, to);
  ase1 <- mydat$Close[1];
  ase2<- mydat$Close[m];
  gain<- (ase1-ase2)/ase2;
  gain<-round(100*gain,5);
  gaint[i]<-gain;
  mydata[i,1]<-portefeu[i];
  mydata[i,2]<-as.numeric(var[i]);
  mydata[i,3]<-as.numeric(gaint[i]);
}

mydata<-data.frame(mydata);
colnames(mydata)<-c("Name","var","gain");



ggplot(gain, var, data=mydata, aes(colour="green", label=Name))+
  geom_point() +geom_text(aes(label=Name),hjust=2, vjust=2)
  
 }
