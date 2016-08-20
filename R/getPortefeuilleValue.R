getPortefeuilleValue<-function(portefe="AC.PA ACA.PA", from = "2013-01-01", to=Sys.time()){
portefeu<-unlist(strsplit(portefe, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
mydata<-yahoodata(portefeu[1], from, to);
n<-length(portefeu);
li<-1000/n;
myporte<-matrix( nrow=m , ncol=2);
gaini<-numeric(m-1)

for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
ai<-li/mydat$Close[1];
for(j in 1:m){
mydata$Close[j]<-as.numeric(mydata$Close[j])+ai*as.numeric(mydat$Close[j]);

}
}



  ase1 <- mydata$Close[1];
  ase2<- mydata$Close[m];
  gain<- (ase2-ase1)/ase1;
  gain<-round(100*gain,5);
  
  gainf<-numeric(floor((m-1)/20));
for(i in 1:(m-1)){
gaini[i]<- 100*((mydata[i,2]-mydata[i+1,2])/mydata[i,2]);
if((i/20)==floor(i/20)){
gainf[(i/20)]<-gaini[i];
}}
moyenneredm<-round(mean(100*gainf),5);
moyennegeoredm<-100*getMoyenneGeometrique(gainf);
moyennegeoredm<-round(moyennegeoredm,5);


qplot(Date, Close, data=mydata, geom = c("line", "smooth"), xlab=paste("Gain du Capital:",gain,"%","; moyenne arithmétique mensuelle du rendement:", moyenneredm,"%","; moyenne géométrique mensuelle du rendement:", moyennegeoredm,"%"))
}
