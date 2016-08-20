getPortefeuilleValue<-function(portefe="AC.PA ACA.PA", from = "2013-01-01", to=Sys.time()){
portefeu<-unlist(strsplit(portefe, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
n<-length(portefeu);
myporte<-matrix( nrow=m , ncol=2);
gaini<-numeric(m-1)

for(i in 1:n){
mydata <- yahoodata(portefeu[i], from, to);
for(j in 1:m){
myporte[j,2]<-myporte[j,2]+as.numeric(mydata$Close[j]);

}
}

myporte[,1]<-mydata$Date;
colnames(myporte)<-c("Date","Close");
myporte<-as.data.frame(myporte);
myporte[is.na(myporte)] <- 0;
  ase1 <- myporte[1,2];
  ase2<- myporte[m,2];
  gain<- (ase1-ase2)/ase2;
  gain<-round(100*gain,5);
  
  gainf<-numeric(floor((m-1)/20));
for(i in 1:(m-1)){
gaini[i]<- 100*((myporte[i,2]-myporte[i+1,2])/myporte[i,2]);
if((i/20)==floor(i/20)){
gainf[(i/20)]<-gaini[i];
}}
moyenneredm<-round(mean(100*gainf),5);
moyennegeoredm<-100*getMoyenneGeometrique(gainf);
moyennegeoredm<-round(moyennegeoredm,5);


qplot(myporte$Date, myporte$Close, geom = c("line", "smooth"), xlab=paste("Gain du Capital:",gain,"%","; moyenne arithmétique mensuelle du rendement:", moyenneredm,"%","; moyenne géométrique mensuelle du rendement:", moyennegeoredm,"%"))
}
