getPortefeuilleValue<-function(portefe="AC.PA ACA.PA",nomb="75 25", from = "2013-01-01", to=Sys.time()){
portefeu<-unlist(strsplit(portefe, " "));
nomb<-unlist(strsplit(nomb, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
mydata<-yahoodata(portefeu[1], from, to);
n<-length(portefeu);
li<-1000000/n;
myporte<-matrix( nrow=m , ncol=2);
gaini<-numeric(m-1)

if(length(nomb)==0){
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
mydat<-c(mydat$Date,mydat$Close);
ai<-floor(li/mydat$Close[m]);
for(j in 1:m){
if(mydat$Date[j]==mydata$Date[j]){
mydata$Close[j]<-as.numeric(mydata$Close[j])+ai*as.numeric(mydat$Close[j]);}
else if(as.numeric(format(mydat$Date[j], "%Y"))<as.numeric(format(mydata$Date[j], "%Y"))){
mydat1<-mydat[1:(j-1),];
mydat2<-mydat[(j+1):m,];
mydat<-rbind(mydat1,mydat2);}

else if(as.numeric(format(mydat$Date[j], "%m"))<as.numeric(format(mydata$Date[j], "%m"))){
mydat1<-mydat[1:(j-1),];
mydat2<-mydat[(j+1):m,];
mydat<-rbind(mydat1,mydat2);}

else if(as.numeric(format(mydat$Date[j], "%d"))<as.numeric(format(mydata$Date[j], "%d"))){
mydat1<-mydat[1:(j-1),];
mydat2<-mydat[(j+1):m,];
mydat<-rbind(mydat1,mydat2);}

else if(as.numeric(format(mydat$Date[j], "%Y"))>as.numeric(format(mydata$Date[j], "%Y"))){
mydat1<-mydat[1:j,];
mydat2<-rbind(mydat[j,],mydat[j:m,]);
mydat<-rbind(mydat1,mydat2);}


else if(as.numeric(format(mydat$Date[j], "%m"))>as.numeric(format(mydata$Date[j], "%m"))){
mydat1<-mydat[1:j,];
mydat2<-rbind(mydat[j,],mydat[j:m,]);
mydat<-rbind(mydat1,mydat2);}

else if(as.numeric(format(mydat$Date[j], "%d"))>as.numeric(format(mydata$Date[j], "%d"))){
mydat1<-mydat[1:j,];
mydat2<-rbind(mydat[j,],mydat[j:m,]);
mydat<-rbind(mydat1,mydat2);}

}
}
}
else{
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
ai<-floor(10000*as.numeric(nomb[i])/mydat$Close[m]);
for(j in 1:m){
mydata$Close[j]<-as.numeric(mydata$Close[j])+ai*as.numeric(mydat$Close[j]);
}}}


  ase1 <- mydata$Close[1];
  ase2<- mydata$Close[m];
  gain<- (ase1-ase2)/ase2;
  gain<-round(100*gain,5);
  
  gainf<-numeric(floor((m-1)/20));
for(i in 1:(m-1)){
gaini[i]<- 100*((mydata[i+1,2]-mydata[i,2])/mydata[i,2]);
if((i/20)==floor(i/20)){
gainf[(i/20)]<-gaini[i];
}}
moyenneredm<-round(mean(gainf),5);
moyennegeoredm<-getMoyenneGeometrique(gainf);
moyennegeoredm<-round(moyennegeoredm,5);
skewn<-skewness(mydata$Close);
kur<-kurtosis(mydata$Close);
cash<-1000000-mydata$Close[m];
mydata$Close<-mydata$Close/1000;

qplot(Date, Close, data=mydata, geom = c("line", "smooth"), xlab=paste("Gain du Capital:",gain,"%","; moyenne arithmétique mensuelle du rendement:", moyenneredm,"%","; moyenne géométrique mensuelle du rendement:", moyennegeoredm,"%","; skewness:", skewn,"; kurtosis:", kur,"cash:",cash))
}
