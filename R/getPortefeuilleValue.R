getPortefeuilleValue<-function(portefe="AC.PA ACA.PA",nomb="75 25", from = "2013-01-01", to=Sys.time()){
portefeu<-unlist(strsplit(portefe, " "));
nomb<-unlist(strsplit(nomb, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
mydata<-yahoodata(portefeu[1], from, to);
mydata$Close<-0;
n<-length(portefeu);
li<-1000000/n;
myporte<-matrix( nrow=m , ncol=2);
gaini<-numeric(m-1);
l<-1;

if(length(nomb)==0){
for(i in 1:n){
if(is.element(<portefeu>, <grep("[[:alpha:]]*L$", txt)>)){
portefeu2[l]<-portefeu[i];
l<-l+1;
portefeu1<-portefeu[-i];
}}

for(i in 1:length(portefeu1)){
mydat <- yahoodata(portefeu1[i], from, to);
ai<-floor(li/mydat$Close[m]);
for(j in 1:m){
mydata$Close[j]<-as.numeric(mydata$Close[j])+ai*as.numeric(mydat$Close[j]);

}
}
}

else{
for(i in 1:length(portefeu1)){
mydat <- yahoodata(portefeu1[i], from, to);
ai<-floor(10000*as.numeric(nomb[i])/mydat[m,2]);
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
mydata1<-mydata;

mydata<-yahoodata(portefeu2[1], from, to);
mydata$Close<-0;
for(i in 1:length(portefeu2)){
mydat <- yahoodata(portefeu2[i], from, to);
ai<-floor(li/mydat$Close[m]);
for(j in 1:m){
mydata$Close[j]<-as.numeric(mydata$Close[j])+ai*as.numeric(mydat$Close[j]);

}
}
}

else{
for(i in 1:length(portefeu2)){
mydat <- yahoodata(portefeu2[i], from, to);
ai<-floor(10000*as.numeric(nomb[i])/mydat[m,2]);
for(j in 1:m){
mydata$Close[j]<-as.numeric(mydata$Close[j])+ai*as.numeric(mydat$Close[j]);
}}}
mydata$Close<-mydata$Close/1000;
mydata2<-mydata;

par(mfrow = c(1,2))
qplot(Date, Close, data=mydata1, geom = c("line", "smooth"), xlab=paste("Gain du Capital:",gain,"%","; moyenne arithmétique mensuelle du rendement:", moyenneredm,"%","; moyenne géométrique mensuelle du rendement:", moyennegeoredm,"%","; skewness:", skewn,"; kurtosis:", kur,"cash:",cash))
qplot(Date, Close, data=mydata2, geom = c("line", "smooth"))
}
