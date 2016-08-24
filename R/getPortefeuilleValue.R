getPortefeuilleValue<-function(portefe="AC.PA ACA.PA",nomb="75 25", from = "2013-01-01", to=Sys.time()){
portefeu<-unlist(strsplit(portefe, " "));
nomb<-unlist(strsplit(nomb, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
mydata<-yahoodata(portefeu[1], from, to);
mydata$Colse<-0;
n<-length(portefeu);
li<-1000000/n;
myporte<-matrix( nrow=m , ncol=2);
gaini<-numeric(m-1)

if(length(nomb)==0){
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
ai<-floor(li/mydat$Close[m]);
for(j in 1:m){
mydata$Close[j]<-as.numeric(mydata$Close[j])+ai*as.numeric(mydat$Close[j]);

}
}
}

else{
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
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
skewn<-skewness(mydata$Close);
kur<-kurtosis(mydata$Close);
cash<-1000000-mydata$Close[1];
mydata$Close<-mydata$Close/1000;

qplot(Date, Close, data=mydata, geom = c("line", "smooth"), xlab=paste("Gain du Capital:",gain,"%","; moyenne arithmétique mensuelle du rendement:", moyenneredm,"%","; skewness:", skewn,"; kurtosis:", kur,"cash:",cash))
}
