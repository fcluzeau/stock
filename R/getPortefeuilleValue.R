
getPortefeuilleValue<-function(portefe="AC.PA ACA.PA",nomb="75 25", from = "2013-01-01", to=Sys.time()){
portefeu<-unlist(strsplit(portefe, " "));
nomb<-unlist(strsplit(nomb, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
mydata<-yahoodata(portefeu[1], from, to);
mydata$Colse<-0;
n<-length(portefeu);
li<-1000000/n;
myporte<-matrix( nrow=m , ncol=2);
gaini<-numeric(m-1);
gaina<-numeric(n);
cash<-0;
a<-0;

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

for(i in 1:n){
mydato<-yahoodata(portefeu[i], from, to);
ase1 <- mydato$Close[1];
  ase2<- mydato$Close[m];
   gaina[i]<-(ase1-ase2)/ase2;
}

  ase1 <- mydata$Close[1];
  ase2<- mydata$Close[m];
  gain<- (ase1-ase2)/ase2;
  gain<-round(100*gain,5);
  
  gainf<-numeric(floor((m-1)/20));
for(i in 1:m-1){
if((i/20)==floor(i/20)){
gainf[i/20]<- 100*((mydata[i,2]-mydata[i+1,2])/mydata[i+1,2]);

}}
moyenneredm<-round(mean(gainf),5);
skewn<-round(skewness(gaina),5);
kur<-round(kurtosis(gaina),5);
cash<-round(1000000-mydata$Close[m],5);
mydata$Close<-mydata$Close/1000;

for(i in 2:dim(mydata)[1]){
res<-log((mydata$Close[i]/mydata$Close[i-1]), base = exp(1));
res<-res*res;
a<-a+res;
}
a<-a*252/((dim(mydata)[1])-1);
a<-sqrt(a);
a<-(round(a,5));


qplot(Date, Close, data=mydata, geom = c("line", "smooth"), xlab=paste("Gain du Capital:",gain,"%","; moyenne arithmétique mensuelle du rendement:", moyenneredm,"%","; skewness:", skewn,"; kurtosis:", kur,"cash:",cash), ylab=paste("volatilité du portefeuille: ", a))
}
