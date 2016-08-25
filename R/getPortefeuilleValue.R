getPortefeuilleValue<-function(portefe="AC.PA ACA.PA", portefeang="OML.L", nomb="75 25", from = "2013-01-01", to=Sys.time()){
portefeu<-unlist(strsplit(portefe, " "));
portefeuang<-unlist(strsplit(portefeang, " "));
nomb<-unlist(strsplit(nomb, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
mydata<-yahoodata(portefeu[1], from, to);
mydata$Colse<-0;
n<-length(portefeu);
li<-1000000/n;
myporte<-matrix( nrow=m , ncol=2);
gaini<-numeric(m-1);
cash<-0;


if(length(portefeuang)!=0){
mydatang<-yahoodata(portefeuang[1], from, to);
mydatang$Close<-0;
l<-dim(mydatang)[1];
ni<-1000/length(portefeuang);
for(i in 1:length(portefeuang){
mydat <- yahoodata(portefeuang[i], from, to);
ai<-floor(ni/mydat$Close[l]);
for(j in 1:l){
mydatang$Close[j]<-as.numeric(mydatang$Close[j])+ai*as.numeric(mydat$Close[j]);

}
}
}


  ase1 <- mydatang$Close[1];
  ase2<- mydatang$Close[l];
  gainang<- (ase1-ase2)/ase2;
  gainang<-round(100*gainang,5);
  
  gainf<-numeric(floor((m-1)/20));
for(i in 1:(m-1)){
gaini[i]<- 100*((mydata[i+1,2]-mydata[i,2])/mydata[i,2]);
if((i/20)==floor(i/20)){
gainf[(i/20)]<-gaini[i];
}}
moyenneredmang<-round(mean(gainf),5);
skewnang<-round(skewness(mydata$Close),5);
kurang<-round(kurtosis(mydata$Close),5);
mydatang$Close<-mydatang$Close/1000;



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
skewn<-round(skewness(mydata$Close),5);
kur<-round(kurtosis(mydata$Close),5);
cash<-round(1000000-mydata$Close[m],5);
mydata$Close<-mydata$Close/1000;

if(length(portefeuang)!=0){
par(mfrow=c(2,1);
qplot(Date, Close, data=mydatang, geom = c("line", "smooth"), xlab=paste("Gain du Capital:",gainang,"%","; moyenne arithmétique mensuelle du rendement:", moyenneredmang,"%","; skewness:", skewnang,"; kurtosis:", kurang))
qplot(Date, Close, data=mydata, geom = c("line", "smooth"), xlab=paste("Gain du Capital:",gain,"%","; moyenne arithmétique mensuelle du rendement:", moyenneredm,"%","; skewness:", skewn,"; kurtosis:", kur,"cash:",cash))
}
else{qplot(Date, Close, data=mydata, geom = c("line", "smooth"), xlab=paste("Gain du Capital:",gain,"%","; moyenne arithmétique mensuelle du rendement:", moyenneredm,"%","; skewness:", skewn,"; kurtosis:", kur,"cash:",cash))}
}
