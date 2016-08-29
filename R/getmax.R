getmax<-function(ticker="GOOG",portefe="AC.PA ACA.PA",nomb="75 25", from = "2013-01-01", to=Sys.time()){
b<-numeric(2);
if(ticker!="portefeuille"){
mydata<- yahoodata(ticker, from, to);
m<-dim(mydata)[1];
maxi<-max(mydata$Value, na.rm=T);
gain<-(max-mydata$Value[m])/(mydata$Value[m]);}

else{
portefeu<-unlist(strsplit(portefe, " "));
nomb<-unlist(strsplit(nomb, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
mydata<-yahoodata(portefeu[1], from, to);
mydata$Value<-0;
n<-length(portefeu);
li<-1000/n;


if(length(nomb)==0){
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
ai<-floor(li/mydat$Value[m]);
for(j in 1:m){
mydata$Value[j]<-as.numeric(mydata$Value[j])+ai*as.numeric(mydat$Value[j]);

}
}
}

else{
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
ai<-floor(10*as.numeric(nomb[i])/mydat[m,2]);
for(j in 1:m){
mydata$Value[j]<-as.numeric(mydata$Value[j])+ai*as.numeric(mydat$Value[j]);
}}}
}
maxi<-as.numeric(max(mydata$Value, na.rm=T));
gain<-(as.numeric(maxi)-as.numeric(mydata$Value[m]))/(as.numeric(mydata$Value[m]));
b[1]<-maxi;
b[2]<-gain;
return(b);}

