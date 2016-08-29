getmax<-function(ticker="GOOG",portefe="AC.PA ACA.PA",nomb="75 25", from = "2013-01-01", to=Sys.time()){
b<-numeric(2);
if(ticker!="portefeuille"){
mydata<- yahoodata(ticker, from, to);
m<-dim(mydata)[1];}

else{
portefeu<-unlist(strsplit(portefe, " "));
nomb<-unlist(strsplit(nomb, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
mydata<-yahoodata(portefeu[1], from, to);
mydata$Close<-0;
n<-length(portefeu);
li<-1000/n;


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
ai<-floor(10*as.numeric(nomb[i])/mydat[m,2]);
for(j in 1:m){
mydata$Close[j]<-as.numeric(mydata$Close[j])+ai*as.numeric(mydat$Close[j]);
}}}
}
maxi<-as.numeric(max(mydata$Close, na.rm=T));
gain<-(as.numeric(maxi)-as.numeric(mydata$Close[m]))/(as.numeric(mydata$Close[m]));
b[1]<-maxi*1000/mydata$Close[m];
b[2]<-100*gain;
return(b);}

