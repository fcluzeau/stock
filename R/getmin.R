getmin<-function(ticker="GOOG",portefe="AC.PA ACA.PA",nomb="75 25", from = "2013-01-01", to=Sys.time()){
b<-numeric(2);
if(ticker!="portefeuille"){
mydata<- yahoodata(ticker, from, to);
m<-dim(mydata)[1];
maxi<-max(mydata$Close, na.rm=T);
gain<-(max-mydata$Close[m])/(mydata$Close[m]);}

else{
portefeu<-unlist(strsplit(portefe, " "));
nomb<-unlist(strsplit(nomb, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
mydata<-yahoodata(portefeu[1], from, to);
mydata$Colse<-0;
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
mini<-min(mydata$Close, na.rm=T);
perte<-(mini-mydata$Close[m])/(mydata$Close[m]);
b[1]<-mini*1000/mydata$Close[m];
b[2]<-100*perte;
return(b);}
