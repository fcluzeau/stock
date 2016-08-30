getPorte<-function(portefe="AC.PA ACA.PA",nomb="75 25", from = "2013-01-01", to=Sys.time()){
portefeu<-unlist(strsplit(portefe, " "));
nomb<-unlist(strsplit(nomb, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
porte<-yahoodata(portefeu[1], from, to);
porte$Close<-0;
n<-length(portefeu);
li<-1000000/n;


if(length(nomb)==0){
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
ai<-floor(li/mydat$Close[m]);
for(j in 1:m){
porte$Close[j]<-as.numeric(porte$Close[j])+ai*as.numeric(mydat$Close[j]);

}
}
}

else{
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
ai<-floor(10000*as.numeric(nomb[i])/mydat[m,2]);
for(j in 1:m){
porte$Close[j]<-as.numeric(portea$Close[j])+ai*as.numeric(mydat$Close[j]);
}}}

mydata}
