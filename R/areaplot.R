#' Areaplot
#' 
#' Creates an area plot of stock data.
#' 
#' @param ticker stock ticker symbol. E.g. "GOOG". 
#' @param from start date. Either string or date object.
#' @param to end date. Either string or date object.
#' @return ggplot object.
#' @export
areaplot <- function(ticker = "GOOG", portefe="ACA.PA AC.PA",nomb="75 25", from = "2013-01-01", to=Sys.time()){
  if(ticker!="portefeuille"){
  mydata <- yahoodata(ticker, from, to);
mydata$up <- mydata$Open < mydata$Close;
  ggplot(data = mydata, ymin=lowpoint, aes(Date, ymin=Low, ymax=High)) + geom_ribbon(color="black", fill="green", alpha=0.5) + ylim(range(mydata$Close));  
}

else{
portefeu<-unlist(strsplit(portefe, " "));
nomb<-unlist(strsplit(nomb, " "));
m<-dim(yahoodata(portefeu[1], from, to))[1];
mydata<-yahoodata(portefeu[1], from, to);
n<-length(portefeu);
li<-1000000/n;


if(length(nomb)==0){
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
ai<-floor(li/mydat$Close[m]);
for(j in 1:m){
mydata$Close[j]<-as.numeric(mydata$Close[j])+ai*as.numeric(mydat$Close[j]);
mydata$Open[j]<-as.numeric(mydata$Open[j])+ai*as.numeric(mydatOpen[j]);
}
}
}

else{
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
ai<-floor(10000*as.numeric(nomb[i])/mydat[m,2]);
for(j in 1:m){
mydata$Close[j]<-as.numeric(mydata$Close[j])+ai*as.numeric(mydat$Close[j]);
mydata$Open[j]<-as.numeric(mydata$Open[j])+ai*as.numeric(mydat$Open[j]);
}}}
mydata$up <- mydata$Open < mydata$Close;
ggplot(data = mydata, ymin=lowpoint, aes(Date, ymin=Low, ymax=High)) + geom_ribbon(color="black", fill="green", alpha=0.5) + ylim(range(mydata$Close));  
}
}
