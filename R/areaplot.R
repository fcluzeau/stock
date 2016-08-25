#' Areaplot
#' 
#' Creates an area plot of stock data.
#' 
#' @param ticker stock ticker symbol. E.g. "GOOG". 
#' @param from start date. Either string or date object.
#' @param to end date. Either string or date object.
#' @return ggplot object.
#' @export
areaplot <- function(ticker = "GOOG", portefe="ACA.PA AC.PA", from = "2013-01-01", to=Sys.time()){
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
mydata$up<-0;
n<-length(portefeu);
li<-1000000/n;
myporte<-matrix( nrow=m , ncol=2);
gaini<-numeric(m-1);
cash<-0;

if(length(nomb)==0){
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
ai<-floor(li/mydat$Close[m]);
mydat$up <- mydat$Open < mydat$Close;
for(j in 1:m){
mydata$up[j]<-as.numeric(mydata$up[j])+ai*as.numeric(mydat$up[j]);

}
}
}

else{
for(i in 1:n){
mydat <- yahoodata(portefeu[i], from, to);
mydat$up <- mydat$Open < mydat$Close;
ai<-floor(10000*as.numeric(nomb[i])/mydat[m,2]);
for(j in 1:m){
mydata$up[j]<-as.numeric(mydata$up[j])+ai*as.numeric(mydat$up[j]);
}}}
ggplot(data = mydata, ymin=lowpoint, aes(Date, ymin=Low, ymax=High)) + geom_ribbon(color="black", fill="green", alpha=0.5) + ylim(range(mydata$Close));  
}
}
