interpolation<-function(ticker = "GOOG", portefe="ACA.PA AC.PA",nomb="75 25", from = "2013-01-01", to=Sys.time(), n="3"){
n<-as.numeric(n);
if(ticker!="portefeuille"){
mydata <- yahoodata(ticker, from, to);
for(i in 1:dim(mydata)[1]){
mydata$Date[i]<-(i-1);}
fit <- lm(mydata$Value ~ poly(mydata$Date, n, raw=TRUE));
}
return(fit)}
