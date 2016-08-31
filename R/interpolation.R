interpolation<-function(ticker = "GOOG", portefe="ACA.PA AC.PA",nomb="75 25", from = "2013-01-01", to=Sys.time(), n="3"){
if(length(n)==0){
n<-"1";}
n<-as.numeric(n);
if(ticker!="portefeuille"){
mydata <- yahoodata(ticker, from, to);
for(i in 1:dim(mydata)[1]){
mydata$Date<-dateToChart(mydata$Date);
mydata$Date[i]<-(i-1);}
fit <- lm(mydata$Value ~ poly(mydata$Date, n, raw=TRUE));
}
plot(mydata$Date, mydata$Value, type="l", lwd=floor(n/10))+points(mydata$Date, predict(fit), type="l", col="blue", lwd=floor(n/10))}
