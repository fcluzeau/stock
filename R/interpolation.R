interpolation<-function(ticker = "GOOG", portefe="ACA.PA AC.PA",nomb="75 25", from = "2013-01-01", to=Sys.time(), n="3"){
if(length(n)==0){
n<-"1";}
n<-as.numeric(as.character(n))

if(ticker!="portefeuille"){
mydata <- yahoodata(ticker, from, to);
mydat<-numeric(dim(mydata)[1]);
for(i in 1:dim(mydata)[1]){
mydat[i]<-(i-1);}
fit <- lm(mydata$Close ~ poly(mydat, n, raw=TRUE));
}
plot(mydat, mydata$Close, type="l", lwd=floor(n/10))+points(mydat, predict(fit), type="l", col="blue", lwd=floor(n/10))}
