interpolation<-function(portefe="ACA.PA AC.PA",nomb="75 25", deg="3", ticker = "GOOG", from = "2013-01-01", to=Sys.time()){
if(length(n)==0){
n<-"1";}
n<-as.numeric(n)

if(ticker!="portefeuille"){
mydata <- yahoodata(ticker, from, to);
mydat<-numeric(dim(mydata)[1]);
mydato<-numeric(dim(mydata)[1]);
for(i in 1:dim(mydata)[1]){
mydat[i]<-(i-1);}
mydato<-mydata$Value;

sample1 <- data.frame(mydat, mydato);
fit <- lm(sample1$mydato ~ poly(sample1$mydat, n, raw=TRUE));
}
points(sample1$mydat, predict(fit), type="l", col="blue", lwd=floor(n/10))}
