interpolation<-function(portefe="ACA.PA AC.PA",nomb="75 25", deg="3", ticker = "GOOG", from = "2013-01-01", to=Sys.time()){
if(length(deg)==0){
deg<-"1";}
deg<-as.numeric(deg)

if(ticker!="portefeuille"){
mydata <- yahoodata(ticker, from, to);
mydat<-numeric(dim(mydata)[1]);
mydato<-numeric(dim(mydata)[1]);
for(i in 1:dim(mydata)[1]){
mydat[i]<-(i-1);
mydato[i]<-mydata$Close[i];}

sample1 <- data.frame(mydat, mydato);
fit <- lm(sample1$mydato ~ poly(sample1$mydat, deg, raw=TRUE));
}
m<-length(sample1$mydat);
plot(sample1$mydat, sample1$mydato, type="l", lwd=floor(2))
points(sample1$mydat, predict(fit), type="l", col="blue", lwd=floor(2))}
