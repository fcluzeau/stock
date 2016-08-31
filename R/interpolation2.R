interpolation2<- function(portefe="AC.PA ACA.PA", nomb="75 25",deg="3", ticker = "GOOG", from = "2013-01-01", to=Sys.time()){
if( ticker!= "portefeuille"){
if(length(deg)==0){deg<-30}
deg<-as.numeric(deg);
mydata <- yahoodata(ticker, from, to);
  vol<-volatilite(ticker, from, to);
  qplot(Date, Close, data = mydata, xlab=paste("Votilité de l'action ",vol),ylab= ticker)+geom_smooth(span = deg/100);  
}

else{
getPortefeuilleValue(portefe, nomb, from, to);}

}
