interpolation<-function(ticker = "GOOG", portefe="ACA.PA AC.PA",nomb="75 25", from = "2013-01-01", to=Sys.time(), deg="3"){
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
