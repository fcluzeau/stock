#' Smooth Plot
#' 
#' Creates a smooth plot of stock data.
#' 
#' @param ticker stock ticker symbol. E.g. "GOOG".
#' @param from start date. Either string or date object.
#' @param to end date. Either string or date object.
#' @return ggplot object.
#> geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
#' @export
smoothplot <- function(portefe="AC.PA ACA.PA", nomb="75 25",deg="3", ticker = "GOOG", from = "2013-01-01", to=Sys.time()){
if( ticker!= "portefeuille"){
if(length(deg)==0){deg<-30}
deg<-as.numeric(deg);
mydata <- yahoodata(ticker, from, to);
  vol<-volatilite(ticker, from, to);
  qplot(Date, Close, data = mydata, geom =c("line","smooth"), xlab=paste("Votilité de l'action ",vol),ylab= ticker);  
}

else{
getPortefeuilleValue(portefe, nomb, from, to);}

}
