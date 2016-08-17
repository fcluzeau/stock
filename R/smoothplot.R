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
smoothplot <- function(portefe="AC.PA ACA.PA", ticker = "GOOG", from = "2013-01-01", to=Sys.time()){
if( ticker!= "portefeuille"){
mydata <- yahoodata(ticker, from, to);
  vol<-volatilite(ticker, from, to);
  qplot(Date, Close, data = mydata, geom = c("line", "smooth"), xlab=paste("Votilité de l'action ",vol),ylab= ticker);  
}

else{
porte<- strsplit(portefe, " ")[[1]];
for(i 1:length(porte)){
portefe[i]<-porte[i];}
getPortefeuilleValue(portefe, from, to);}

}
