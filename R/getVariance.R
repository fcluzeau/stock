getVariance <- function(ticker="GOOG", portefe="ACA.PA AC.PA", nomb="75 25", from = "2013-01-01", to=Sys.time()){
    if(ticker!="portefeuille"){ mydata <- yahoodata(ticker, from, to);
    names(mydata) <- c("Symbol","Value","Date","Time","Name");
    var<-var(mydata$Value);
    var<-round(var,5);
    return(var);
}
else{
porte<-getPorte(portefe, nomb, from, to)
return(round(var(porte$Close),5));}
