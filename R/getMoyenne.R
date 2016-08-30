getMoyenne <- function(ticker="GOOG",portefe="ACA.PA AC.PA", nomb="75 25", from = "2013-01-01", to=Sys.time()){
     if(ticker!="portefeuille"){
     mydata <- yahoodata(ticker, from, to);
    names(mydata) <- c("Symbol","Value","Date","Time","Name");
    moyenne<-mean(mydata$Value);
    moyenne<-round(moyenne,5);
    return(moyenne);
}

else{
porte<-getPorte(portefe, nomb, from, to)
return(round(mean(porte$Close),5));}
