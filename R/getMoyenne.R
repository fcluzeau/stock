getMoyenne <- function(ticker="GOOG",portefe="ACA.PA AC.PA", nomb="75 25", from = "2013-01-01", to=Sys.time()){

     mydata <- yahoodata(ticker, from, to);
    names(mydata) <- c("Symbol","Value","Date","Time","Name");
    moyenne<-mean(mydata$Value);
    moyenne<-round(moyenne,5);
    return(moyenne);
}


