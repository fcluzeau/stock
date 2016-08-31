getVariance <- function(ticker="GOOG", from = "2013-01-01", to=Sys.time()){
    mydata <- yahoodata(ticker, from, to);
    names(mydata) <- c("Symbol","Value","Date","Time","Name");
    var<-var(mydata$Value);
    var<-round(var,5);
    return(var);
}
