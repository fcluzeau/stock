volatilite <- function(ticker="GOOG",from = "2013-01-01", to=Sys.time()){
     mydata <- yahoodata(ticker, from, to);
    names(mydata) <- c("Symbol","Value","Date","Time","Name");
    res<-0;
    a<-0;
for(i in 2:dim(mydata)[1]){
res<-log((mydata$Value[i]/mydata$Value[i-1]), base = exp(1));
res<-res*res;
a<-a+res;
}
a<-a*252/((dim(mydata)[1])-1);
a<-sqrt(a);
return(round(a,5));}
