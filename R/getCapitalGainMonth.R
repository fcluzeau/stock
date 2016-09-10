
getCapitalGainMonth <- function(ticker = "GOOG", from = "2013-01-01", to=Sys.time()){
mydata <- yahoodata(ticker, from, to);
names(mydata) <- c("Symbol","Value","Date","Time","Name");
num<-dim(mydata)[1];
gain<-matrix(ncol=2,nrow=num-1);
gainf<-matrix(ncol=2,nrow=floor((num-1)/20));
for(i in 1:num-1){
if((i/20)==floor(i/20)){
gainf[i/20,1]<- 100*((mydata[i+1,2]-mydata[i,2])/mydata[i+1,2]);
gainf[i/20,2]<-mydata[i+1,3];

}}
gainf<-as.data.frame(gainf);
colnames(gainf)<-c("Value","Date");
return(gainf);
}
