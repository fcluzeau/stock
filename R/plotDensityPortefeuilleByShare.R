plotDensityPortefeuilleByShare<-function(portefe="AC.PA ACA.PA", from="2013-01-01", to=Sys.time()){

portefe<-unlist(strsplit(portefe, " "))
gain<-numeric(length(portefe));
for(i in 1:(length(portefe))){
ticke<-portefe[i];
gaini<-getCapitalGain(ticke, from, to);
gain[i]<-round(gaini,5);
}
 kurt<-kurtosis(gain);
    kurt<-round(kurt,5);
    var<-var(gain);
var<-round(var,5);
 moyenne<-mean(gain);
    moyenne<-round(moyenne,5);
skewn<-skewness(gain);
    skewn<-round(skewn,5);

x <- gain;
h<-hist(x, breaks=10, col="red", xlab=paste("Variations sur la période choisie en %; moyenne arithmétique:",moyenne,"%; variance:",var,"; skewness", skewn,"; kurtosis:", kurt),
   main="Histogramme de la Répartition des Variations des Actions du Portefeuille sur la Période")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 
abline(h=0,v=moyenne,col="black");
}




