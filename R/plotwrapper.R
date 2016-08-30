#' Plot Wrapper
#' 
#' This is a wrapper for the OpenCPU application. It is a single function that calls out to various plot types.
#' The function prints a plot to the graphics device and returns nothing.
#' 
#' @param type type of plot to create. One of "smoothplot", "highlowplot", "areaplot".
#' @param ticker stock ticker symbol. E.g. "GOOG".
#' @param from start date. Either string or date object.
#' @param to end date. Either string or date object.
#' @param current include the current price of this stock. TRUE/FALSE.
#' @import ggplot2
#' @export
plotwrapper <- function(type=c("smoothplot", "highlowplot", "areaplot", "plotDensity","getPlotCapitalGain", "densityGain","plotDensityPortefeuilleByShare", "getPortefeuilleValue", "varianceGain"), ticker="GOOG", portefe="AC.PA ACA.PA", nomb="75 25", from="2013-01-01", to=Sys.time(), current=FALSE, moyenne=FALSE, variance=FALSE, max=FALSE, min=FALSE){
	type <- match.arg(type);
	myplot <- switch(type,
		smoothplot = smoothplot(portefe, nomb, ticker, from, to),
		highlowplot = highlowplot(ticker, from, to),
		areaplot = areaplot(ticker, portefe, nomb, from, to),
		plotDensity = plotDensity(ticker, from, to),
		getPlotCapitalGain = getPlotCapitalGain(ticker, from, to),
		densityGain = densityGain(ticker, from, to),
		plotDensityPortefeuilleByShare = plotDensityPortefeuilleByShare(portefe, from,to),
		getPortefeuilleValue = getPortefeuilleValue(portefe, nomb, from, to),
		varianceGain = varianceGain(portefe, from, to),
		stop("Unknown plot type:", type)
	);
	if(type=="smoothplot"){myplot}
	else if(type!="plotDensity" & type!="getPlotCapitalGain" & type!="densityGain" & type!="plotDensityPortefeuilleByShare" & type!="getPortefeuilleValue" &type!="varianceGain"){
	#remove axis label date
	myplot <- myplot + xlab("Date") + ylab(ticker);}
	else{myplot}
	
	if(isTRUE(current)){
		currentvalue <- getcurrent(ticker)$Value
		myplot <- myplot + geom_hline(yintercept = currentvalue, colour = "red", linetype = 2, size = 0.8);	
		myplot <- myplot + geom_label(x=-Inf, y = currentvalue, size=4, label = paste("Valeur en temps réelle en devise locale:", currentvalue), hjust = -1, vjust = -0.5, color="red");
	}
	
		if(isTRUE(moyenne)){
		
		moyenne <- getMoyenne(ticker, portefe, nomb, from, to);
		myplot <- myplot + geom_hline(yintercept = moyenne, colour = "blue", linetype = 2, size = 0.8);	
		myplot <- myplot + geom_label(x=-Inf,y = (moyenne/1.05), size=4, label = paste("Moyenne en devise locale:", moyenne), hjust = -1, vjust = -0.5, color="blue");
	
		}
		
		if(isTRUE(variance)){
		moyenne <- getMoyenne(ticker, portefe, nomb, from, to)
		var<-getVariance(ticker, from, to)
		myplot <- myplot + geom_label(x=-Inf,y = (moyenne/1.1), size=4, label = paste("Variance:", var), hjust = -1, vjust = -0.5, color="blue");	
		}
		
		if(isTRUE(max)){
		maxi <- round(getmax(ticker,portefe,nomb, from, to)[1],5);
		gain <- round(getmax(ticker,portefe,nomb, from, to)[2],5);
		myplot <- myplot + geom_hline(yintercept = maxi, colour = "green", linetype = 2, size = 1.2);	
		myplot <- myplot + geom_label(x=-Inf, y = maxi, size=5, label = paste("Valeur maximale:", maxi,"; Gain Maximal:", gain ,"%" ), hjust = -1, vjust = -0.5, color="green");
	}
	
		if(isTRUE(min)){
		mini <- round(getmin(ticker,portefe,nomb, from, to)[1],5);
		perte <- round(getmin(ticker,portefe,nomb, from, to)[2],5);
		myplot <- myplot + geom_hline(yintercept = mini, colour = "red", linetype = 2, size = 1.2);	
		myplot <- myplot + geom_label(x=-Inf, y = mini, size=5, label = paste("Valeur minimale:", mini, "; Perte maximale:",perte,"%"), hjust = -1, vjust = 1.5, color="red");
	}
	
	        
	#make sure to print the plot
	print(myplot);
	
	#no need to return anything
	invisible();
}
