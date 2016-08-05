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
plotwrapper <- function(type=c("smoothplot", "highlowplot", "areaplot", "plotDensity","getPlotCapitalGain", "densityGain","plotDensityPortefeuilleByShare", "getPortefeuilleValue"), portefeuille=("AC.PA","ACA.PA"), ticker="GOOG", from="2013-01-01", to=Sys.time(), current=FALSE, moyenne=FALSE, variance=FALSE, skewness=FALSE, kurtosis=FALSE){
	type <- match.arg(type);
	myplot <- switch(type,
		smoothplot = smoothplot(ticker, from, to),
		highlowplot = highlowplot(ticker, from, to),
		areaplot = areaplot(ticker, from, to),
		plotDensity = plotDensity(ticker, from, to),
		getPlotCapitalGain = getPlotCapitalGain(ticker, from, to),
		densityGain = densityGain(ticker, from, to),
		plotDensityPortefeuilleByShare = plotDensityPortefeuilleByShare( from,to),
		getPortefeuilleValue = getPortefeuilleValue(portefeuille, from, to),
		stop("Unknown plot type:", type)
	);
	if(type=="smoothplot"){myplot}
	else if(type!="plotDensity" & type!="getPlotCapitalGain" & type!="densityGain" & type!="plotDensityPortefeuilleByShare" & type!="getPortefeuilleValue"){
	#remove axis label date
	myplot <- myplot + xlab("Date") + ylab(ticker);}
	else{myplot}
	
	if(isTRUE(current)){
		currentvalue <- getcurrent(ticker)$Value
		myplot <- myplot + geom_hline(yintercept = currentvalue, colour = "red", linetype = 2, size = 0.8);	
		myplot <- myplot + geom_label(x=-Inf, y = currentvalue, size=4, label = paste("Valeur en temps réelle en devise locale:", currentvalue), hjust = -1, vjust = -0.5, color="red");
	}
	
		if(isTRUE(moyenne)){
		capgain <- getCapitalGain(ticker,from,to);
		moyenne <- getMoyenne(ticker, from, to);
		myplot <- myplot + geom_hline(yintercept = moyenne, colour = "blue", linetype = 2, size = 0.8);	
		myplot <- myplot + geom_label(x=-Inf,y = (moyenne/1.05), size=4, label = paste("Moyenne en devise locale:", moyenne, ";   Gain du Capital:", capgain,"%"), hjust = -1, vjust = -0.5, color="blue");
	
		}
		
		if(isTRUE(variance)){
		moyenne <- getMoyenne(ticker, from, to)
		var<-getVariance(ticker, from, to)
		myplot <- myplot + geom_label(x=-Inf,y = (moyenne/1.1), size=4, label = paste("Variance:", var), hjust = -1, vjust = -0.5, color="blue");	
		}
		if(isTRUE(skewness)){
		moyenne <- getMoyenne(ticker, from, to)
		skewn<-getSkewness(ticker, from, to)
		myplot <- myplot + geom_label(x=-Inf, y = (moyenne/1.15),  size=4, label = paste("Skewness:", skewn), hjust = -1, vjust = -0.5, color="blue");
		}
		if(isTRUE(kurtosis)){
		moyenne <- getMoyenne(ticker, from, to)
		kurto<-getKurtosis(ticker, from, to)	
		myplot <- myplot + geom_label(x=-Inf, y = (moyenne/1.2),  size=4, label = paste("Kurtosis:", kurto), hjust = -1, vjust = -0.5, color="blue");	
	}
	
	        
	#make sure to print the plot
	print(myplot);
	
	#no need to return anything
	invisible();
}
