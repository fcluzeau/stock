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
plotwrapper <- function(type=c("plotDensityPortefeuilleByShare", "getPortefeuilleValue"), ticker="GOOG", portefeuille=c("AC.PA","ACA.PA"), from="2013-01-01", to=Sys.time(), current=FALSE, moyenne=FALSE, variance=FALSE, skewness=FALSE, kurtosis=FALSE){
	type <- match.arg(type);
	myplot <- switch(type,
		
		plotDensityPortefeuilleByShare = plotDensityPortefeuilleByShare( from,to),
		getPortefeuilleValue = getPortefeuilleValue(portefeuille, from, to),
		stop("Unknown plot type:", type)
	);
	#if(type=="smoothplot"){myplot}
	#else if(type!="plotDensity" & type!="getPlotCapitalGain" & type!="densityGain" & type!="plotDensityPortefeuilleByShare" & type!="getPortefeuilleValue"){
	#remove axis label date
	#myplot <- myplot + xlab("Date") + ylab(ticker);}
	#else{
	myplot
	#}
	
	
	
	        
	#make sure to print the plot
	print(myplot);
	
	#no need to return anything
	invisible();
}
