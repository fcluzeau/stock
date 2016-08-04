listbyindustry <- function(){
  newtable <- with(stocktable, data.frame(id=paste0("symbol_", Ticker), text=paste(Ticker, "-", Company), nodeType= 'async', leaf=TRUE, Country=Country, Industry=Industry, Sector=Sector));
  mydata <- splitIntoTree(newtable, "Country");
  mydata$children <- lapply(mydata$children, splitIntoTree, f="Industry", out=c("id", "text", "leaf","nodeType"));
  invisible(list("text"=".", children=mydata))
}

splitIntoTree <- function(x, f, out){
  stopifnot(!missing(f))
  mydata <- split(x, x[f], drop=TRUE);
  if(!missing(out)){
    mydata <- lapply(mydata, "[", out)
  }
  structure(list(names(mydata), unname(mydata)), class="data.frame", names=c("text", "children"), row.names=as.character(seq_along(mydata)));
}
