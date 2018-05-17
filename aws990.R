getStuff<-function( l ){
  results<-drill( l )
  results<-toJSON(results[['ReturnData']][['IRS990']])
  results<-as.data.table(t(as.data.table(fromJSON(results,simplifyDataFrame=T))))
  return(results)
}

drill <- function( l ){
  results<-list()
  if(is.list(l) & length(l)>0){
    result<-filter(l)
  }
  if(length(result) > 0){
    results<-c(results,result)
    return(results)
  }
  lapply(l, function(x) if(is.list(x)) drill(x)) 
}

filter<-function( l ){
  if(any(names(l) %like% "Form990PartVIISectionA")){
    matches<-l[[names(l) %like% "Form990PartVIISectionA"]]
  }
  if(length(matches)>0){
    return(matches)
  }
}



