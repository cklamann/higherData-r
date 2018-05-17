#cleanUp -- may not work?
cleanNumericTable<-function(DT){
  DT<-suppressWarnings(as.data.table(lapply(DT, as.numeric)))
  DT<-namesToLower(DT)
  DT
}

namesToLower<-function(DT){
  setnames(DT,names(DT),tolower(names(DT)))
  DT
}

#make empty data table with standardized fieldnames
initializeDataTable<-function( fieldVec ){
  DT <- data.table()
  for(field in fieldVec){
    DT[[field]]<-numeric()
  }
  DT
}

##field mapping functions

#this just builds a map DT out of vectors of years and field names, useful for renaming
buildFieldList<-function(yearsDT,sourceFieldVec,permFieldVec){
  sourceFields <- list()
  permFields <- list()
  for(n in 1:nrow(yearsDT)){
    sourceFields[[n]]<-sourceFieldVec
    permFields[[n]]<-permFieldVec
  }
  yearsDT[,permFields:=permFields]
  yearsDT[,sourceFields:=sourceFields]
  yearsDT
}

#utils

plus<-function(vec1,vec2){
  result=vector()
  for(n in 1:length(vec1)){
    if(is.na(vec1[n])){
      result[n]<-vec2[n] 
    }
    else if(is.na(vec2[n])){
      result[n]<-vec1[n]
    }
    else result[n]<-vec1[n]+vec2[n]
  }
  result
}

#vectorwise version of plus, much faster!
plus2<-function(vec1,vec2){
  result=vector()
  ifelse(is.na(vec1) & is.na(vec2), 
         result<-NA,
         ifelse(!is.na(vec1) & !is.na(vec2),
                result<-vec1+vec2,
                ifelse(is.na(vec1) & !is.na(vec2),
                       result<-vec1,
                       result<-vec2
                )
         )           
  )           
  result  
}

#locate a field in csv
findField<-function(years,fieldName,dir){
  for(n in years){
    table<-fread(paste0(dir,n,".csv"))
    setnames(table,names(table),tolower(names(table)))
    fieldName<-tolower(fieldName)
    if(fieldName %in% names(table)){
      print(paste0(fieldName," in ",n))
    }
  }
}

ayToFy <- function(yearVec){
  springYear <- as.numeric(substr(yearVec,3,4))
  springYear <- ifelse(springYear < 10, paste0("0",springYear),springYear)
  year <- paste0("20",springYear)
  year
}

fyToAy <-function(yearVec){
  springYear <- as.numeric(substr(yearVec,3,4))
  fallYear <- springYear - 1
  springYear<-ifelse(springYear < 10, paste0("0",springYear),springYear)
  fallYear <- ifelse(fallYear < 10,paste0("0",fallYear),fallYear)
  year<-paste0(fallYear, springYear)
  year
}

fyToAyFull <-function(yearVec,separator = '_'){
  springYear <- as.numeric(substr(yearVec,3,4))
  fallYear <- springYear - 1
  springYear<-ifelse(springYear < 10, paste0("0",springYear),springYear)
  fallYear <- ifelse(fallYear < 10,paste0("0",fallYear),fallYear)
  year<-paste0("20",fallYear,"_","20", springYear)
  year
}