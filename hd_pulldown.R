##strategy:
#each time a new one of these comes out, run this script and push

library(dplyr)
library(data.table)
library(stringr)
hdYears <- c(2002:2017)
hdDownloadDir<-"/Users/cklamann/ipeds/hd/"
hdSourceFiles <- data.table(file = c(paste0("HD",hdYears)),fy = hdYears)
hdFilterFields <- c("unitid","instnm","instnm_key","stabbr","sector","hbcu","locale","fiscal_year")
hdReturnFields<- c("unitid","name","state","sector","hbcu","locale","slug")

hdDownloadTables<-function( yearVec = hdYears ){
  download(hdSourceFiles,yearVec,hdDownloadDir)
}

hdFilterData<-function(years = hdYears){ 
  hdTable<-initializeDataTable(hdFilterFields)
  hdTable[,instnm_key:=as.character(instnm)]
  #start with most recent year and work back
  for(n in rev(years)) {
    table<-as.data.table(fread(paste0(hdDownloadDir,n,".csv")))
    table<-namesToLower(table)
    table[,instnm_key:=tolower(instnm)]
    new<-setDT(anti_join(table,hdTable,by="instnm_key"))
    new[,fiscal_year:=n]
    new<-new[,..hdFilterFields]
    hdTable<-rbind(new,hdTable)
    print(paste0("finished ", n))
  }
  
  #below takes most recent entry for fields with multiple names
  hdTable[,state:=stabbr]
  sdcols <- !"unitid" %in% hdFilterFields
  hdTable<-merge(hdTable[,lapply(.SD,max),by="unitid",.SDcols=c("fiscal_year","instnm")],hdTable,by=c("unitid","instnm"))
  setnames(hdTable,c("fiscal_year.x","stabbr","instnm"),c("fiscal_year","state","name"))
  #build unique names, with state
  dupeNames<-hdTable[,name][duplicated(hdTable[,name])]
  lapply(dupeNames, function(nm){
    hdTable[name == nm, name:=paste0(" (",state,")")]
  })
  #build unique names, with incremement 
  dupeNames<-hdTable[,name][duplicated(hdTable[,name])]
  lapply(dupeNames, function(nm){
    hdTable[name == nm, name:=make.unique(name)]
  })
  #build slug
  hdTable[,slug:=tolower(gsub(' ','-',name))]
  hdTable[,slug:=gsub('&','and',slug)]
  #protect against edge-case slug dupes
  dupeSlugs<-hdTable[,slug][duplicated(hdTable[,slug])]
  lapply(dupeSlugs, function(slg){
    hdTable[slug == slg, slug:=make.unique(slug)]
  })
  hdTable[,..hdReturnFields]
}

updateSchools <-function(table, con){
  if(nrow(table[,slug]) != nrow(table)){
    stop("slugs aren't unique!")
  }
  if(nrow(table[,name]) != nrow(table)){
    stop("names aren't unique!")
  }
  if(nrow(table[,unitid]) != nrow(table)){
    stop("unitids aren't unique!")
  }
  
  oldTable<-setDT(con$find())
  
  newSchools<-anti_join(table,oldTable,by="name")
  
  con$drop()
  
  con$insert(table)
  
  #update school_data with latest names by unitid, using anti_joined table (i.e., that of new names) for speed
}