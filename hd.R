##strategy:
#each time a new one of these comes out, run this script and push
library(dplyr)
library(data.table)
library(stringr)
hdYears <- c(2002:2019)
hdSourceFiles <- data.table(file = c(paste0("HD",hdYears)),fy = hdYears)
hdFilterFields <- c("unitid","instnm","instnm_key","stabbr","sector","hbcu","locale","fiscal_year", "city","ein")
hdReturnFields<- c("unitid","name","state","sector","hbcu","locale","slug", "city","ein")

hdDownloadTables<-function(targetDir, yearVec = hdYears ){
  download(hdSourceFiles, yearVec, targetDir)
}

hdTransformData<-function(hdDownloadDir, years = hdYears){ 
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
  .transformHdTable(hdTable)
}

buildCrosswalk<-function(dir = hdDownloadDir, years = hdYears){
  tabl<-lapply(years,function(year){
    table<-fread(paste0(dir,year,'.csv'))
    setnames(table,names(table),tolower(names(table)))
    table[,.(opeid,unitid)]
  })
  tabl <- rbindlist(tabl)
  tabl <- unique(tabl,by=c("opeid"))
  tabl[!is.na(unitid) & !is.na(opeid)]
}

.transformHdTable<-function(hdTable){
  hdTable[,state:=stabbr]
  hdTable[,name:=sub("^\\s+", "", instnm)]

  hdTable<-hdTable[, .SD[which.max(fiscal_year)],by=unitid]
  
  #build unique names, with state
  dupeNames<-hdTable[,name][duplicated(hdTable[,name])]
  lapply(dupeNames, function(nm){
    hdTable[name == nm, name:=paste0(name," (",state,")")]
  })
  
  #build unique names, with increment 
  dupeNames<-hdTable[,name][duplicated(hdTable[,name])]
  lapply(dupeNames, function(nm){
    hdTable[name == nm, name:=make.unique(name)]
  })
  #build slug
  hdTable[,slug:=tolower(gsub(' ','-',name))]
  hdTable[,slug:=tolower(gsub("-{2,}","-",name))]
  hdTable[,slug:=tolower(gsub(' ','-',name))]
  hdTable[,slug:=gsub('&','and',slug)]
  #protect against edge-case slug dupes
  dupeSlugs<-hdTable[,slug][duplicated(hdTable[,slug])]
  lapply(dupeSlugs, function(slg){
    hdTable[slug == slg, slug:=make.unique(slug)]
  })
  hdTable[,..hdReturnFields]  
}

#after this, want to run mongo_scripts::updateSchoolNames() with school connection
updateSchools <-function(table, con){
  if(length(table[,slug]) != nrow(table)){
    stop("slugs aren't unique!")
  }
  if(length(table[,name]) != nrow(table)){
    stop("names aren't unique!")
  }
  if(length(table[,unitid]) != nrow(table)){
    stop("unitids aren't unique!")
  }
  
  oldTable<-setDT(con$find())
  
  if("instnm" %in% names(oldTable)){
    setnames(oldTable,"instnm","name")
  }
  
  newSchools<-anti_join(table,oldTable,by="name")
  
  con$drop()
  
  con$insert(table)
}