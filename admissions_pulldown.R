#returns: total_applied,total_admitted,total_enrolled

#sourceFile name discrepancies:
##ICyyyy till 2014, afterward ADMyyyy
##yyyy refers to fy
#year definition

library(data.table)
admYears <- c(2001:2016)
admDownloadDir<-"/Users/cklamann/Desktop/adm/"
admSourceFiles <- data.table(file = c(paste0("IC",c(2001:2013)),paste0("ADM",c(2014:2016))),fy = admYears)

#below is unnecessary because there are no field discrepancies here, but useful as reference
admFields<-c("unitid","applcnm","applcnw","admssnw","admssnm","enrlftm","enrlftw")
admSourceMaps<-buildFieldList(data.table(fy = 2000:2016),admFields,admFields)

#fields to return
admReturnFields<-c("unitid","total_applied","total_admitted","total_enrolled","fiscal_year")

admDownloadTables<-function( yearVec = admYears ){
  download(admSourceFiles,yearVec,admDownloadDir)
}

admFilterData<-function(years = admYears){
  admTable<-initializeDataTable(admReturnFields)
  for(n in years){
    table<-as.data.table(read.csv(paste0(admDownloadDir,n,".csv"),stringsAsFactors = F))
    table<-cleanNumericTable(table)
    table[,total_applied:=plus(applcnm,applcnw)]
    table[,total_enrolled:=plus(enrlftm,enrlftw)]
    table[,total_admitted:=plus(admssnw,admssnm)]
    table[,fiscal_year:=n]
    table<-table[,.SD, .SDcols = admReturnFields]
    admTable<-rbind(table,admTable)
  }
  admTable
}



