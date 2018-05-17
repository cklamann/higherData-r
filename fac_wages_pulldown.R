#SALyyyy
#yyyy refers to ay, must be adjusted to fy in code
#tables start in ay 2002-3, which is when the necessary data became available

#return fields: "full_professor","associate_professor","assistant_professor","instructor","lecturer"

library(data.table)
facWageYears <- c(2002:2015)
facWageDownloadDir<-"/Users/cklamann/Desktop/facWage/"
facWageSourceFiles <- data.table(file = c(paste0("SAL",2001,"_A_S"),paste0("SAL",c(2002:2011),"_A"),paste0("SAL",c(2012:2015),"_IS")),fy = facWageYears)

#aggregation maps:

#2002:2011 
##saoutlt = plus(outlaym,outlayw)
##satotlt = plus(empcntm,empcntw)

#also, 2002-2011 has contract field, while 2012+ is only for fac on a 9, 10, or 12 month contract
#we take mean of 9/10/11/12 for 2002-11 and exclude less than 9 months (contract==3) to make them even

facWageReturnFields<-c("unitid","full_professor","associate_professor","assistant_professor","instructor","lecturer","fiscal_year")

facWageDownloadTables<-function( yearVec = facWageYears ){
  download(facWageSourceFiles,yearVec,facWageDownloadDir)
}

facWageFilterData<-function(years = facWageYears){
  facWageTable<-initializeDataTable(facWageReturnFields)
  for(n in years){
    table<-as.data.table(read.csv(paste0(facWageDownloadDir,n,".csv"),stringsAsFactors = F))
    table<-cleanNumericTable(table)
    if(n<2012){
      table[,saoutlt:=plus(outlaym,outlayw)]
      table[,satotlt:=plus(empcntm,empcntw)]
      table<-table[contract != 3]
    }
    table[,avgsal:=saoutlt/satotlt]
    table<-dcast.data.table(table, unitid~arank, value.var="avgsal", fill=as.integer(NA),fun.aggregate = mean)
    setnames(table,c("unitid",1,2,3,4,5,6,7),c("unitid","full_professor","associate_professor","assistant_professor","instructor","lecturer","none","total"))
    table[,fiscal_year:=n+1]
    table<-table[,.SD, .SDcols = facWageReturnFields]
    facWageTable<-rbind(facWageTable,table)
  }
  facWageTable
}