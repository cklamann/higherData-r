#SALyyyy
#yyyy refers to ay, adjusted to fy in filter function
#tables start in ay 2002-3, which is when the necessary data became available

#return fields: "full_professor_wage","associate_professor_wage","assistant_professor_wage","instructor_wage","lecturer_wage"

library(data.table)
facWageYears <- c(2003:2017)
facWageDownloadDir<-"/home/conor/higherData-r/data/ipeds/fac-wage/"
facWageSourceFiles <- data.table(file = c(paste0("SAL",c(2003:2011),"_A"),paste0("SAL",c(2012:2017),"_IS")),fy = facWageYears)

#aggregation maps:

#2002:2011 
##saoutlt = plus(outlaym,outlayw)
##satotlt = plus(empcntm,empcntw)

#also, 2002-2011 has contract field, while 2012+ is only for fac on a 9, 10, or 12 month contract
#we take mean of 9/10/12 for 2002-11 and exclude less than 9 months (contract==3) to make them even

facWageReturnFields<-c("unitid","full_professor_wage","associate_professor_wage","assistant_professor_wage",
                       "instructor_wage","lecturer_wage","fiscal_year")

facWageDownloadTables<-function( yearVec = facWageYears ){
  download(facWageSourceFiles,yearVec,facWageDownloadDir)
}

facWageFilterData<-function(years = facWageYears){
  facWageTable<-initializeDataTable(facWageReturnFields)
  for(n in years){
    table<-fread(paste0(facWageDownloadDir,n,".csv"),stringsAsFactors = F)
    table<-cleanNumericTable(table)

    if(n<2012){
      table[,saoutlt:=plus(outlaym,outlayw)]
      table[,satotlt:=plus(empcntm,empcntw)]
      table<-table[contract != 3]
    }
    table[,avgsal:=saoutlt/satotlt]
    table<-dcast.data.table(table, unitid~arank, value.var="avgsal", fill=as.integer(NA),fun.aggregate = mean)
    setnames(table,c("unitid",1,2,3,4,5,6),c("unitid","full_professor_wage","associate_professor_wage","assistant_professor_wage",
                                             "instructor_wage","lecturer_wage","none"))
    table[,fiscal_year:=n+1]
    table<-table[,.SD, .SDcols = facWageReturnFields]
    facWageTable<-rbind(facWageTable,table)
  }
  facWageTable
}