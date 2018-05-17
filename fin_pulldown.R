##strategy:
# pulldown all tables from both paths
# compile/rename each so that they have the same fields
# rbind

#file names in folders refer to fiscal years
#private: FyyYY_F2 where yyYY = ay, e.g. 0405 is ay 0405, fy 2005
#public: FyyYY_F1A where yyYY = ay, e.g. 0405 is ay 0405, fy 2005

library(data.table)
finYears <- c(2002:2015)
finPrivDownloadDir<-"/Users/cklamann/Desktop/finPriv/"
finPubDownloadDir<- "/Users/cklamann/Desktop/finPub/"
finPrivSourceFiles <- data.table(file = c(paste0("F",fyToAy(finYears),"_F2")),fy = finYears)
finPubSourceFiles <- data.table(file = c(paste0("F",fyToAy(finYears),"_F1A")),fy = finYears)
finReturnFields<- c("unitid","total_unrest_net_assets","total_net_assets","total_tuition_and_fees",
                    "state_appropriations","investment_income","gifts","fiscal_year")

finDownloadTables<-function( yearVec = finYears ){
  download(finPrivSourceFiles,yearVec,finPrivDownloadDir)
  download(finPubSourceFiles,yearVec,finPubDownloadDir)
}

finNames<-c("total_unrest_net_assets","total_net_assets","total_tuition_and_fees","state_appropriations","investment_income")
finPrivVars<-c("f2a04","f2a06", "f2d01","f2d03","f2d10")
finPubVars<- c("f1a17","f1a18","f1b01","f1b11","f1b17")

finFilterData<-function(years = finYears){ 
  finTable<-initializeDataTable(finReturnFields)
  for(n in years) {
    pubTable<-as.data.table(read.csv(paste0(finPubDownloadDir,n,".csv"),stringsAsFactors = F))
    privTable<-as.data.table(read.csv(paste0(finPrivDownloadDir,n,".csv"),stringsAsFactors = F))
    privTable<-cleanNumericTable(privTable)
    privTable[,gifts:= plus2(f2d08,f2d09)]
    setnames(privTable,finPrivVars,finNames)
    pubTable<-cleanNumericTable(pubTable)
    pubTable[,gifts:=sum(f1b15,f1b16,f1b18,f1b21,f1b22,f1b24,na.rm=T)]
    setnames(pubTable,finPubVars,finNames)
    privTable[,fiscal_year:= n]
    pubTable[,fiscal_year:= n]
    privTable<-privTable[,.SD, .SDcols = finReturnFields]
    pubTable<-pubTable[,.SD, .SDcols = finReturnFields]
    table<-rbind(privTable,pubTable)
  	finTable<-rbind(table,finTable)
  	print(paste0("finished ", n))
	}
	finTable
}

