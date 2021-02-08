##strategy:
# pulldown all tables from both paths
# compile/rename each so that they have the same fields
# rbind

#file names in folders refer to fiscal years
#private: FyyYY_F2 where yyYY = ay, e.g. 0405 is ay 0405, fy 2005
#public: FyyYY_F1A where yyYY = ay, e.g. 0405 is ay 0405, fy 2005

library(data.table)
finYears <- c(2002:2018)
finPrivSourceFiles <- data.table(file = c(paste0("F",fyToAy(finYears),"_F2")),fy = finYears)
finPubSourceFiles <- data.table(file = c(paste0("F",fyToAy(finYears),"_F1A")),fy = finYears)
finProSourceFiles <- data.table(file = c(paste0("F",fyToAy(finYears),"_F3")),fy = finYears)
finReturnFields<- c("unitid","total_unrest_net_assets","total_rest_net_assets","total_net_assets","total_tuition_and_fees",
                    "state_appropriations","investment_income","gifts","auxiliary_revenues","fiscal_year")

finDownloadTables<-function(targetDir, yearVec = finYears ){
  download(finPrivSourceFiles,yearVec,paste0(targetDir, "/priv_"))
  download(finPubSourceFiles,yearVec,paste0(targetDir, "/pub_"))
  download(finProSourceFiles,yearVec,paste0(targetDir, "/pro_"))
}

#note that gifts and total_rest_net aren't here, they're managed in the loop b/c they have calcs
finNames<-c("total_unrest_net_assets","total_net_assets","total_tuition_and_fees","state_appropriations","investment_income","auxiliary_revenues")
finPrivVars<-c("f2a04","f2a06", "f2d01","f2d03","f2d10","f2d12")
finPubVars<- c("f1a17","f1a18","f1b01","f1b11","f1b17","f1b05")
finProVars<-c("total_unrest_net_assets","total_net_assets","f3d01","f3d03a","f3d05","f3d07")

finTransformData<-function(targetDirectory, years = finYears){ 
  finTable<-initializeDataTable(finReturnFields)
  for(n in years) {
    pubTable<-as.data.table(read.csv(paste0(paste0(targetDirectory, "/pub_"),n,".csv"),stringsAsFactors = F))
    privTable<-as.data.table(read.csv(paste0(paste0(targetDirectory, "/priv_"),n,".csv"),stringsAsFactors = F))
    proTable<-as.data.table(read.csv(paste0(paste0(targetDirectory, "/pro_"),n,".csv"),stringsAsFactors = F))
    privTable<-cleanNumericTable(privTable)
    privTable[,gifts:= plus3(f2d08,f2d09)]
    privTable[,total_rest_net_assets:=f2a05]
    setnames(privTable,finPrivVars,finNames)
    pubTable<-cleanNumericTable(pubTable)
    pubTable[,gifts:=plus3(f1b16,f1b21,f1b22)]
    pubTable[,total_rest_net_assets:=plus3(f1a15,f1a16)]
    setnames(pubTable,finPubVars,finNames)
    proTable<-cleanNumericTable(proTable)
    proTable[,c("total_unrest_net_assets","total_rest_net_assets"):=NA]
    proTable[,total_net_assets:=minus(f3a01,f3a02)]
    proTable[,gifts:=f3d04]
    if(! "f3d03a" %in% names(proTable)){
      proTable[,f3d03a:=0]
    }
    setnames(proTable,finProVars,finNames)
    proTable[,fiscal_year:= n]
    privTable[,fiscal_year:= n]
    pubTable[,fiscal_year:= n]
    privTable<-privTable[,.SD, .SDcols = finReturnFields]
    pubTable<-pubTable[,.SD, .SDcols = finReturnFields]
    proTable<-proTable[,.SD, .SDcols = finReturnFields]
    table<-rbind(privTable,pubTable,proTable)
  	finTable<-rbind(table,finTable)
  	print(paste0("finished ", n))
	}
	finTable
}

