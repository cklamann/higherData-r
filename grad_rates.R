#gryyyy refers to fiscal_year 2015 (specifically, data as of August 31, yyyy for 2 and 4-yr cohorts)
#grtypes:
#8 = 4-yr BA, full cohort
#9 = 4-yr BA, completers in 150% of time
#29 = 2 yr cohort
#30 = 2 yr completers in 150% of time

library(data.table)
gradRateYears <- c(2001:2020)
gradRateSourceFiles <- data.table(file = c(paste0("GR",gradRateYears)),fy = gradRateYears)

gradRateReturnFields<-c("unitid","white_p","asian_p","black_p","hispanic_p","total_p","fiscal_year")

gradRateDownloadTables<-function(gradRateDownloadDir, yearVec = gradRateYears ){
  download(gradRateSourceFiles,yearVec,gradRateDownloadDir)
}

#renaming maps:
#grraceXX is GRXXX from 2002 to 2010, like so: (though grrace24 becomes grracetotlt in 2008)
oldCols <- c("grrace18","grrace19","grrace20","grrace21","grrace22","grtotlt")
newCols <- c("grbkaat","graiant","grasiat","grhispt","grwhitt","grtotlt")

#2008-2010 have both sets of columns

gradRateFilterData<-function(years = gradRateYears){ 
  gradRateTable<-initializeDataTable(gradRateReturnFields)
  for(n in years) {
      table<-as.data.table(read.csv(paste0(gradRateDownloadDir,n,".csv"),stringsAsFactors = F))
      table<-cleanNumericTable(table)
    if(n<2008){
      if("grrace24" %in% names(table)){
        setnames(table,c("grrace24"),c("grtotlt"))
        setnames(table,oldCols,newCols)
      }
    }
    if(n %in% c(2008:2010)){ 
      for(i in 1: length(oldCols)){
        table[[newCols[i]]] <- plus2(table[[newCols[i]]],table[[oldCols[i]]])
      }
    }
    gradTable<-table[grtype==9]
    setnames(gradTable,newCols,c("black_g","american_indian_alaskan_g","asian_or_pac_islander_g","hispanic_g","white_g","total_g"))  
    allTable<-table[grtype==8]
    setnames(allTable, newCols, c("black_t","american_indian_alaskan_t","asian_or_pac_islander_t","hispanic_t","white_t","total_t"))		
    table<-merge(gradTable,allTable,by="unitid")
    table[,fiscal_year:=n]
    table[,c("white_p","asian_p","black_p","hispanic_p","total_p")]<-list(100*(round(table[,white_g]/table[,white_t],2)),100*(round(table[,asian_or_pac_islander_g]/table[,asian_or_pac_islander_t],2)),100*(round(table[,black_g]/table[,black_t],2)),100*(round(table[,hispanic_g]/table[,hispanic_t],2)),100*(round(table[,total_g]/table[,total_t],2)))
    table<-table[,.SD, .SDcols = gradRateReturnFields]
    gradRateTable<-rbind(table,gradRateTable)
    print(paste0("finished ", n))
  }
  gradRateTable
}