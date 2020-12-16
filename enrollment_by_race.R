#EFyyyyA
#yyyy refers to academic year, adjusted to fiscal year in filter function

#returns numbers for efalevel==24, Full-time students, Undergraduate, Degree/certificate-seeking, First-time
#in other words, this is freshmen...

library(data.table)
raceYears <- c(2002:2020)
raceSourceFiles <- data.table(file = c(paste0("EF",raceYears,"A")),fy = raceYears)

raceReturnFields<-c("unitid","american_indian_alaskan_enr","asian_or_pac_islander_enr","black_enr","hispanic_enr","white_enr","other_enr","fiscal_year")

#renaming maps:
#efraceXX is EFAXXX from 2002 to 2007
oldCols <- c("efrace18","efrace19","efrace20","efrace21","efrace22","efrace24")
newCols <- c("efbkaat","efaiant","efasiat","efhispt","efwhitt","eftotlt")

raceDownloadTables<-function(raceDownloadDir, yearVec = raceYears ){
  download(raceSourceFiles,yearVec,raceDownloadDir)
}

raceFilterData<-function(years = raceYears){ 
  raceTable<-initializeDataTable(raceReturnFields)
  for(n in years) {
    table<-as.data.table(read.csv(paste0(raceDownloadDir,n,".csv"),stringsAsFactors = F))
    table<-cleanNumericTable(table)
    if(n<2008){
      setnames(table,oldCols,newCols)
    }
    if(n==2008 | n==2009){ #2008 and 2009 have both sets of variables
      for(i in 1: (length(oldCols)-1)){
        table[[newCols[i]]] <- plus(table[[newCols[i]]],table[[oldCols[i]]])
      }
    }
	  table<-table[efalevel==24]
	  setnames(table, c(newCols), c("black_enr","american_indian_alaskan_enr","asian_or_pac_islander_enr","hispanic_enr","white_enr","total"))		
  	table[,fiscal_year:=n+1]
  	na.omit(table)
  	table[,other_enr:= total - (black_enr + american_indian_alaskan_enr + asian_or_pac_islander_enr + hispanic_enr + white_enr)]
  	table<-table[,.SD, .SDcols = raceReturnFields]
  	raceTable<-rbind(table,raceTable)
  	print(paste0("finished ", n))
	}
	raceTable
}