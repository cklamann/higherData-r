#https://ope.ed.gov/athletics/api/dataFiles/file?fileName=EADA_2016-2017.zip
#https://ope.ed.gov/athletics/api/dataFiles/file?fileName=EADA%202002-2003.zip

#each year is fiscal year
#note: the data is consistent only from 2006!

sportsReturnFields <- tolower(c("unitid","fiscal_year","STUDENTAID_MEN","STUDENTAID_WOMEN","HDCOACH_SAL_FTE_MEN",
                                "HDCOACH_SAL_FTE_WOMN","ASCOACH_SAL_FTE_MEN", "ASCOACH_SAL_FTE_WOMN","GRND_TOTAL_REVENUE",
                                "GRND_TOTAL_EXPENSE"))


library('xlsx') 
sportYears1 <- c(2015:2015)
sportYears2 <- c(2017:2018)
sportDownloadYears1 <- fyToAyFull(sportYears1,'-')
sportDownloadYears2 <- fyToAyFull(sportYears2,'-')
sportDownloadDir <- "/home/conor/higherData-r/data/sports/"
sportSourceFiles <- data.table(file = c(paste0("EADA%20",sportDownloadYears1,".zip")),fy = sportYears1)
sportSourceFiles <- rbind(sportSourceFiles, data.table(file = c(paste0("EADA_",sportDownloadYears2,".zip")),fy = sportYears2))
sportSourceFiles <- data.table(file = c(paste0("EADA_",sportDownloadYears2,".zip")),fy = sportYears2)
sportDownloadUrl <- "https://ope.ed.gov/athletics/api/dataFiles/file?fileName="

sportsDownloadTable<-function(){
  temp <- tempfile()
  for(n in 1:nrow(sportSourceFiles)){
    download_file<-sportSourceFiles[n,file]
    download.file(paste0(sportDownloadUrl,download_file),temp)
    unzipped_data<-unzip(temp)
    unlink(temp)
    file<-grep("inst.+\\.xls",as.vector(unzipped_data),ignore.case=TRUE, perl=TRUE, value=FALSE) #search for file that starts with inst and ends with xls/xlsx
    if(length(file > 1)){
      file <- file[1] #
    }
    if(length(file) == 0 ){
      file<-grep(".xls",as.vector(unzipped_data),ignore.case=TRUE, perl=TRUE, value=FALSE) #search for file that starts with inst and ends with xls/xlsx  
    }
    table<-read.xlsx2(unzipped_data[file],1,stringsAsFactors=FALSE) #readxlsx2 works better
    write.csv(table, paste0(sportDownloadDir,sportSourceFiles[n,fy], '.csv'), row.names=FALSE)		
  }
}

#table<-table[c("unitid","STUDENTAID_MEN","STUDENTAID_WOMEN","STUDENTAID_TOTAL","RECRUITEXP_MEN","RECRUITEXP_WOMEN","RECRUITEXP_TOTAL","HDCOACH_SAL_FTE_MEN","HDCOACH_SAL_FTE_WOMN","ASCOACH_SAL_FTE_MEN", "ASCOACH_SAL_FTE_WOMN","UNDUP_CT_PARTIC_MEN","UNDUP_CT_PARTIC_WOMEN","GRND_TOTAL_REVENUE","GRND_TOTAL_EXPENSE")]

filterSportsTable<-function(){
  value<-c(2007:2017) 
  dir<-"/home/conor/higherData-r/data/sports/"
  sportsTable<-initializeDataTable(sportsReturnFields)
  for(n in 1:(length(value))){
    table<-fread(paste0(dir,value[n],".csv"))
    table<-cleanNumericTable(table)
    table[,fiscal_year:=value[n]]
    sportsTable <- rbind(sportsTable,table[,..sportsReturnFields])
  }
  sportsTable
}