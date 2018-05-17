#hmmm looks like they put all this shit behind some javascript, need to dl manually (fuck)

library('xlsx') 
sportYears <- c(2001:2017)
sportDownloadYears<-fyToAyFull(sportYears,'-')
sportDownloadDir<-"/home/conor/Dropbox/study/research/sports/"
sportSourceFiles <- data.table(file = c(paste0("EADA%20",sportDownloadYears,".zip")),fy = sportYears)
sportDownloadUrl <- "http://ope.ed.gov/athletics/dataFiles/"

sportsDownloadTable<-function(){
  temp <- tempfile()
  for(n in 1:nrow(sportSourceFiles)){
    download_file<-sportSourceFiles[n,file]
    download.file(paste0(sportDownloadUrl,download_file),temp)
    unzipped_data<-unzip(temp)
    unlink(temp)
    file<-grep("inst.*\\.xls*",as.vector(unzipped_data),ignore.case=TRUE, perl=TRUE, value=FALSE) #search for file that starts with inst and ends with xls/xlsx
    table<-read.xlsx2(unzipped_data[file],1,stringsAsFactors=FALSE) #readxlsx2 works better
    write.csv(table, paste0(sportDownloadDir,sportSourceFiles[n,fy]), row.names=FALSE)		
  }
}

#table<-table[c("unitid","STUDENTAID_MEN","STUDENTAID_WOMEN","STUDENTAID_TOTAL","RECRUITEXP_MEN","RECRUITEXP_WOMEN","RECRUITEXP_TOTAL","HDCOACH_SAL_FTE_MEN","HDCOACH_SAL_FTE_WOMN","ASCOACH_SAL_FTE_MEN", "ASCOACH_SAL_FTE_WOMN","UNDUP_CT_PARTIC_MEN","UNDUP_CT_PARTIC_WOMEN","GRND_TOTAL_REVENUE","GRND_TOTAL_EXPENSE")]

filterTable<-function(){
  value<-c(15) 
  dest_dir<-"/home/conor/Dropbox/study/research/sports/"
  temp <- tempfile()
  for(n in 1:(length(value))){
    table<-fread(paste0(dest_dir,value[n],value[n]+1,".csv")) #readxlsx2 works better
    table<-table[,.(unitid,STUDENTAID_MEN,STUDENTAID_WOMEN,STUDENTAID_TOTAL,HDCOACH_SAL_FTE_MEN,HDCOACH_SAL_FTE_WOMN,ASCOACH_SAL_FTE_MEN,ASCOACH_SAL_FTE_WOMN,GRND_TOTAL_REVENUE,GRND_TOTAL_EXPENSE)]
    table<-as.data.table(lapply(table,as.numeric))
    table[is.na(table)]<-0
    fall_year<-paste("20",value[n],sep="")
    table[,fall_year:=fall_year]
    print(head(table))
    table[,c("OPEID","type","INSTNM"):=NULL]
    write.csv(table, file = paste0(dest_dir,value[n],value[n]+1,"_f.csv"), row.names=FALSE)		
  }
  table
}