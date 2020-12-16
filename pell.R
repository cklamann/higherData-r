grabtable<-function(){
  value<-as.character(c("01","02","03","04","05","06","07","08","09","10","11","12","13","14"))
  download_dir<-"https://nces.ed.gov/ipeds/datacenter/data/"
  dest_dir<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/pell/"
  temp <- tempfile()
  for(n in 1:(length(value)-1)){
    download_file<-paste("F",value[n],value[n+1],"_F1A",".zip", sep="")
    unzipped_file<-paste("f",value[n],value[n+1],"_f1a",".csv", sep="")
    dest_file<-paste(value[n],".csv", sep="")
    download.file((paste(download_dir,download_file, sep="")), temp)
    data <- read.csv(unz(temp,unzipped_file))
    unlink(temp)
    write.csv(data, file = (paste(dest_dir,dest_file, sep="")), row.names=FALSE)		
  }
}

filter_fin_table<-function(){
  value<-as.character(c("01","02","03","04","05","06","07","08","09","10","11","12","13","14"))
  directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/pell/"
  filter_file<-read.csv("C:/Users/Conor/Desktop/new_colleges.csv",stringsAsFactors=FALSE)
  filteredTable <- data.frame()
  for(i in 1:length(value)){
    dest_file<-paste(value[i],"_f.csv", sep="")
    filteredTable <- read.csv((paste(directory,value[i],".csv", sep="")),stringsAsFactors=FALSE)
    library('data.table')
    filteredTable<-as.data.table(filteredTable)   
    names(filteredTable)<-tolower(names(filteredTable))	
    filteredTable<-merge(filteredTable, filter_file,by="unitid")
    filteredTable[,OPEID:=NULL]
    filteredTable[,INSTNM:=NULL]
    filteredTable[,type:=NULL]
    names(filteredTable)[names(filteredTable)=="f1e03"] <- "pell_total"
    filteredTable<-filteredTable[,c("unitid","pell_total"),with=FALSE]
    year<-paste("20",value[i], sep="")
    filteredTable$fall_year <- year
    print(tail(filteredTable))
    write.csv(filteredTable, file=(paste(directory,dest_file, sep="")), row.names=FALSE)
  }
}

merge_fin_tables<-function(){
  value<-as.character(c("01","02","03","04","05","06","07","08","09","10","11","12","13"))
  directory<- "C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/pell/"
  frame <- data.frame()
  for(i in 1:(length(value))){
    frame <- rbind(frame, read.csv(paste(directory, value[i], "_f.csv", sep=""))) 	
  }
  write.csv(frame, file=(paste(directory,"big_one.csv", sep="")), row.names=FALSE)
  frame
}