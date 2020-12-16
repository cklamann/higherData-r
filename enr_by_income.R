gettable<-function(){
  value<-c("08","09","10","11","12","13","14")
  value<-as.character(value)
  download_dir<-"https://nces.ed.gov/ipeds/datacenter/data/"
  dest_dir<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/enr_by_income/"
  temp <- tempfile()
  for(n in 1:(length(value))){
    download_file<-paste("SFA",value[n],value[n+1],".zip", sep="")
    unzipped_file<-paste("sfa",value[n],value[n+1],".csv", sep="")
    dest_file<-paste(value[n],".csv", sep="")
    download.file((paste(download_dir,download_file, sep="")), temp)
    data <- read.csv(unz(temp,unzipped_file))
    unlink(temp)
    write.csv(data, file = (paste(dest_dir,dest_file, sep="")), row.names=FALSE)		
  }
}

filterTable<-function(){
  library('data.table')
  value<-c("08","09","10","11","12","13")
  directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/enr_by_income/"
  for(n in 1:length(value)){
    dest_file<-paste(value[n],value[n+1],"_f.csv",sep="")
    filteredTable<-as.data.table(read.csv(paste(directory,value[n],value[n+1],".csv",sep=""),stringsAsFactors = FALSE))
    names(filteredTable)<-tolower(names(filteredTable))
    #lapply(filteredTable, function(x) gsub(".",0,x))
    filteredTable<-filteredTable[,list(unitid,gis4n12,gis4n22,gis4n32,gis4n42,gis4n52)]
    setnames(filteredTable,c("gis4n12","gis4n22","gis4n32","gis4n42","gis4n52"),c("0_to_30k","30k_to_48k","48k_to_75k","75k_to_110k","over_110k"))
    filteredTable<-as.data.table(lapply(filteredTable, as.numeric))
    filteredTable[is.na(filteredTable)]<-0
    #filteredTable<-filteredTable[,list(unitid,applcnm,applcnw,admssnm,admssnw,enrlftm,enrlftw,total_app,total_admit,total_enr)]
    filteredTable[,"fall_year"]<-paste(20,value[n],sep="")
    filteredTable<-merge(filteredTable,as.data.table(read.csv("C:/Users/Conor/Desktop/colleges.csv")),by="unitid")
    print(head(filteredTable))
    write.csv(filteredTable,paste(directory,dest_file,sep=""))
  }
}

mergeTables<-function(){
  value<-c("08","09","10","11","12","13")
  directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/enr_by_income/"
  frame <- data.frame()
  for(i in 1:(length(value)-1)){
    frame <- rbind(frame, read.csv(paste(directory, value[i],value[i+1], "_f.csv", sep=""))) 	
  }
  frame[is.na(frame)]<-0
  write.csv(frame, file=(paste(directory,"big_one.csv", sep="")), row.names=FALSE)
}
