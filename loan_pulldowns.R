gettable<-function(){
	value<-formatYears(14)
	download_dir<-"https://nces.ed.gov/ipeds/datacenter/data/"
	dest_dir<-"/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/loan_a/"
	temp <- tempfile()
	for(n in 1){
	  download_file<-paste("SFA",value[n],value[n]+1,".zip", sep="")
	  unzipped_file<-paste("sfa",value[n],value[n]+1,".csv", sep="")
	  dest_file<-paste(value[n],".csv", sep="")
	  download.file((paste(download_dir,download_file, sep="")), temp)
	  data <- read.csv(unz(temp,unzipped_file))
        unlink(temp)
	  write.csv(data, file = (paste(dest_dir,dest_file, sep="")), row.names=FALSE)		
	}
}

filtertable<-function(){
	value<-formatYears(14)
	directory<-"/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/loan_a/"
	write_directory <- "/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/loan_a/"
	filter_file<-as.data.table(read.csv("/home/conor/Desktop/new_colleges.csv",stringsAsFactors=FALSE))
	filteredTable <- data.table()
	for(i in 1:length(value)){
	dest_file<-paste(value[i],"_f.csv", sep="")
	filteredTable <- as.data.table(read.csv((paste0(directory,value[i],".csv")),stringsAsFactors=FALSE))
	names(filteredTable)<-tolower(names(filteredTable))
	filteredTable<-filteredTable[,.(unitid,loan_a)]
	year<-paste("20",value[i], sep="")
	filteredTable[,"fall_year"] <- year
	print(str(filteredTable))
	filteredTable<-merge(filteredTable, filter_file,by="unitid")
	filteredTable[,INSTNM:=NULL]
	filteredTable[,OPEID:=NULL]
	filteredTable[,type:=NULL]
	print(tail(filteredTable))
	write.csv(filteredTable, file=(paste(write_directory,dest_file, sep="")), row.names=FALSE)
	}
	filteredTable
}

mergeTables<-function(){
	value<-formatYears(9:13)
	directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/loan_a/"
	frame <- data.frame()
	for(i in 1:(length(value))){
	  frame <- rbind(frame, read.csv(paste(directory, value[i], "_f.csv", sep=""))) 	
	  print(value[i])
	}
	write.csv(frame, file=(paste(directory,"big_one.csv", sep="")), row.names=FALSE)
  frame
}




