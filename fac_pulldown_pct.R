      gettable<-function(){
	  value<-2012:2013
	  value<-as.character(value)
	  download_dir<-"https://nces.ed.gov/ipeds/datacenter/data/"
	  dest_dir<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/fac/"
	  temp <- tempfile()
	  for(n in 1:(length(value))){
	    download_file<-paste("s",value[n],"_nh.zip", sep="")
	    unzipped_file<-paste("s",value[n],"_nh.csv", sep="")
	    dest_file<-paste(value[n],".csv", sep="")
	    download.file((paste(download_dir,download_file, sep="")), temp)
	    data <- read.csv(unz(temp,unzipped_file))
          unlink(temp)
	    write.csv(data, file = (paste(dest_dir,dest_file, sep="")), row.names=FALSE)		
	  }
      }

	filtertable<-function(){
	value<-2002:2013
      directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/fac/"
	filter_file<-read.csv("C:/Users/Conor/Desktop/colleges.csv",stringsAsFactors=FALSE)
	filteredTable <- data.frame()
	library('data.table')
	for(i in 1:length(value)){
	  dest_file<-paste(value[i],"_f.csv", sep="")
	  filteredTable <- read.csv((paste(directory,value[i],".csv", sep="")),stringsAsFactors=FALSE)
	  if("staff24" %in% colnames(filteredTable)){	
		filteredTable <- filteredTable[,c("unitid","sgtype","staff24")]
	    names(filteredTable)[names(filteredTable)=="staff24"] <- "HRTOTLT"
		names(filteredTable)[names(filteredTable)=="unitid"] <- "UNITID"
		names(filteredTable)[names(filteredTable)=="sgtype"] <- "SGTYPE"
	  }
	  else if("FACSTAT" %in% colnames(filteredTable)){
		filteredTable <- filteredTable[,c("UNITID","FACSTAT","HRTOTLT","OCCUPCAT")]
		names(filteredTable)[names(filteredTable)=="FACSTAT"] <- "SGTYPE"
		filteredTable$SGTYPE[which(filteredTable$OCCUPCAT == 100)] <- 6 #all new staff
		filteredTable$SGTYPE[filteredTable$SGTYPE==0]<-1 #all new fac hires
		filteredTable$SGTYPE[filteredTable$SGTYPE==20]<-3 
		filteredTable$SGTYPE[filteredTable$SGTYPE==30]<-4
		filteredTable$SGTYPE[filteredTable$SGTYPE==40]<-5
		filteredTable$SGTYPE[filteredTable$SGTYPE==50]<-5	  
		filteredTable$OCCUPCAT <- NULL
	  }
	
	filteredTable<-as.data.table(filteredTable)
	filteredTable<-dcast.data.table(filteredTable, UNITID~SGTYPE, value.var="HRTOTLT",sum, fill=as.integer(0))
	setnames(filteredTable, c("1", "3", "4", "5", "6"), c("total_hires","tenured","tt","non_tt","staff"))
	filteredTable[,t_tt_total:=tenured+tt]
	filteredTable[,fall_year:=value[i]]
	filteredTable<-filteredTable[, c("UNITID","total_hires","t_tt_total", "non_tt","staff","fall_year"), with=FALSE]
	filteredTable<-merge(filteredTable, filter_file, by="UNITID")
	filteredTable$t_tt_total<-round((filteredTable$t_tt_total/filteredTable$total_hires)*100, digits=2)
	filteredTable$non_tt<-round((filteredTable$non_tt/filteredTable$total_hires)*100, digits=2)
	filteredTable$staff<-round((filteredTable$staff/filteredTable$total_hires)*100, digits=2)
	m_total_hires <- median(filteredTable[,total_hires], na.rm=TRUE)
	m_t_tt_total <- median(filteredTable[,t_tt_total], na.rm=TRUE)
	m_non_tt <- median(filteredTable[,non_tt], na.rm=TRUE)
	m_staff <- median(filteredTable[,staff], na.rm=TRUE)
	filteredTable<-as.data.frame(filteredTable)
	filteredTable[nrow(filteredTable)+1,] <- c(999999, m_total_hires, m_t_tt_total, m_non_tt, m_staff, value[i])
	write.csv(filteredTable, file=(paste(directory,dest_file, sep="")), row.names=FALSE)
	}
}

   mergeTables<-function(){
	value<-value<-2002:2013
	directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/fac/"
	frame <- data.frame()
	for(i in 1:(length(value))){
	  frame <- rbind(frame, read.csv(paste(directory, value[i], "_f.csv", sep=""))) 	
	}
	frame[is.na(frame)]<-0
	write.csv(frame, file=(paste(directory,"big_one.csv", sep="")), row.names=FALSE)
  }