gettable<-function(){
	  value<-2012:2014
	  value<-as.character(value)
	  download_dir<-"https://nces.ed.gov/ipeds/datacenter/data/"
	  dest_dir<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/admin_v_teaching/"
	  temp <- tempfile()
	  for(n in 1:(length(value))){
	    download_file<-paste("s",value[n],"_OC.zip", sep="")
	    unzipped_file<-paste("s",value[n],"_oc.csv", sep="")
	    dest_file<-paste(value[n],".csv", sep="")
	    download.file((paste(download_dir,download_file, sep="")), temp)
	    data <- read.csv(unz(temp,unzipped_file))
          unlink(temp)
	    write.csv(data, file = (paste0(dest_dir,dest_file)), row.names=FALSE)		
	  }
}

filtertable<-function(){ 
	value<-2002:2014
  directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/admin_v_teaching/"
	filter_file<-as.data.table(read.csv("C:/Users/Conor/Desktop/new_colleges.csv",stringsAsFactors=FALSE))
	library('data.table')
	for(i in 1:length(value)){
	  filteredTable <- as.data.table(read.csv(paste(directory,value[i],".csv", sep=""),stringsAsFactors=FALSE))
	  names(filteredTable)<-tolower(names(filteredTable))
	  if(!"staffcat" %in% colnames(filteredTable)){	
	     if("hrtotlt" %in% colnames(filteredTable)){
	       setnames(filteredTable,"hrtotlt","staff24")
	     }
	    filteredTable <- filteredTable[,.(unitid,sabdtype,staff24)]
	    names(filteredTable)<-tolower(names(filteredTable))
	    filteredTable<-as.data.table(dcast.data.table(filteredTable, unitid~sabdtype, value.var="staff24",sum, fill=NA_real_))
	    setnames(filteredTable,c("1","2"),c("total","teaching"))
	    filteredTable[,"staff"]<-minus(filteredTable[,total], filteredTable[,teaching])
	    filteredTable<-filteredTable[,.(unitid,total,teaching,staff)]

	  }
	  else{
	    names(filteredTable)<-tolower(names(filteredTable))
	    filteredTable<-getTeachVsStaffPost2011(filteredTable)
	  }
	  filteredTable<-as.data.table(merge(filteredTable,filter_file,by="unitid"))
	  filteredTable[,INSTNM:=NULL]
	  filteredTable[,OPEID:=NULL]
	  filteredTable[,type:=NULL]
	  filteredTable[,fall_year:=value[i]]
	  filteredTable<-filteredTable[, .(unitid,teaching,staff,total,fall_year)]
	  print(head(filteredTable))
	  write.csv(filteredTable, file=(paste0(directory,value[i],"_f.csv")), row.names=FALSE)
  }
}

mergeTables<-function(){
	value<-2002:2014
	directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/admin_v_teaching/"
	frame <- data.frame()
	for(i in 1:(length(value))){
	  frame <- rbind(frame, read.csv(paste(directory, value[i], "_f.csv", sep=""))) 	
	}
	frame[is.na(frame)]<-"NULL"
	write.csv(frame, file=(paste(directory,"big_one_totals_with_NULLs.csv", sep="")), row.names=FALSE)
  frame
}

plus<-function(vec1,vec2){
  result=vector()
  for(n in 1:length(vec1)){
    if(is.na(vec1[n])){
      result[n]<-vec2[n] 
    }
    else if(is.na(vec2[n])){
      result[n]<-vec1[n]
    }
    else result[n]<-vec1[n]+vec2[n]
  }
  result
}

minus<-function(vec1,vec2){
  result=vector()
  for(n in 1:length(vec1)){
    if(is.na(vec1[n])){
      result[n]<-vec1[n] 
    }
    else if(is.na(vec2[n])){
      result[n]<-vec1[n]
    }
    else result[n]<-vec1[n]-vec2[n]
  }
  result
}

getTeachVsStaffPost2011<-function(table){
  teachers<-table[staffcat==1200]
  all<-table[staffcat==1100]
  teachers<-teachers[,.(unitid,hrtotlt)]
  all<-all[,.(unitid,hrtotlt)]
  teachers<-teachers[,sum(hrtotlt),by="unitid"]
  setnames(teachers,"V1","teaching")
  setnames(all,"hrtotlt","total")
  setkey(teachers,unitid)
  setkey(all,unitid)
  new<-all[teachers]
  new[,"staff"]<-minus(new[,total],new[,teaching])
  new
}