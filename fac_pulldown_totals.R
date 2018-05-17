gettable<-function(){
	  value<-2011:201
	  value<-as.character(value)
	  download_dir<-"https://nces.ed.gov/ipeds/datacenter/data/"
	  dest_dir<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/fac/"
	  temp <- tempfile()
	  for(n in 1:(length(value))){
	    download_file<-paste("s",value[n],"_G.zip", sep="")
	    unzipped_file<-paste("s",value[n],"_g.csv", sep="")
	    dest_file<-paste(value[n],".csv", sep="")
	    download.file((paste(download_dir,download_file, sep="")), temp)
	    data <- read.csv(unz(temp,unzipped_file))
          unlink(temp)
	    write.csv(data, file = (paste(dest_dir,dest_file, sep="")), row.names=FALSE)		
	  }
}

filtertable<-function(){ #keep in mind that you jerry rigged 2010 by manually putting staff18 as a column name
	value<-2015
  directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/fac/"
	filter_file<-as.data.table(read.csv("C:/Users/Conor/Desktop/new_colleges.csv",stringsAsFactors=FALSE))
	library('data.table')
	for(i in 1:length(value)){
	  dest_file<-paste(value[i],"_f_totals.csv", sep="")
	  filteredTable <- as.data.table(read.csv(paste(directory,value[i],".csv", sep=""),stringsAsFactors=FALSE))
	  names(filteredTable)<-tolower(names(filteredTable))
	  if(!"snhcat" %in% colnames(filteredTable)){	
	     if("hrtotlt" %in% colnames(filteredTable)){
	       setnames(filteredTable,"hrtotlt","staff24")
	     }
	    filteredTable <- filteredTable[,.(unitid,sgtype,staff24)]
	    names(filteredTable)<-tolower(names(filteredTable))
	    filteredTable<-dcast.data.table(filteredTable, unitid~sgtype, value.var="staff24",sum, fill=NA_real_)
	    setnames(filteredTable, c("1","2","6"), c("total_hires","teach_total","staff"))
	    print(filteredTable[unitid==146481,.(total_hires,teach_total,staff)])
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
	  filteredTable<-filteredTable[, .(unitid,teach_total,staff,total_hires,fall_year)]
	  print(head(filteredTable))
	  write.csv(filteredTable, file=(paste0(directory,value[i],"_f_totals.csv")), row.names=FALSE)
}
}

mergeTables<-function(){
	value<-2002:2014
	directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/fac/"
	frame <- data.frame()
	for(i in 1:(length(value))){
	  frame <- rbind(frame, read.csv(paste(directory, value[i], "_f_totals.csv", sep=""))) 	
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

getTeachVsStaffPost2011<-function(nh_table){
  teachers<-nh_table[snhcat==20000]
  staff<-nh_table[snhcat>25000]
  all<-nh_table[snhcat==10000]
  teachers<-teachers[,.(unitid,hrtotlt)]
  staff<-staff[,.(unitid,hrtotlt)]
  all<-all[,.(unitid,hrtotlt)]
  teachers<-teachers[,sum(hrtotlt),by="unitid"]
  staff<-staff[,sum(hrtotlt),by="unitid"]
  setnames(staff,"V1","staff")
  setnames(teachers,"V1","teach_total")
  setnames(all,"hrtotlt","total_hires")
  setkey(staff,unitid)
  setkey(teachers,unitid)
  setkey(all,unitid)
  new<-staff[teachers]
  all<-all[new]
  all
}