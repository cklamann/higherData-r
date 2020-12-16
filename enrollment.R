#fte -- pulls from the "enrollment_by_race" table, see enrollment_by_race.R to download data
#can comment out line below to bring in only undergrads, but have to be careful to change save directory as well or
	#the old values will be overwritten

filter_enrollment_table<-function(){ 
  source_directory<-"/home/conor/Desktop/"
  value<-formatYears(1980)
  dest_directory<-"/home/conor/Desktop/"
  filter_file<-as.data.table(read.csv("/home/conor/Desktop/new_colleges.csv",stringsAsFactors=FALSE))
  for(n in 1:length(value)){
    filteredTable <- as.data.table(read.csv((paste(source_directory,"ef1980_a",".csv", sep="")),stringsAsFactors=FALSE))
    dest_file<-paste(value[n],"_f.csv", sep="")
    names(filteredTable)<-tolower(names(filteredTable))
    if("eftotlt" %in% colnames(filteredTable)){
      setnames(filteredTable,"eftotlt","efrace24")
    }
    fall_year<-paste("20",value[n],sep="")
    filteredTable[,fall_year:=fall_year]
    filteredTable<-filteredTable[efalevel %in% c(4,21,22,32,42,52),.(fall_year,unitid,efalevel,efrace24)]
    filteredTable<-dcast.data.table(filteredTable,unitid+fall_year~efalevel,fill=NA) 
    setnames(filteredTable,c("4","21","22","32","42","52"),c("ftf_total","ft_total","ft_ug","ft_g","pt_ug","pt_g"))
    filteredTable[,c("ft_g","pt_g"):=0] #if you want to filter out grads uncomment this
    filteredTable<-as.data.table(merge(filteredTable, filter_file,by="unitid"))
    fte_private<-as.data.table(filteredTable[type=="Private NPO",.(unitid,"fte"=plus(plus((pt_ug*.392857),(pt_g*.382059)),plus(ft_ug,ft_g)))])
    fte_public<-as.data.table(filteredTable[type=="Public",.(unitid,"fte"=plus(plus((pt_ug*.403543),(pt_g*.361702)),plus(ft_ug,ft_g)))])
    ftes<-rbind(fte_private,fte_public)
    filteredTable<-merge(ftes,filteredTable,by="unitid")
    filteredTable[,c("OPEID","type","INSTNM","ft_g","pt_g"):=NULL]
    write.csv(filteredTable,paste0(dest_directory,dest_file))
    print(head(filteredTable))
  }
  #filteredTable
}
  
  merge_enrollment_tables<-function(){
    value<-formatYears(2:14)
    directory<-"C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/fte_ug_only/"
    frame <- data.frame()
    for(i in 1:(length(value))){
      frame <- rbind(frame, read.csv(paste(directory, value[i], "_f.csv", sep=""))) 	
    }
    write.csv(frame, file=(paste(directory,"big_one.csv", sep="")), row.names=FALSE)
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
  
  