grabtable<-function(){
  #save as fiscal year!
  fileValue<-"0203"
  fy<-"03"
	download_dir<-"https://nces.ed.gov/ipeds/datacenter/data/"
	dest_dir<-"/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/private_fin/"
	temp <- tempfile()
	for(n in 1:1){
	  download_file<-paste("F",fileValue,"_F2",".zip", sep="")
	  unzipped_file<-paste("f",fileValue,"_f2",".csv", sep="")
	  dest_file<-paste(fy,".csv", sep="")
	  download.file((paste(download_dir,download_file, sep="")), temp)
	  data <- read.csv(unz(temp,unzipped_file))
        unlink(temp)
	  write.csv(data, file = (paste(dest_dir,dest_file, sep="")), row.names=FALSE)		
	}
}

filter_fin_table_priv<-function(){
	value<-"15"
	directory<-"/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/private_fin/"
	write_directory <- "/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/private_fin/filtered/"
	filter_file<-as.data.table(read.csv("/home/conor/Desktop/new_colleges.csv",stringsAsFactors=FALSE))
	filteredTable <- data.frame()
	for(i in 1:length(value)){
  	dest_file<-paste(value[i],"_full.csv", sep="")
  	filteredTable <- read.csv((paste(directory,value[i],".csv", sep="")),stringsAsFactors=FALSE)
  	filteredTable<-as.data.table(filteredTable)
  	setnames(filteredTable, names(filteredTable), tolower(names(filteredTable)))	
  	#filteredTable<-merge(filteredTable, filter_file,by="unitid")
  	filteredTable[,INSTNM:=NULL]
  	filteredTable[,OPEID:=NULL]
  	filteredTable[,type:=NULL]
  	filteredTable <- filteredTable[,.(unitid,f2d08,f2d09,f2a04,f2a06,f2b04,f2c01,f2c03,f2d01,f2d02,f2d03,f2d10,f2e011,f2e012,f2e013,f2e021,f2e022,f2e031,f2e041,f2e042,f2e051,f2e052,f2e061,f2e062,f2e081)]
  	#these are now correctly labeled -- 2/2/2017
  	names(filteredTable)[names(filteredTable)==c("unitid","f2d08","f2d09","f2a04","f2a06","f2b04","f2c01","f2c03","f2d01","f2d02","f2d03",
  								"f2d10","f2e011","f2e012","f2e013","f2e021","f2e022",
  								"f2e031","f2e041","f2e042","f2e051","f2e052","f2e061",
  								"f2e062","f2e081")] <-c("unitid","gift1","gift2","total_unrest_net_assets","total_net_assets",
  								"change_in_net_ass","pell_total","state_grants","total_tuition_and_fees",
  								"total_fed_app","state_appropriations","investment_income","inst_total","inst_wages",
  								"inst_benefits","research_total","research_salaries","public_service_total",
                                                  "ac_sup_total","ac_sup_wages","ss_total","ss_wages","inst_sup_total",
  								"inst_sup_wages", "net_grant_aid_to_students")
  	filteredTable[,gifts:=gift1+gift2]
  	filteredTable<-filteredTable[,.(unitid,gifts,total_unrest_net_assets,total_net_assets,total_tuition_and_fees,investment_income,state_appropriations)]
  	if(value[i]<10){
  	  prefix = "200"
  	} else prefix = "200"
  	year<-paste(prefix,as.numeric(value[i]) - 1, sep="")
  	#the below used to be wrong...
  	filteredTable$fall_year <- year
  	write.csv(filteredTable, file=(paste(write_directory,dest_file, sep="")), row.names=FALSE)
	}
	filteredTable
}
merge_fin_tables<-function(){
	value<-formatYears(1:13)
	directory<-write_directory <- "/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/private_fin/filtered/"
	frame <- data.frame()
	for(i in 1:(length(value))){
	  frame <- rbind(frame, read.csv(paste(directory, value[i], ".csv", sep=""))) 	
	}
	write.csv(frame, file=(paste(directory,"big_one.csv", sep="")), row.names=FALSE)
  frame
}

filter_gifts<-function(){
  value<-formatYears(15)
  directory<-"/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/private_fin/" 
  filter_file<-as.data.table(read.csv("/home/conor/Desktop/new_colleges.csv",stringsAsFactors=FALSE))
  full_table<-data.table()
  for(i in 1:length(value)){
    filteredTable <- as.data.table(read.csv((paste(directory,value[i],".csv", sep="")),stringsAsFactors=FALSE))
    names(filteredTable)<-tolower(names(filteredTable))	
    filteredTable<-merge(filteredTable, filter_file,by="unitid")
    filteredTable[,c("INSTNM","OPEID","type"):=NULL]
    filteredTable <- filteredTable[,.(unitid,(f2d08+f2d09))] #combine the 2 gift columns
    setnames(filteredTable,"V2","gifts")
    year<-paste("20",value[i]-1, sep="")
    filteredTable[,fall_year:=as.integer(year)]
    full_table<-rbind(filteredTable,full_table)
  }
  full_table
}
