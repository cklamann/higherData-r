grabtable<-function(){
	#save as fiscal year!
	fileValue<-"0001"
	fy<-"01"
	download_dir<-"https://nces.ed.gov/ipeds/datacenter/data/"
	dest_dir<-"/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/public_fin/"
	temp <- tempfile()
	for(n in 1){
	  download_file<-paste("F",fileValue,"_F1A",".zip", sep="")
	  unzipped_file<-paste("f",fileValue,"_f1a",".csv", sep="")
	  dest_file<-paste(fy,".csv", sep="")
	  download.file((paste(download_dir,download_file, sep="")), temp)
	  data <- read.csv(unz(temp,unzipped_file))
        unlink(temp)
	  write.csv(data, file = (paste(dest_dir,dest_file, sep="")), row.names=FALSE)		
	}
}

filter_fin_table<-function(){
	value<-as.character(c("15"))
	directory<-"/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/public_fin/"
	filter_file<-read.csv("/home/conor/Desktop/new_colleges.csv",stringsAsFactors=FALSE)
	filteredTable <- data.table()
	for(i in 1:length(value)){
	dest_file<-paste(value[i],"_full.csv", sep="")
	filteredTable <- read.csv((paste(directory,value[i],".csv", sep="")),stringsAsFactors=FALSE)
	library('data.table')
	filteredTable<-as.data.table(filteredTable)   
	names(filteredTable)<-tolower(names(filteredTable))	
	#filteredTable<-merge(filteredTable, filter_file,by="unitid")
	filteredTable[,OPEID:=NULL]
	filteredTable[,INSTNM:=NULL]
	filteredTable[,type:=NULL]

	names(filteredTable)[names(filteredTable)=="f1a06"] <- "total_assets"
	names(filteredTable)[names(filteredTable)=="f1a17"] <- "total_unrest_net_assets"
	names(filteredTable)[names(filteredTable)=="f1a18"] <- "total_net_assets"
	names(filteredTable)[names(filteredTable)=="f1b01"] <- "total_tuition_and_fees"
	names(filteredTable)[names(filteredTable)=="f1b10"] <- "fed_appropriations"
	names(filteredTable)[names(filteredTable)=="f1b11"] <- "state_appropriations"
	names(filteredTable)[names(filteredTable)=="f1b12"] <- "local_appropriations"
	names(filteredTable)[names(filteredTable)=="f1b17"] <- "investment_income"
	names(filteredTable)[names(filteredTable)=="f1b22"] <- "additions_to_perm_endow"
	names(filteredTable)[names(filteredTable)=="f1c011"] <- "inst_total"
	names(filteredTable)[names(filteredTable)=="f1c012"] <- "inst_wages"
	names(filteredTable)[names(filteredTable)=="f1c021"] <- "research_total"
	names(filteredTable)[names(filteredTable)=="f1c031"] <- "public_service_total"
	names(filteredTable)[names(filteredTable)=="f1c051"] <- "ac_sup_total"
	names(filteredTable)[names(filteredTable)=="f1c052"] <- "ac_sup_wages"
	names(filteredTable)[names(filteredTable)=="f1c061"] <- "ss_total"
	names(filteredTable)[names(filteredTable)=="f1c062"] <- "ss_wages"
	names(filteredTable)[names(filteredTable)=="f1c071"] <- "inst_sup_total"
	names(filteredTable)[names(filteredTable)=="f1c072"] <- "inst_sup_wages"
	names(filteredTable)[names(filteredTable)=="f1d03"] <- "net_asset_increase"
	names(filteredTable)[names(filteredTable)=="f1e03"] <- "pell_total"
	names(filteredTable)[names(filteredTable)=="f1e05"] <- "rest_inst_grants"
	names(filteredTable)[names(filteredTable)=="f1e06"] <- "unrest_inst_grants"
	names(filteredTable)[names(filteredTable)=="f1e08"] <- "discounts_to_tuition"
	giftColumns<-c("f1b15","f1b16","f1b18","f1b21","f1b22","f1b24");
	realGiftColumns <- match(names(filteredTable), giftColumns) 
	giftColumns<-giftColumns[realGiftColumns[!is.na(realGiftColumns)]]
	gift <- 0
	for(gifter in giftColumns){
    gift = gift + filteredTable[[gifter]]	  
	}
	filteredTable[,gifts:=gift]
	filteredTable<-filteredTable[,c("unitid","total_assets","total_unrest_net_assets","total_net_assets","total_tuition_and_fees",
		"state_appropriations","investment_income","gifts"),with=FALSE]
	if(value[i]<10){
	  prefix = "200"
	} else prefix = "200"
	year<-paste(prefix,as.numeric(value[i])-1, sep="") #this used to be wrong
	filteredTable$fall_year <- year
	write.csv(filteredTable, file=(paste(directory,dest_file, sep="")), row.names=FALSE)
	}
	filteredTable
}

merge_fin_tables<-function(){
	value<-formatYears(9:13)
	directory<- "C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/public_fin/"
	frame <- data.frame()
	for(i in 1:(length(value))){
	  frame <- rbind(frame, read.csv(paste(directory, value[i], "_f.csv", sep=""))) 	
	}
	write.csv(frame, file=(paste(directory,"big_one.csv", sep="")), row.names=FALSE)
  frame
}

filter_gifts<-function(){
  value<-formatYears(15)
  directory<-"/home/conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/public_fin/" 
  filter_file<-as.data.table(read.csv("/home/conor/Desktop/new_colleges.csv",stringsAsFactors=FALSE))
  full_table<-data.table()
  for(i in 1:length(value)){
    filteredTable <- as.data.table(read.csv((paste(directory,value[i],".csv", sep="")),stringsAsFactors=FALSE))
    names(filteredTable)<-tolower(names(filteredTable))	
    filteredTable<-merge(filteredTable, filter_file,by="unitid")
    filteredTable[,c("instnm","opeid","type"):=NULL]
    filteredTable <- filteredTable[,.(unitid,(f1b15+f1b16+f1b18+f1b21+f1b22+f1b24))] #combine the gift columns
    setnames(filteredTable,"V2","gifts")
    year<-paste("20",as.numeric(value[i])-1, sep="")
    filteredTable[,fall_year:=as.integer(year)]
    full_table<-rbind(filteredTable,full_table)
  }
  full_table
}

