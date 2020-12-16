#EAPyyyy
#yyyy refers to academic year, adjusted to fiscal year in filter function
#2010 has unstandard file name (all caps) -- i downloaded it manually

library(data.table)
eapYears <- c(2002:2016)
eapDownloadDir<-"/home/conor/higherData-r/data/ipeds/eap/"
eapSourceFiles <- data.table(file = c(paste0("EAP",eapYears)),fy = eapYears)

#renaming maps:
#eaprectp becomes eapcat after 2011
#eapcat has different codes than eaprectp:

#eapcat | eaprectp
#-----------------
#10000  |  1100	 
#10020  |  1102
#10030  |  1103
#40000  |  1106
#21143  |  3110

eapDownloadTables<-function( yearVec = eapYears ){
  download(eapSourceFiles,yearVec,eapDownloadDir)
}

eapReturnFields<-c("unitid","all_employees","all_tenured_employees","on_tenure_track","grad_assistants", "fiscal_year", "part_time_instructors")

eapFilterData<-function(years = eapYears){ #run this to get the tenured, total, and TAs
	eapTable<-initializeDataTable(eapReturnFields)
  for(n in years) {
    table<-as.data.table(read.csv(paste0(eapDownloadDir,n,".csv"),stringsAsFactors = F))
    table<-cleanNumericTable(table)
    if(n<2012){
	    setnames(table,"eaprectp","eapcat")
	  }
  	table<-dcast.data.table(table, unitid~eapcat, value.var="eaptyp", fill=as.integer(NA))
  	setnames(table,names(table),as.character(names(table)))
  	if(n<2012){
  	  setnames(table,c("1100","1102","1103","1106","3110"),c("10000","10020","10030","40000","21143"))
  	}
  	setnames(table, c("10000","10020","10030","40000","21143"), c("all_employees","all_tenured_employees","on_tenure_track","grad_assistants","part_time_instructors"))		
	  table[,fiscal_year:=n+1]
	  table<-table[,.SD, .SDcols = eapReturnFields]
	  eapTable<-rbind(table,eapTable)
	  print(paste0("finished", n+1))
  }
	eapTable
}