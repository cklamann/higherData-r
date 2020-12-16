#SFAyyYY
#yyYY refers to academic year, changed to fiscal in filter function
#returns financial aid information

library(data.table)
sfaYears <- fyToAy(c(2002:2015))
sfaDownloadDir<-"/home/conor/Dropbox/study/research/ipeds/full_tables/sfa/"
sfaSourceFiles <- data.table(file = c(paste0("SFA",sfaYears)),fy = sfaYears)

sfaReturnFields<-c("unitid","total_ftf","instate_ug_count","out_of_state_ug_count","average_institutional_grants","number_of_institutional_grants","pct_pell","fiscal_year")

sfaDownloadTables<-function( yearVec = sfaYears ){
  download(sfaSourceFiles,yearVec,sfaDownloadDir)
}

sfaFilterData<-function(years = sfaYears){
	sfaTable<-initializeDataTable(sfaReturnFields)
	for(n in years){
	  table<-as.data.table(read.csv(paste0(sfaDownloadDir,n,".csv"),stringsAsFactors = F))
	  table<-cleanNumericTable(table)
		table[,'fiscal_year':= ayToFy(n)]
		setnames(table, c("unitid","scfa1n","scfa12n","scfa13n","igrnt_a","igrnt_n","fgrnt_p"), c("unitid","cohort_ftf","instate_ug_count","out_of_state_ug_count","average_institutional_grants","number_of_institutional_grants","pct_pell"))	
		table<-table[,.SD, .SDcols = sfaReturnFields]
		sfaTable<-rbind(table,sfaTable)
		print(paste0("finished ",n))
	}
	sfaTable
}