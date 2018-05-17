#Problems with 2009:2013, no heading for unitid imported, interprets them as rownames and omits if row.names=FALSE. Names also shifted to the right.
#Now seems as if there's a unitid heading but no unitids.... what the hell?

#ICYYYY_AY
#yyyy refers to academic year, adjusted to fiscal year in filter function
tcaYears <- c(2000:2016)
tcaDownloadDir<-"/home/conor/Dropbox/study/research/ipeds/full_tables/tca/"
tcaSourceFiles <- data.table(file = c(paste0("IC",tcaYears,"_AY")),fy = tcaYears)

tcaDownloadTables<-function( yearVec = tcaYears ){
  stop("will this overwrite 2009-2012, cause the new data for those years is bad and can't be recovered!")
  download(tcaSourceFiles,yearVec,tcaDownloadDir)
}

tcaReturnFields<-c("unitid","in_state_tuition","in_state_fees","out_of_state_tuition","out_of_state_fees","books_and_supplies","room_and_board","other_expenses","fiscal_year")

tcaFilterData<-function(years = tcaYears){ 
  tcaTable<-initializeDataTable(tcaReturnFields)
  for(n in years) {
    table<-as.data.table(read.csv(paste0(tcaDownloadDir,n,".csv"),stringsAsFactors = F))
    table<-cleanNumericTable(table)
    if("v1" %in% names(table) | "x" %in% names(table)){
      newNames<-names(table)[2:length(names(table))]
      newNames<-c(newNames,"")
      setnames(table,names(table),newNames)
    }
    setnames(table,c("tuition2","fee2","tuition3","fee3","chg4ay3","chg5ay3","chg6ay3"),c("in_state_tuition","in_state_fees","out_of_state_tuition","out_of_state_fees","books_and_supplies","room_and_board","other_expenses"))
    table[,fiscal_year:=n+1]
    table<-table[,.SD, .SDcols = tcaReturnFields]
    tcaTable<-rbind(table,tcaTable)
    print(paste0("finished",n))
  }
  tcaTable
}