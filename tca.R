#ICYYYY_AY
#yyyy refers to academic year, adjusted to fiscal year in filter function

#2009-2012 files are bad (no unitid), have to manually pull in the stata files and replace

tcaYears <- c(2000:2019)
tcaSourceFiles <- data.table(file = c(paste0("IC",tcaYears,"_AY")),fy = tcaYears)

tcaDownloadTables<-function(targetDir, yearVec = tcaYears ){
  download(tcaSourceFiles,yearVec,targetDir)
}

tcaReturnFields<-c("unitid","in_state_tuition","in_state_fees","out_of_state_tuition","out_of_state_fees","books_and_supplies","room_and_board","other_expenses","fiscal_year")

tcaTransformData<-function(tcaDownloadDir, years = tcaYears){ 
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