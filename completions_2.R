#CYYYY_A
#yyyy refers to all awards given out in fiscal year YYYY
#crace24 becomes ctotalt in 2007
#this counts total degrees -- both first and second majors

#tmp notes for upload -- run this, then convert all besides value to string, then upload

compYears <- c(2002:2016)
compDownloadDir<-"/home/conor/Dropbox/study/research/ipeds/full_tables/completions/"
#compDownloadDir<-"C:/Users/cklamann/Desktop/completions/"
compSourceFiles <- data.table(file = c(paste0("C",compYears,"_A")),fy = compYears)
rosterFile<-"/home/conor/Desktop/hd2015.csv"
#cipMap<-fread("/home/conor/Dropbox/study/scripts_and_notes/cipMap.csv")

compReturnFields <- c("unitid","fiscal_year","awlevel","cipcode","total_degrees") 

compDownloadTables<-function( yearVec = compYears ){
  download(compSourceFiles,yearVec,compDownloadDir)
}

#todo: put cipMap (from latest completions dictionary) in db
compFilterData<-function(years = compYears){ 
  compTable<-initializeDataTable(compReturnFields)
  for(n in compYears) {
    table<-as.data.table(read.csv(paste0(compDownloadDir,n,".csv"),stringsAsFactors = F))
    table<-cleanNumericTable(table)
    if("crace24" %in% colnames(table)){	
      setnames(table, "crace24","ctotalt")		
    }
    setnames(table,"ctotalt","total_degrees")

    table[,majornum:=NULL]
    table[,lapply(.SD,sum),by=c("unitid","cipcode","awlevel")]
    table[,fiscal_year:=n]
    table <- table[,compReturnFields,with=FALSE]
    print(paste0("finished ",n))
    compTable<-rbind(compTable,table)
  }
  compTable
}

convertCips<-function(table,cipMap){
  longCipMap <- expandCipMap(cipMap);
  table<-merge(table,longCipMap,by='cipcode')
  table
}

expandCipMap <- function(cipMap){
  longMap<-apply(cipMap,1, function(x){ data.table('cipcode' = ((as.numeric(x['cip_start']) * 10000) : (as.numeric(x['cip_end']) *10000)),'my_cip'=x['my_cip'])})
  res <- do.call(rbind,longMap)
  res[,cipcode:= (cipcode/10000)]
  res
}