#CYYYY_A
#yyyy refers to all awards given out in fiscal year YYYY
#crace24 becomes ctotalt in 2007
#this counts total degrees -- both first and second majors
#includes 2 and 4 year schools -- 2 year is associates, 4 is bachelors

#sectors 
#4year -> [1 = public, 2 = private, 3 = for-profit] 
#2year -> [4 = public, 5 = private, 6 = for-profit]

compYears <- c(2002:2016)
compDownloadDir<-"/home/conor/Dropbox/study/research/ipeds/full_tables/completions/"
compSourceFiles <- data.table(file = c(paste0("C",compYears,"_A")),fy = compYears)
rosterFile<-"/home/conor/Desktop/hd2015.csv"
cipMap<-fread("/home/conor/Dropbox/study/scripts_and_notes/cipMap.csv")

compReturnValues <- c("education","engineering","computer_science","english","fine_arts","economics","biology") 

compDownloadTables<-function( yearVec = compYears ){
  download(compSourceFiles,yearVec,compDownloadDir)
}

#todo: put cipMap in db, add variable_name to map, since that's what's gonna get pushed
#also, for future -- FILL OUT cipMap for complete coverage
compFilterData<-function(years = compYears){ 
  roster<-namesToLower(fread(rosterFile))
  awlevelLookup<-roster[,awlevel:=ifelse(sector %in% c(1,2,3),5,ifelse(sector %in% c(4,5,6),3,NA))][,.(unitid,awlevel)]
  for(n in compYears) {
    table<-as.data.table(read.csv(paste0(compDownloadDir,n,".csv"),stringsAsFactors = F))
    table<-cleanNumericTable(table)
    table<-merge(table,awlevelLookup,by=(c("unitid","awlevel")))
    if("crace24" %in% colnames(table)){	
      setnames(table, "crace24","ctotalt")		
    }
    table <- convertCips(table,cipMap)
    table<-table[,sum(ctotalt),by=.(unitid,my_cip)]
    table[,my_cip:=as.numeric(my_cip)]
    table<-merge(table,cipMap,all.x = T)
    table[,fiscal_year:=n]
    setnames(table,"V1","value")
    table<-table[,.(unitid,fiscal_year,value,field)]
    table[,variable := gsub(' ', '_',field)] 
    table[,variable := tolower(variable)]
    table[,field:=NULL]
   # table<-table[variable %in% compReturnValues]
    if(exists('compTable')){
      compTable<-rbind(table,compTable)
    } else compTable <-table
    print(paste0("fiished ",n))
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