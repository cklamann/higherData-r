###NOTE: problems at every turn, ended up downloading and cleaning manually
library(data.table)
#@@library(bit64)

#currently (2017) returns these variables:

#[1] "federal_pell_grant_program_amt_disb"             "academic_competitiveness_grant_program_amt_disb"
#[3] "national_smart_grant_program_amt_disb"           "teach_grant_program_amt_disb"                   
#[5] "iraq_afghanistan_service_grant_program_amt_disb"

#also note that the filter function also returns _num_disb, just not uploading them

#variable renames:  
##2007, 2010, 2011 - national_smart_program, 2009: national_smart_grant_program
##2007, 2010, 2011 - academic_competitiveness, 2009: academic_competitiveness_grant_program
##2010:2017, ecept 2009 - teach_program, 2009: teach_grant_program

fsaPellYears <- c(2001:2017)

fsaPellDownloadUrl <- "http://studentaid.gov/sites/default/files/fsawg/datacenter/library/"

fsaPellDownloadDir<-"/home/conor/higherData-r/data/fsa/pell/"

fsaPellSourceFiles <- data.table(file = c(paste0("AY",fyToAyStub2(2001:2006),"Pell.xls"),paste0("Q4",fyToAy(2007:2017),"AY.xls")),fy = fsaPellYears)

#method=libcurl allows for simultaneous downloads
fsaDownload<-function(filenames,destFolder){
  destFile <- paste0(destFolder,filenames[['fy']],".xls")
  download.file(url=paste0(fsaDownloadUrl,filenames[['file']]),destfile = destFile,mode="wb",method="libcurl")
}

downloadDL<-function(){
  apply(fsaDLSourceFiles,1,fsaPellDownloadUrl,fsaPellDownloadDir)
}

###filter function

filterFsaPellSheets<-function(directory = fsaPellDownloadDir,years = fsaPellYears){
  for(n in 1:length(years)){
    filePath <- paste0(directory,years[n], ".csv")
    res<-fread(filePath)
    res[,fiscal_year:=years[n]]
    res<-lapply(res,as.integer64)
    setDT(res)
    res<-melt.data.table(res,id.vars=c('opeid','fiscal_year'))
    res[,opeid:=as.numeric(opeid)]
    res[,variable:=as.character(variable)]
    res[variable == "national_smart_program_amt_disb", variable:= "national_smart_grant_program_amt_disb"]
    res[variable == "academic_competitiveness_amt_disb", variable:= "academic_competitiveness_grant_program_amt_disb"]
    res[variable == "teach_program_amt_disb", variable:= "teach_grant_program_amt_disb"]
      if(!exists('melted')){
        melted<-res
      } else {
        melted<-rbind(melted,res)
      }
  }
  melted<-melted[!is.na(value) & !is.na(opeid)]
  melted<-melted[value>0] 
  melted
}

