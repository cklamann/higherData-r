###NOTE: 2007 is missing a header row for both fl and dl -- i just inserted the extra row manually on these files
###NOTE: coercing opeid to numeric, do the same for lookup table (removes leading zeroes)

#currently (2017) returns these variables: 

#[1] "dl_subsidized_undergraduate_amt_disb"   "dl_unsubsidized_undergraduate_amt_disb"
#[3] "dl_parent_plus_amt_disb"                "dl_grad_plus_amt_disb"                 
#[5] "dl_subsidized_graduate_amt_disb"        "dl_unsubsidized_graduate_amt_disb"     
#[7] "ffel_subsidized_amt_disb"               "ffel_unsubsidized_amt_disb"            
#[9] "ffel_parent_plus_amt_disb"              "ffel_grad_plus_amt_disb"  

#also note that the filter function also returns _num_disb, just not uploading them

##variable changes:
##ffel: ffel_plus_amt_disb becomes ffel_parent_plus_amt_disb beginning fy 2007
##dl: dl_plus becomes dl_parent_plus beginning fy 2007
## dl_unsubsidized_amt_disb becomes dl_unsubsidized_undergraduate_amt_disb starting fy 2011
## dl_subsidized_amt_disb becomes dl_subsidized_undergraduate_amt_disb for fy 2011 and 2012

##!!!!note that this is because they only started keeping track of ug vs g loans in 2011! -- only change plus, keep others....

##working sample commands:
#dls<-filterFsaSheets(fsaDLDownloadDir,fsaYears)
#ffels<-filterFsaSheets(fsaFFELDownloadDir,fsaYears)

library(readxl)
library(data.table)

fsaYears <- 2001:2017
fsaDLYears<-2001:2017
fsaFFELYears<-2001:2010


#format years for remote filename
fsaDownloadyears<-fyToAyFull(fsaDLYears,'_')

fsaDownloadUrl <- "http://studentaid.gov/sites/default/files/fsawg/datacenter/library/"

fsaDownloadDir<-"/home/conor/Dropbox/study/research/fsa/"
fsaDLDownloadDir<-paste0(fsaDownloadDir,"dl/")
fsaFFELDownloadDir<-paste0(fsaDownloadDir,"ffel/")
fsaPellDownloadDir<-paste0(fsaDownloadDir,"pell/")

fsaDLSourceFiles <- data.table(file = c(paste0("DL_AwardYr_Summary_AY",fyToAyFull(2001:2006,'_'),"_All.xls"),paste0("DL_Dashboard_AY",fyToAyFull(2007:2017),"_Q4.xls")),fy = fsaDLYears)
fsaFFELSourceFiles <- data.table(file = c(paste0("FFEL_AwardYr_Summary_AY",fyToAyFull(2001:2006,'_'),"_All.xls"),paste0("FL_Dashboard_AY",fyToAyFull(2007:2010),"_Q4.xls")),fy = fsaFFELYears)

#method=libcurl allows for simultaneous downloads
fsaDownload<-function(filenames,destFolder){
  destFile <- paste0(destFolder,filenames[['fy']],".xls")
  download.file(url=paste0(fsaDownloadUrl,filenames[['file']]),destfile = destFile,mode="wb",method="libcurl")
}

downloadDL<-function(){
  apply(fsaDLSourceFiles,1,fsaDownload,fsaDLDownloadDir)
}

downloadFFEL<-function(){
  apply(fsaFFELSourceFiles,1,fsaDownload,fsaFFELDownloadDir)
}

###filter functions -- filterFsaSheets is only public

filterFsaSheets<-function(directory,years){
  filePath <- paste0(directory,years, ".xls")
  config<-data.table(path=filePath,fy=years)
  res<-apply(config,1,formatFsaLoans) 
  res<-lapply(res,melt.data.table,id.vars=c('opeid','fiscal_year'))
  res<-do.call(rbind,res)
  setDT(res)
  res[,opeid:=as.numeric(opeid)]
  res[variable=="ffel_plus_amt_disb",variable:="ffel_parent_plus_amt_disb"]
  res[variable=="dl_plus_amt_disb", variable:="dl_parent_plus_amt_disb"]
  res[,variable:=as.character(variable)]
  res
}

#below maps the page of the full FY data to the sheet number
sheetMap<-data.table(year=c(2000:2006,2007:2017),sheet=c(rep(1,7),rep(2,11)))

formatFsaLoans<-function(config){
  whichSheet<-sheetMap[config[['fy']] == year,sheet] 
  print(paste0("reading from sheet ",whichSheet))
  categories<-getCategories(config[['path']],whichSheet)
  table<-getBody(config[['path']],categories,whichSheet)
  setDT(table)
  table[,fiscal_year:=config[['fy']]]
  table
}

getCategories<-function(filePath,whichSheet){
  sheet<-as.data.table(read_excel(filePath,sheet=whichSheet,skip=4,n_max=1))
  cats<-names(sheet)
  cats<-cats[-grep('__',cats)]
  if(length(cats) > 7){
    str(cats)
    stop(paste0("Looks like we're looking at the wrong row for category names",filePath))
  }
  cats<-tolower(cats)
  cats<- gsub("-","",cats)
  cats<- gsub(" ","_",cats)
}

getBody<-function(filePath,categories,whichSheet){
  #returns a dataframe with colnames
  sheet<-as.data.table(read_excel(filePath,sheet=whichSheet,skip=5))
  setnames(sheet,names(sheet),tolower(names(sheet)))
  #subset columns we need
  names<-names(sheet)[grep('ope|disb',names(sheet))]
  if(length(names)<2 | length(names)==0){
    str(names)
    str(filePath)
    stop("looks like we're looking at the wrong row for the column names")
  }
  sheet<-sheet[,names,with=F]
  #rename columns based on categories
  if((length(sheet)-1) != (length(categories)*2)){
    str(categories)
    str(sheet)
    stop(paste0("number of categories is not half the number of variables for ",filePath))
  }
  newNames<-as.character(sapply(categories,paste0,c("_num_disb","_amt_disb")))
  newNames<-c("opeid",newNames)
  setnames(sheet,names(sheet),newNames)  
  sheet
}