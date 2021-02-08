##new plan --> open up another mongo table for this, call it fsa, get name, etc from crosswalk and store
##don't bother matching with unitids, because it won't work....

###NOTE: coercing opeid to numeric, do the same for lookup table (removes leading zeroes)

#[7] "ffel_subsidized_amt_disb"               "ffel_unsubsidized_amt_disb"            
#[9] "ffel_parent_plus_amt_disb"              "ffel_grad_plus_amt_disb"  

##variable changes:
##ffel: ffel_plus_amt_disb becomes ffel_parent_plus_amt_disb beginning fy 2007
##dl: dl_plus becomes dl_parent_plus beginning fy 2007 (b/c grad plus introduced in 2006)

##!!!!note that the below are because they only started keeping track of ug vs g loans in 2011! -- only change plus, keep others....
## dl_unsubsidized_amt_disb becomes dl_unsubsidized_undergraduate_amt_disb starting fy 2011
## dl_subsidized_amt_disb becomes dl_subsidized_undergraduate_amt_disb for fy 2011 and 2012



dl_return_field_map <- list("dl.subsidized_amt_disb" =  "dl_sub_all",          
                            "dl.subsidized.graduate_amt_disb" = "dl_sub_g" ,
                            "dl.subsidized.undergraduate_amt_disb" = "dl_sub_u",
                            "dl.unsubsidized.graduate_amt_disb" = "dl_unsub_g",      
                            "dl.unsubsidized_amt_disb" = "dl_unsub_all",           
                            "dl.unsubsidized.undergraduate_amt_disb" ="dl_unsub_u",
                            "dl.plus_amt_disb" = "dl_plus_u",                
                            "dl.parent.plus_amt_disb" = "dl_plus_u",                
                            "dl.grad.plus_amt_disb" = "dl_plus_g"                 
                       )

ffel_return_field_map <- list("ffel.subsidized_amt_disb" = "ffel_sub",   
                            "ffel.unsubsidized_amt_disb" = "ffel_unsub",
                            "ffel.plus_amt_disb" = "ffel_plus_u",         
                            "ffel.grad.plus_amt_disb" = "ffel_plus_g",   
                            "ffel.parent.plus_amt_disb" = "ffel_plus_u")

##working sample commands:
#dls<-filterFsaSheets(fsaDLDownloadDir,fsaYears)
#ffels<-filterFsaSheets(fsaFFELDownloadDir,fsaYears)

#pell name changes: [swap out num for amt for nums] --> we always take the later!
# academic.competitiveness._amt_disb 2007,2008, 2010
# academic.competitiveness.grant.program_amt_disb 2009
# national.smart.program_amt_disb 2007,2008, 2010
# national.smart.grant.program_amt_disb 2009
# teach.grant.program_amt_disb 2009
# teach.program_amt_disb 2010:2017

library(data.table)
library(xlsx)

fsaYears <- 2001:2020
fsaDLYears<-2001:2020
fsaFFELYears<-2001:2010
fsaPellYears<-2001:2020


#format years for remote filename
fsaDownloadyears<-fyToAyFull(fsaDLYears,'_')

fsaDownloadUrl <- "https://studentaid.gov/sites/default/files/"

#this is the new url and quarterly counts are not cumulative!! (after 2005-6)
"https://studentaid.gov/sites/default/files/fsawg/datacenter/library/dl-dashboard-ay2020-2021-q1.xls"

#https://studentaid.gov/sites/default/files/DL_AwardYr_Summary_AY2000_2001_All.xls
#https://studentaid.gov/sites/default/files/DL_Dashboard_AY2006_2007_Q1.xls
#https://studentaid.gov/sites/default/files/fsawg/datacenter/library/DL_Dashboard_AY2014_2015_Q4.xls
#https://studentaid.gov/sites/default/files/fsawg/datacenter/library/dl-dashboard-ay2018-2019-q4.xls
convertToQuarter <-function(arg){
  paste0(arg, paste0("-q", 1:4), '.xls')
}

convertToQuarterUnderscore <-function(arg){
  paste0(arg, paste0("_Q", 1:4), '.xls')
}

fsaDLSourceFiles2000To2006 = paste0("DL_AwardYr_Summary_AY",paste0(fyToAyFull(2001:2006,'_'), '_All.xls'))
fsaDLSourceFiles2007To2015 = as.matrix(as.list(apply(as.matrix(paste0("DL_Dashboard_AY",paste0(fyToAyFull(2007:2014,'_')))), 1, FUN=convertToQuarterUnderscore)))
fsaDLSourceFiles2016To2018 = as.matrix(as.list(apply(as.matrix(paste0("fsawg/datacenter/library/DL_Dashboard_AY",paste0(fyToAyFull(2015:2018,'_')))), 1, FUN=convertToQuarterUnderscore)))
fsaDLSourceFiles2019Plus = as.matrix(as.list(apply(as.matrix(paste0("fsawg/datacenter/library/dl-dashboard-ay",paste0(fyToAyFull(2019:2021,'-')))), 1, FUN=convertToQuarter)))
fsaDLSourceFiles = as.matrix(c(fsaDLSourceFiles2016To2018, fsaDLSourceFiles2019Plus))
fsaFFELSourceFiles2000To2005 <- as.matrix(paste0("FFEL_AwardYr_Summary_AY",fyToAyFull(2001:2006,'_'),"_All.xls"))
fsaFFELSourceFiles2006To2010 <- as.matrix(as.list(apply(as.matrix(paste0("FL_Dashboard_AY",paste0(fyToAyFull(2007:2010,'_')))), 1, FUN=convertToQuarterUnderscore)))
fsaFFELSourceFiles <- as.matrix(c(fsaFFELSourceFiles2000To2005,fsaFFELSourceFiles2006To2010))

fsaPellSourceFiles <- data.table(file = c(paste0("AY",fyToAyStub2(2001:2006, '-'),"Pell.xls"),paste0("Q4",fyToAy(2007:2020),"AY.xls")),fy = fsaPellYears)


fsaDownload<-function(filenames,destFolder){
  splt <- strsplit(filenames[[1]], '/')[[1]] 
  destFile <- paste0(destFolder,splt[length(splt)]) 
  download(url=paste0(fsaDownloadUrl,filenames['file']),destfile = destFile,mode="wb")
}

downloadDL<-function(targetDir){
  apply(fsaDLSourceFiles,1,fsaDownload,targetDir)
}

downloadFFEL<-function(targetDir){
  apply(fsaFFELSourceFiles,1,fsaDownload,targetDir)
}

downloadPell<-function(targetDir){
  apply(fsaPellSourceFiles,1,fsaDownload,targetDir)
}

getName<-function(n,l){
  l[[n]]  
}


###filter functions -- filterFsaSheets is only public
#todo: move into Python

#get crosswalk with getCrosswalk function....
transformFsaSheets<-function(directory, years, nameMap, crosswalk, extension = ".xls"){
  filePath <- paste0(directory,years, extension)
  config<-data.table(path=filePath,fy=years)
  res<-apply(config,1,formatFsaFile) 
  #melt so we can merge
  res<-lapply(res,melt.data.table,id.vars=c('opeid','fiscal_year'))
  res<-do.call(rbind,res)
  setDT(res)
  #drop extra dots
  res[,variable:= gsub('\\.{2,}',".", variable)]
  #filter fields
  res <- res[variable %in% names(nameMap)]
  res[,variable := sapply(variable, getName, nameMap)]
  #cast back to standard format
  res <- dcast.data.table(res, fiscal_year + opeid~variable, value.var = "value",fun=identity,fill=NA)
  res <- merge(res,crosswalk, by="opeid")
  res[,opeid:=NULL]
}

formatFsaFile<-function(config){
  sheet<-.getCumulativeSheet(config[['path']])
  bodyStartRow <- .getBodyStartRow(config[['path']], sheet) + 1
  categoryStartRow <- bodyStartRow - 1
  categories<-getCategories(config[['path']],sheet,categoryStartRow)
  table<-getBody(config[['path']],categories,sheet,bodyStartRow)
  setDT(table)
  table[,fiscal_year:=config[['fy']]]
  print(config[['fy']])
  table
}

.getCumulativeSheet <-function(path, sheet = 1) {
  rows <- read.xlsx2(file=path,sheetIndex=sheet,startRow=1,endRow=3,header=F, as.data.frame=F)
  rows = paste0(rows)
  found = F
  if(found == F){
    for(row in rows){
      if(isTRUE(grep('Cumulative', row) > 0)){
        found = T
        break
      }
    }
    if(found == F){
      sheet <- .getCumulativeSheet(path, sheet + 1)  
    }
  }
  sheet
}

.getBodyStartRow <- function(path,sheet,startRow=2) {
  row <- read.xlsx2(path,sheet,NULL,startRow,NULL,startRow+1,F)
  if(!isTRUE(grep('OPE',row) > 0)){
    startRow <- .getBodyStartRow(path, sheet, startRow + 1)
  }
  startRow
}

getCategories<-function(filePath,whichSheet, startRow){
  sheet<-as.data.table(read.xlsx2(filePath,sheetIndex=whichSheet,startRow = startRow))
  cats<-names(sheet)
  cats<-cats[-grep('X.',cats)]
  if(length(cats) > 7){
    str(cats)
    stop(paste0("Looks like we're looking at the wrong row for category names",filePath))
  }
  cats<-tolower(cats)
  cats<- gsub("-","",cats)
  cats<- gsub(" ","_",cats)
}

getBody<-function(filePath,categories,whichSheet, startRow){
  #returns a dataframe with colnames
  sheet<-as.data.table(read.xlsx2(filePath,sheetIndex=whichSheet,startRow = startRow)) 
  
  setnames(sheet,names(sheet),tolower(names(sheet)))
  #subset columns we need
  names<-names(sheet)[grep('ope|disb',names(sheet))] #pell needs rec, remove for loans...
  if(length(names)<2 | length(names)==0){
    str(names)
    str(filePath)
    stop("looks like we're looking at the wrong row for the column names... is 'rec' in the regex? ")
  }
  sheet<-sheet[,..names]
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