#contains generic functions for downloading and cleaning data for IPEDS ONLY

#table download functions
##see admissionsPulldown.R for sample call
download<-function(sourceFiles, years, destFolder){
  sourceFiles<-sourceFiles[fy %in% years]
  downloadfileNames<-makeDownloadFileNames(sourceFiles)
  unzipFileNames<-makeUnzipFileNames(sourceFiles)
  fileNames<-data.frame(years = years, download = downloadfileNames,unzip = unzipFileNames)
  apply(fileNames,1,getRemoteFiles,destFolder)
}

getRemoteFiles<-function(fileNameVec,destFolder){
  downloadDir<-"https://nces.ed.gov/ipeds/datacenter/data/"
  temp <- tempfile()
  download.file(paste0(downloadDir,fileNameVec[['unzip']]), temp)
  str(temp)
  data <- read.csv(unz(temp,fileNameVec[['download']]))
  unlink(temp)
  write.csv(data, file = paste0(destFolder,fileNameVec['years'],".csv"), row.names=F)		
}
  
makeUnzipFileNames<-function( sourceFiles ){
  files<-paste0(sourceFiles[,file],".zip")
  files
}

makeDownloadFileNames<-function( sourceFiles ){
  files<-tolower(sourceFiles[,file])
  files<-paste0(files,".csv")
  files
}

ayToFy <- function(yearVec){
  springYear <- as.numeric(substr(yearVec,3,4))
  springYear <- ifelse(springYear < 10, paste0("0",springYear),springYear)
  year <- paste0("20",springYear)
  year
}

fyToAy <-function(yearVec){
  springYear <- as.numeric(substr(yearVec,3,4))
  fallYear <- springYear - 1
  springYear<-ifelse(springYear < 10, paste0("0",springYear),springYear)
  fallYear <- ifelse(fallYear < 10,paste0("0",fallYear),fallYear)
  year<-paste0(fallYear, springYear)
  year
}

fyToAyStub2 <-function(yearVec){
  #given 2000, returns 2000-01
  springYear <- substr(yearVec,3,4)
  fallYear <- yearVec - 1
  year<-paste0(fallYear,'-', springYear)
  year
}

fyToAyFull <-function(yearVec,separator){
  springYear <- as.numeric(substr(yearVec,3,4))
  fallYear <- springYear - 1
  springYear<-ifelse(springYear < 10, paste0("0",springYear),springYear)
  fallYear <- ifelse(fallYear < 10,paste0("0",fallYear),fallYear)
  year<-paste0("20",fallYear,separator,"20",springYear)
  year
}

