#note that these all have to be pulled down and cleaned manually. Hence the two
#separate directories -- one for the raw excels, the other for the csvs that have
#been hand-cleaned. Also note that the quarterly files contain a running total sheet attached.

grab_fsa_table<-function(){
  value<-as.character(c("16"))
  download_dir<-"https://studentaid.ed.gov/sites/default/files/fsawg/datacenter/library/FL_Dashboard_AY"
  dest_dir<-"/home/conor/Desktop/"
  for(n in 1:(length(value))){
    download_year<-paste(20,value[n],"_",20,value[n+1],sep="")
    #dest_file<-paste(value[n],value[n+1],".xls",sep="")
    #download.file(paste(download_dir,download_file, sep=""),paste(dest_dir,dest_file,sep=""),mode="wb")
    for(i in 1:4){
      download_file<-paste(download_dir,download_year,"_Q",i,".xls", sep="")
      dest_file<-paste(value[n],value[n+1],"q",i,".xls", sep="")
      download.file(download_file, paste(dest_dir,dest_file,sep=""), mode="wb")
    }
  
  }
}

merge_fsa_quarter_tables<-function(){
  library('data.table')
  value<-as.character(c("15"))
  directory<-"/home/conor/Desktop/1516/"
  for(i in 1:length(value)){
    curr_table<-data.frame()
    for(n in 1:4){
      curr_table<-rbind(curr_table, read.csv(paste(directory,value[i],value[i+1],"q",n,".csv", sep=""),stringsAsFactors=FALSE))
      #curr_table<-read.csv(paste(directory,value[i],value[i+1],"q",n,".csv", sep=""),stringsAsFactors=FALSE)
      print(names(curr_table))
    }  
  curr_table<-curr_table[,c(-2,-3,-4,-5)]
  curr_table<-lapply(curr_table, function(x) gsub("[,$]","",x))
  curr_table<-lapply(curr_table,as.numeric)
  curr_table<-as.data.table(curr_table)
  str(curr_table)
  curr_table[is.na(curr_table)]<-0
  curr_table<-curr_table[,lapply(.SD,sum),by="OPE.ID"]
  curr_table<-merge(curr_table, read.csv("/home/conor/Desktop/new_colleges.csv"), by="opeid")
  fall_year<-as.numeric(paste("20",value[i],sep=""))
  curr_table[,fall_year:=fall_year] 
  write.csv(curr_table, file=paste("/home/conor/Dropbox/study/research/fsa/dl/",value[i],value[i+1],".csv",sep=""),row.names=FALSE)
  }
}

fix_years<-function(){ #clean up the tables that reported annual amounts only
  library('data.table')
  value<-as.numeric(c("15"))
  directory<-"/home/conor/Dropbox/study/research/fsa/dl/"
  for(i in 1:length(value)){
    curr_table<-fread(paste(directory,value[i],value[i]+1,".csv",sep=""))
    curr_table<-curr_table[,c(-2,-3,-4,-5)]
    curr_table<-lapply(curr_table, function(x) gsub("[,$]","",x))
    curr_table<-lapply(curr_table,as.numeric)
    curr_table<-as.data.table(curr_table)
    curr_table<-merge(curr_table, read.csv("/home/conor/Desktop/crosswalk.csv"), by="opeid")
    fall_year<-as.numeric(paste("20",value[i],sep=""))
    curr_table<-as.data.table(curr_table)
    curr_table[,fall_year:=fall_year] 
    print(head(curr_table))
    write.csv(curr_table, file=paste("/home/conor/Dropbox/study/research/fsa/dl/",value[i],".csv",sep=""),row.names=FALSE)
  }
}

merge_fsa_year_tables<-function(){
  value<-as.character(c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15"))
  directory<-"C:/Users/Conor/Dropbox/study/research/fsa/dl/"
  frame <- data.frame()
  for(i in 1:(length(value)-1)){
    frame <- rbind(frame, read.csv(paste(directory, value[i],value[i+1],".csv", sep=""))) 	
    print(value[i])
  }
  frame[is.na(frame)]<-0
  write.csv(frame, file=(paste(directory,"big_one.csv", sep="")), row.names=FALSE)
}

merge_big_ones<-function(){
  fl<-"C:/Users/Conor/Dropbox/study/research/fsa/fl/big_one_fl.csv"
  dl<-"C:/Users/Conor/Dropbox/study/research/fsa/dl/big_one.csv"
  frame <- data.frame()
  frame <- rbind(frame, read.csv(fl))
  frame <- rbind(frame, read.csv(dl),by="unitid")
  frame<-as.data.table(frame)
  frame<-frame[,lapply(frame,as.numeric)]
  options(datatable.optimize=1)
  frame<-frame[,lapply(.SD,sum),by=list(unitid,fall_year)]
  str(frame)
  frame<-merge(frame, read.csv("C:/Users/Conor/Desktop/colleges.csv"),by="unitid")
  write.csv(frame, file="C:/Users/Conor/Dropbox/study/research/fsa/big_big_one.csv", row.names=FALSE)
}

cleanFL<-function(dataTable){
  new_table<-as.data.table(lapply(dataTable, function(x) gsub("[,$]","",x)))
  new_table<-new_table[,.(OPE.ID,fls_recipients,fls_cash_disbursed,flu_recipients,flu_cash_disbursed,flp_recipients,flp_cash_disbursed)]
  new_table<-as.data.table(lapply(new_table,as.numeric))
}

cleanDL<-function(dataTable){
  new_table<-as.data.table(lapply(dataTable, function(x) gsub("[,$]","",x)))
  new_table<-new_table[,.(OPE.ID,dls_recipients,dls_cash_disbursed,dlu_recipients,dlu_cash_disbursed,dlp_recipients,dlp_cash_disbursed)]
  new_table<-as.data.table(lapply(new_table,as.numeric))
}

getSchool<-function(dataTable){
  row<-dataTable[OPE.ID==170600]
}  

