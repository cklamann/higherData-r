getCPIs<-function(cpi, table){
  cpi<-as.data.table(cpi)
  table<-as.data.table(table)
  max_year<-as.numeric(cpi[,max(year)])
  max_year_average<-cpi[year==max_year,average]
  real<-table[,lapply(.SD,function(x) x*(max_year_average/cpi[year==fall_year,average])),by=.(fall_year,unitid)]  
  real
}

filterCPIs<-function(){
  toFilter<-list.files("C:/Users/Conor/Dropbox/study/real_values/")
  directory<-"C:/Users/Conor/Dropbox/study/real_values/"
  mergeFile<-as.data.table(read.csv("C:/Users/Conor/Desktop/new_colleges.csv"))
  for(n in 1:length(toFilter)){
    file<-as.data.table(read.csv(paste0(directory,toFilter[n])))
    cat(toFilter[n])
    filtered<-merge(file,mergeFile,by="unitid")
    filtered[,c("INSTNM","type","OPEID"):=NULL]
    write.csv(filtered,paste0(directory,toFilter[n],"_merged.csv"))
  }
}