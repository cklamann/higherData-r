#calculates ftes for schools
#pulls from enrollment tables from enrollment_by_race.R

#EFyyyyA
#yyyy refers to academic year, adjusted to fiscal year in filter function

# sectors:
# 1-3 4yr
# 4-6 2yr
# 7-9 1yr
# 1,4,7 = public
# 2,5,8 = npo
# 3,6,9 = fp

#efalevels:

# 22 = fulltime ug
# 42 = parttime ug
# 32 = ft g
# 52 = pt g

library(data.table)
fteYears <- c(2002:2018)
fteSourceFiles <- data.table(file = c(paste0("EF",fteYears,"A")),fy = fteYears)

fteDownload<-function(fteDownloadDir, years = fteYears){
  download(fteSourceFiles, years, fteDownloadDir)
}


#through 2007, efrace24 is eftotlt

fteReturnFields<-c("unitid","fiscal_year","fte_g","fte_ug")

fteTransform<-function(fteDownloadDir, schoolsTable, years = fteYears){ 
  fteTable<-initializeDataTable(fteReturnFields)
  for(n in years) {
    table<-fread(paste0(fteDownloadDir,n,".csv"),stringsAsFactors = F)
    table<-cleanNumericTable(table)
    if("efrace24" %in% names(table)){
      setnames(table,"efrace24","eftotlt")  
    }
    table<-table[efalevel %in% c(22,32,42,52),.(unitid,efalevel,eftotlt)]
    table<-merge(table,schoolsTable,by="unitid")
    table[sector %in% c(1,4,7) & efalevel %in% c(42,52),fte:=eftotlt*.403543]
    table[!sector %in% c(1,4,7) & efalevel %in% c(42,52),fte:=eftotlt*.392857]
    table[efalevel %in% c(22,32),fte:=eftotlt]
    table<-table[,.(unitid,fte,efalevel)]
    table<-dcast.data.table(table,formula = unitid~efalevel,value.var="fte")
    table[,fte_ug:=plus(`22`,`42`)]
    table[,fte_g:=plus(`32` , `52`)]
    table[,fiscal_year:=n+1]
    table<-table[,.SD, .SDcols = fteReturnFields]
    fteTable<-rbind(table,fteTable)
    print(paste0("finished ",n))
  }
  fteTable
}