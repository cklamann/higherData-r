#calculates ftes for schools
#pulls from enrollment tables saved in full_tables/race/

#EFyyyyA
#yyyy refers to academic year, adjusted to fiscal year in filter function

#efalevels:

# 22 = fulltime ug
# 42 = parttime ug
# 32 = ft g
# 52 = pt g

library(data.table)
fteYears <- c(2002:2015)
fteDownloadDir<-"/home/conor/Dropbox/study/research/ipeds/full_tables/enrollment_by_race/"
fteSourceFiles <- data.table(file = c(paste0("EF",fteYears,"A")),fy = fteYears)
rosterFile<-fread("/home/conor/Desktop/hd2015.csv")
setnames(rosterFile,names(rosterFile),tolower(names(rosterFile)))

#through 2007, efrace24 is eftotlt

fteReturnFields<-c("unitid","fiscal_year","fte_g","fte_ug")

fteFilterData<-function(years = fteYears){ 
  fteTable<-initializeDataTable(fteReturnFields)
  for(n in years) {
    table<-fread(paste0(fteDownloadDir,n,".csv"),stringsAsFactors = F)
    table<-cleanNumericTable(table)
    if("efrace24" %in% names(table)){
      setnames(table,"efrace24","eftotlt")  
    }
    table<-table[efalevel %in% c(22,32,42,52),.(unitid,efalevel,eftotlt)]
    table<-merge(table,rosterFile,by="unitid")
    table[sector %like% "Public" & efalevel %in% c(42,52),fte:=eftotlt*.403543]
    table[sector %like% "Private" | sector %like% "Proprietary" & efalevel %in% c(42,52),fte:=eftotlt*.392857]
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