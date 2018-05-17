## these scripts help in merge. They should:
# 1. melt table
# 3. filter out nas
# 3. get meds
# 4. upload data to melted table in database

##uploading to remote:
# 1. dump to local: sudo mysqldump --net_buffer_length=8192 trends2 chart_data > backup.sql
# 2. zip backup.sql.zip backup.sql
# 3. transfer zipped file to remote server, unzip
# 4. upload: mysql -u klamann -p klamann_trends2 < backup.sq


getAllMeds<-function(meltedTable){
  setDT(meltedTable)
  roster <- fread("/home/conor/Desktop/hd2015.csv")
  roster <- namesToLower(roster)
  DT <- merge(roster[sector < 7,.(unitid,sector)],meltedTable,by="unitid")
  medians<-DT[,median(value),by=c("fiscal_year","variable","sector")]
  medians[,unitid:=paste0("99999",sector)]
  medians[,sector:=NULL]
  setnames(medians,"V1","value")
  medians
}

meltTable<-function(dataTable){
  f<-as.data.table(melt.data.table(dataTable,c("unitid","fiscal_year"),variable.factor=FALSE))
  f
}

#main method -- note it can update any table, not just chart_data -- not really, the update shit won't work if there's dupes
#Also if yr uploading strings, need to do dbEscapeStrings instead of sendQuery
updateChartData<-function(connection,dataTable){
  dbSendQuery(connection, "SET GLOBAL innodb_lock_wait_timeout = 120")
  #dbSendQuery(connection, "SET FOREIGN_KEY_CHECKS = 0")
  #dbEscapeStrings(connection, updateRows(dataTable,"chart_data"))
  dbSendQuery(connection, updateRows(dataTable,"chart_data"))
  #purge(connection)
  #dbSendQuery(connection, "SET FOREIGN_KEY_CHECKS = 1")
}

connect<-function(whichdb){
  mydb = dbConnect(MySQL(), user='trends', password='', dbname=whichdb, host='127.0.0.1')
  mydb
}

connectRemote <-function(whichdb){
  mydb = dbConnect(MySQL(), user='klamann', password='ul9bOn}dh6tD', host='108.167.180.200')
  mydb
}

#internal use only
updateRows<-function(dataTable,tableName){ #tableName must be quoted 
  columns<-getColumns(dataTable)
  statement<-paste0('INSERT INTO ',tableName," (",columns,") VALUES ")
  statement_2<-getRows(dataTable)
  statement_3<-"ON DUPLICATE KEY UPDATE value = VALUES(value)"
  full_statement<-paste0(statement,statement_2,statement_3); 
  print(full_statement)
  full_statement
}

getColumns<-function(mysqlTable){
  columns<-names(mysqlTable)
  col_list<-""
  for(n in 1:length(columns)){
    col_list <- paste(col_list,columns[n],sep=",")
  }
  col_list<-substring(col_list,2)
  col_list
}

getRows<-function(dataTable){
  #todo: vectorize!
  values=list()
  parens=list()
  for(n in 1:nrow(dataTable)){
    row<-as.character(dataTable[n]) 
    values[[n]]<-put_quotes(row)
    values[[n]]<- paste(values[[n]],collapse=",")
  }
  for(n in 1:length(values)){
    parens[n]<-paste("(",values[n],")")
  }
  toString(parens)
}

put_quotes<-function(vec){
  vec = paste0("'",vec,"'")
}

closeConnections<-function(){
  lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
}
