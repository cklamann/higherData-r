
connectMongo <- function(collection,db){
  url = "mongodb://127.0.0.1:27017"
  mongo(collection = collection, db= db,url = url)
}


#add new values or update existing
updateSchoolData<-function(dt,conn){
  tryCatch({length(dt[,sector]) >0}, error = function(e){stop("sector missing, did you TransformTable()?")} )
  apply(dt,1,.updateSchoolData, conn)
}

.updateSchoolData<-function(row,conn){
  query<-paste0('{"unitid":"',row[['unitid']],'","fiscal_year":"',row[['fiscal_year']],'","variable":"',row[['variable']],'"}')
  updateStatemnt = paste0('{"$set":{"unitid":"',row[['unitid']],'","fiscal_year":"',row[['fiscal_year']],'","variable":"',row[['variable']],
                            '","name":"',row[['name']],'","state":"',row[['state']],'","sector":"',row[['sector']],'","value":',row[['value']],'}}')
  conn$update(query, update = updateStatemnt, upsert = TRUE, multiple = FALSE)
}

#schoolTable should be pulled straight from database
transformTable<-function(dataTable,schoolTable){
  f<-as.data.table(melt.data.table(dataTable,c("unitid","fiscal_year"),variable.factor=FALSE))
  f<-f[!is.na(f[,value])]
  merged<-merge(f,schoolTable[,.(unitid,sector,state,name)],by="unitid")
  merged[,c("unitid","fiscal_year","sector"):=lapply(.SD,as.character),.SDcols=c("unitid","fiscal_year","sector")]
}

updateSchoolNames(dt,conn){
  apply(dt,1,.updateSchoolNames, conn)  
}

updateSchoolNames(row,conn){
  query<-paste0('{"unitid":"',row[['unitid']],'"}')
  updateStatemnt = paste0('{"$set":{"name":"',row[['name']],'"}}')
  conn$update(query, update = updateStatemnt, upsert = FALSE, multiple = TRUE)
}


