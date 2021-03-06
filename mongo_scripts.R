library(mongolite)

mongostring <- paste0("mongodb://",MONGO_USER,":",MONGO_PW,"@localhost:",MONGO_PORT,"/",MONGO_DB)

connectMongo <- function(collection, url = mongostring){
  mongo(collection=collection, url=url)
}

insertSchoolData <- function(dt, conn){
  conn$insert(dt)
}

#this is very slow and should be used only for targeted updates
updateSchoolData<-function(dt,conn){
  tryCatch({length(dt[,sector]) >0}, error = function(e){stop("sector missing, did you Tr
ansformTable()?")} )
  apply(dt,1,.updateSchoolData, conn)
}

.updateSchoolData<-function(row,conn){
  query<-paste0('{"unitid":"',row[['unitid']],'","fiscal_year":"',row[['fiscal_year']],'","variable":"',row[['variable']],'"}')
  updateStatemnt = paste0('{"$set":{"unitid":"',row[['unitid']],'","fiscal_year":"',row[['fiscal_year']],'","variable":"',row[['variable']],
                            '","name":"',row[['name']],'","state":"',row[['state']],'","sector":"',row[['sector']],'","value":',row[['value']],'}}')
  conn$update(query, update = updateStatemnt, upsert = TRUE, multiple = FALSE)
}

#schoolTable should be pulled straight from database
prepareTable<-function(dataTable,schoolTable){
  setDT(schoolTable)
  f<-as.data.table(melt.data.table(dataTable,c("unitid","fiscal_year"),variable.factor=FALSE))
  f<-f[!is.na(f[,value])]
  if(!all(unique(f[,unitid]) %in% schoolTable[,unitid])){
      lost = length(unique(f[!f[,unitid] %in% schoolTable[,unitid],unitid]))
      print(paste0("Omitting records from ",lost," schools with no unitid match in the schools table!"))
      #stop("there are unitids in the school_data table that aren't in the schools table!")
  }
  
  merged<-merge(f,schoolTable[,.(unitid,sector,state,name)],by="unitid")
  merged[,c("unitid","fiscal_year","sector"):=lapply(.SD,as.character),.SDcols=c("unitid","fiscal_year","sector")]
}

updateSchoolNames<-function(dt,conn){
  apply(dt,1,.updateSchoolNames, conn)  
}

.updateSchoolNames<-function(row,conn){
  query<-paste0('{"unitid":"',row[['unitid']],'"}')
  updateStatement = paste0('{"$set":{"name":"',row[['name']],'"}}')
  conn$update(query, update = updateStatement, upsert = FALSE, multiple = TRUE)
}


