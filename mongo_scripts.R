#takes a melted datatable and returns nested json keyed by unitid
#opeid should become unitid....

connectMongo <- function(collection,db){
  url = "mongodb://127.0.0.1:27017"
  mongo(collection = collection, db= db,url = url)
}

updateMongoData<-function(connection,dt){
  #connection should be to schools table
  setDT(dt)
  dt<-dt[!is.na(value)]
  apply(dt,1,updateMongoDataSub,connection)
}

updateMongoDataSub<-function(dt,connection){
  dt<-as.data.frame(lapply(dt,as.character),stringsAsFactors = F)
  #if data field exists and already has the year/var/value
  filterU<-paste0('{"unitid":"',dt['unitid'],'",
                  "data" : {  
                    "$elemMatch" : {
                      "$and": [
                                  {"variable":"',dt['variable'],'"},
                              {"fiscal_year":"',dt['fiscal_year'],'"}
                               ]
                      }
                  }}')
  filterU<-gsub(' |\\n','',filterU)
  setU<-paste0('{"$set": {"data.$.value":',dt['value'],'}}')
  update<-connection$update(filterU,setU,upsert=FALSE)
  #else
  filterP<-paste0('{"unitid":"',dt['unitid'],'"}')
  setP<-paste0('{"$addToSet":{"data":{"fiscal_year":"',dt['fiscal_year'],'","variable":"',dt['variable'],'","value":',dt['value'],'}}}')
  insert<-connection$update(filterP,setP,upsert=FALSE)
} 

#query to mongo will bring back datatable with nested datatable in data property
#melt with this function, after doing whatever filtering:
unnestData<-function(dt){
  data<-apply(dt,1,function(x){
    x[['data']]$unitid <-x[['unitid']]
    x[['data']]
  })
  data<-rbindlist(data,fill=T)
}

updateLots <- function(con,dt){
  apply(dt,1, updateMongo,con)
}

updateMongo<-function(row,con){
  con$update(paste0('{"_id": "', row[['id']], '"},{"$set":{"instnm":"', row[['instnm']], ' (', row[['state']], ')"}}'))
}

