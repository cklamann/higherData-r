connect<-function(whichdb){
  mydb = dbConnect(MySQL(), user='trends', password='', dbname=whichdb, host='127.0.0.1')
  mydb
}

fetchTable<-function(connection,table){ #table name must be quoted
  statement = paste("SELECT * FROM ",as.character(table),sep="")
  rs=dbSendQuery(connection,statement)
  data=fetch(rs,n=-1)
  as.data.table(data)
}

makeReal<-function(connection,this_year,that_year,value){
  cpi_table<-as.data.table(fetchTable(connection,"cpi_table"))
  number<-cpi_table[year==that_year,average]
  ratio<-cpi_table[year==this_year,average] / number
  value*ratio
}
  
purge<-function(connection){
  dbSendQuery(connection, "DELETE FROM graph_data WHERE variable NOT IN (SELECT variable_name FROM graph_variables)")
}

makeOneReal<-function(connection=NULL,this_year,that_year,value,cpi_table=NULL){
  #Need to pass either the table itself or a connection to it
  #cpi table needs to have column names "year" and "average"
  if(is.null(cpi_table)){
    cpi_table<-as.data.table(fetchTable(connection,"cpi_table"))
  }
  number<-cpi_table[year==that_year,average]
  ratio<-cpi_table[year==this_year,average] / number
  value*ratio
}

makeLotsReal<-function(this_connection,dataTable,this_cpi_table=NULL){  #takes a melted dataTable, the ones in graph_data--will make "value" column real
  max_year<-max(dataTable[,fall_year])
  setorder(dataTable,unitid,fall_year)
  dataTable[,lapply(.SD,makeOneReal,connection=this_connection,this_year=max_year,that_year=fall_year,cpi_table=this_cpi_table),by=.(fall_year,unitid,variable),.SDcols="value"]
}

runAbruptChange<-function(vec,pct_less=.5,pct_more=2){#used with checkChange
  result=0
  if(length(vec)>2){
    for(n in 1:(length(vec)-1)){
      if(is.na(vec[n])| is.na(vec[n+1]) | vec[n]==0 | vec[n+1]==0){
        next
      }
      if(vec[n+1]<vec[n]*pct_less){
        result<-round(vec[n+1]/vec[n],2)
      }
      else if(vec[n+1]>vec[n]*pct_more){
        result<-round(vec[n+1]/vec[n],2)
      }
    }
  }
    result
}

runBiggestChange<-function(vec,pct_less=.5,pct_more=2){#checks for certain discrepancy between highest peak and lowest valley
  result=0
  vec<-vec[!is.na(vec)]
  if(length(vec)>2){
    max_val<-max(vec)
    min_val<-min(vec)
      if(max_val>pct_more*(min_val)){
        result<-max_val/min_val
      }
      else if(min_val<pct_less*(max_val)){
        result<-min_val/max_val
      }
  }
  result
}

runFirstLastChange<-function(vec,pct_less=.5,pct_more=2){#used with checkChange, checks disc. between first and last years
  result=0
  vec<-vec[complete.cases(vec)]
  if(length(vec)>1){
    max_year<-length(vec)
    min_year<-1
    if(vec[max_year]>pct_more*vec[min_year]){
      result<-abs(vec[max_year]/vec[min_year])
    }
    else if(vec[max_year]<pct_less*vec[min_year]){
      result<--(abs(vec[max_year]/vec[min_year]))
    }
  }
  result
}

#this checks to see how big the change has been from one year to the next
#can be passed a data.table of any size, field called "value" will be checked for change irregularities
#default change threshholds are .5 and 2
#if thresholds are so small that nothing is filetered, will just return all change ratios
checkChange<-function(melted_dataTable,abrupt=FALSE,biggest=FALSE,first_last=FALSE,this_pct_of=.5,this_pct_more=1.2){ 
  result="Something went wrong!"
  setorder(melted_dataTable,unitid,fall_year)#make sure they are in order....
  if(abrupt==FALSE & first_last==FALSE & biggest==TRUE){
    result<-melted_dataTable[,lapply(.SD,runBiggestChange,pct_less=this_pct_of,pct_more=this_pct_more),by="unitid",.SDcols="value"]
  }
  else if(abrupt==TRUE & biggest==FALSE && first_last==FALSE){
    result<-melted_dataTable[,lapply(.SD,runAbruptChange,pct_less=this_pct_of,pct_more=this_pct_more),by="unitid",.SDcols="value"]
  }
  else if(abrupt==FALSE & biggest==FALSE && first_last==TRUE){
    result<-melted_dataTable[,lapply(.SD,runFirstLastChange,pct_less=this_pct_of,pct_more=this_pct_more),by="unitid",.SDcols="value"]
  }
result
}


grabrevised<-function(year_vec,dest_dir_short,full_year=FALSE,ac_year=FALSE,abbreviation_caps,suffix_inc_rv=""){
  value<-formatYears(year_vec)
  download_dir<-"https://nces.ed.gov/ipeds/datacenter/data/"
  dest_dir<-paste0("C:/Users/Conor/Dropbox/study/research/ipeds/converted_excel_sheets/data_blade_2/",dest_dir_short,"/")
  temp <- tempfile()
  if(full_year==FALSE){
    full_year<-""
  }
    else full_year<-"20"
  if(ac_year==TRUE){
    years=vector()
    for(n in 1:length(year_vec)){
      years[n]<-paste0(formatYears(year_vec[n]),formatYears(year_vec[n]+1))
    }
    value<-years
    print(str(value))
  }
  for(n in 1:(length(value))){
    download_file<-paste0(abbreviation_caps,full_year,value[n],suffix_inc_rv,".zip")
    print(download_file)
    unzipped_file<-paste0(tolower(abbreviation_caps),full_year,value[n],tolower(suffix_inc_rv),".csv")
    tryCatch({
      dest_file<-paste0(full_year,value[n],".csv")
      print(paste0(download_dir,download_file))
      download.file((paste0(download_dir,download_file)), temp)
      data <- read.csv(unz(temp,unzipped_file))
      unlink(temp)
      write.csv(data, file = (paste0(dest_dir,dest_file)), row.names=FALSE)		
      print(paste0("revisions saved in ",value[n]))
    },
    warning = function(war) {
      unlink(temp)
      print(paste0("warning in ",value[n]))
      print(war)
    },
    error = function(err) {
      unlink(temp)
      print(paste0("no revisions in ",value[n]))
      print(err)
    },  
    finally = {
      next
    })
  }
}

 myPlus<-function(vec1,vec2){
   results = vector()
   for(n in 1:length(vec1)){
    if(is.na(vec1[n])){
      vec1[n]<-0
    }
    else if(is.na(vec2[n])){
      vec2[n]<-0
    } 
    results[n]<-vec1[n]+vec2[n]
   }
  results 
}

 myDivide<-function(vec1,vec2){
   results = vector()
   for(n in 1:length(vec1)){
     if(is.na(vec1[n])){
       vec1[n]<-0
     }
     else if(is.na(vec2[n])){
       vec2[n]<-0
     } 
     results[n]<-vec1[n]/vec2[n]
   }
   results 
 }
 
 
 
 #returns only the first word of each element of a character vector
 truncLabel<-function(vec){
   vec<-strsplit(vec," ")
   result<-as.character(lapply(vec,function(x){x[1]}))
   result
 }
 
#ifelse is necessary b/c if is not vectorized
#this will return % growth (not ratio)
absGrowthPct<-function(numerator,denominator){
   result = ifelse(numerator>0 & denominator >0,
                 numerator/denominator,
                 ifelse(numerator<0 & denominator<0,
                   (numerator/denominator),
                   (numerator - denominator )/(abs(denominator)))
            )
   result = ifelse(result>0 & numerator>0 & denominator>0,result-1,result)
   result = ifelse(result>1 & numerator<0 & denominator<0,(result-1),result)
   result = ifelse(result>0 & result<1 & numerator<0 & denominator<0,abs(result-1),result)
   result = ifelse(numerator<0 & denominator<0 & numerator<denominator,result*-1,result)
   result
}
 