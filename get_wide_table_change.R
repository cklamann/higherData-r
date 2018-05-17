#function takes a data.table of wide time-series data (like that extracted from Delta...)
#one column should be called fall_year
#also, unitid and instname should be there
#returns list of percentage increase for each category from first year to last year
#currently not equipped to handle NAs. Will cross that bridge when it comes...
#assumes that every schools has the same year range

getChangeWide<-function(dataTable){
  max_year<-max(dataTable[,fall_year]) #get year range 
  min_year<-min(dataTable[,fall_year])

  min_table<-as.data.table(dataTable[fall_year==min_year]) #extract min and max rows
  max_table<-as.data.table(dataTable[fall_year==max_year])
  
  max_table[,c("fall_year","unitid","instname"):=NULL] #these won't factor in equations
  
  for(n in 1:ncol(max_table)){
    setnames(max_table,names(max_table)[n],paste0(names(max_table)[n],"_",max_year)) #rename max_columns for later identification
  }
  
  nurr_diff<-data.table(instname=min_table[,instname]) # random data.table with column of names (if you've got 'em) or whatever
  
  new_table<-cbind(min_table,max_table) #combine the max and min tables for easy arithmetic
  
  for(n in 1:ncol(min_table)){
    if(names(min_table)[n] %in% c("fall_year","unitid","instname")){ #skip these
      next
    }
    nurr_diff[,names(min_table)[n]:= paste0(round( ( ( (new_table[,get(paste0(names(min_table)[n],"_",max_year))] / new_table[,get(names(min_table)[n])])-1)*100),2),"%")]
    }
  nurr_diff
    
}