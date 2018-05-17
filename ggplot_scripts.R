#for twitter, look up school name and calculate jump in php.
#chart is chart, twitter content comes from php script on same dataset
#so only need unitid in filename.

#Accurate now

drawAll<-function(long_data){
  unitids<-as.data.table(unique(long_data[,unitid]))
  setnames(unitids, "V1","unitid")
  names<-merge(unitids,as.data.table(fread("/home/conor/Desktop/new_colleges.csv")),by="unitid")[,instnm]
  for(i in 1:nrow(unitids)){
    makeChart(long_data, unitids[i], names[i])
  }
}

makeChart<-function(long_data, passed_unitid, name){
  subsetted<-subsetSchools(long_data, passed_unitid)
  drawLine(subsetted,name)
}

subsetSchools<-function(long_data, passed_unitid){
 long_data[unitid == passed_unitid]
 #throw error if unitid is not in the dataset.
}

drawLine<-function(data_subset,name){
  data_subset[is.na(total_plus)]<-0
  low_val<-min(data_subset[,total_plus])
  high_val<-max(data_subset[,total_plus])
  gap = high_val - low_val
  name=formatName(name)
  years = unique(data_subset[,fall_year])
  filename = paste0("/home/conor/Dropbox/study/publicity/blog/charts/plus_jumps/",data_subset[,unitid[1]],".png")
  plot <- ggplot(data_subset, data_subset[,aes(fall_year,total_plus)]) +
  geom_line(color="firebrick") +
  geom_point(color="firebrick") +
  scale_y_continuous(name=element_blank(),labels=makeYLabels,breaks=seq(low_val,high_val,gap/10)) +
  scale_x_continuous(name="Academic Year",breaks=years,labels=makeXLabels(years)) +
  labs(title=paste0("Undergraduate PLUS Loan Revenue\n",name),subtitle="*All values in 2016 dollars",caption = "Source: Federal Student Aid") +
  theme(axis.text.x = element_text(size = 9, angle = 50, hjust = 1),axis.text.y=element_text(size=9),
        plot.caption = element_text(size = rel(.6)), plot.title = element_text(size=rel(1.1),color="firebrick"),
        plot.subtitle = element_text(size=rel(.6)), plot.margin = unit(c(1,1,1,0),"mm"))
  ggsave(filename,plot)
  print(plot)
  return(plot)
}

formatName<-function(name){
    #maxLength -- how long can the title be?
    #break it up into a string of that length.
    name
}

makeYLabels <-function(value){
  vec = character();
  suffix=""
  for(i in value){
    if(i>999999){
      suffix = " Mil"
      i<-paste0(dollar(i/1000000), suffix)
    } else{
      i = dollar(i)
    }
    vec=c(vec,i)
  }
  vec
}

makeXLabels<-function(x){
  nextYear<-x+1
  lastpart<-paste0("-",substr(as.character(nextYear),3,4))
  formatted<-paste0(x,lastpart)
  formatted
}