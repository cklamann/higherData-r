#should be line plots faceted by variable fo NU, KU, uPheonix

library(ggplot2)
library(scales)
options(scipen=10000)
plotData<-function(meltedData){
  #nu, #ku
  testSchools <- c(147767, 155317)
  
  plotData <- meltedData[unitid %in% testSchools]
  
  plotData[,value:= as.numeric(value)]
  
  ggplot(data = plotData, aes(fiscal_year, value, color = unitid)) +
    geom_point() +
    scale_y_continuous(labels=scales::comma) +
    geom_path(aes(group = unitid)) +
    facet_wrap(vars(variable)) +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))  
}