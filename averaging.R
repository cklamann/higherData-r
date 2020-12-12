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
 