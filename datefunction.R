# use the built in date sequence function to build a frame of date ranges
dRangeSeq<-function(start_date, end_date, by=1){
  start_date<-as.Date(start_date)
  end_date<-as.Date(end_date)
  sdates<-seq.Date(start_date, end_date, by=by)
  
  result<-data.frame("start_date" = sdates[-length(sdates)], 
                     "end_date" = sdates[2:length(sdates)])
  return(result)
}
