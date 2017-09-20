updateData_mysql=function(){
  print("Dont have data. Sourcing...")
  req = paste0("http://data.pvoutput.org/service/r2/getregionoutput.jsp?r=1:victoria&sid=40450&key=",pvoutputKey)
  h <- basicHeaderGatherer()
  doc <- getURI(req, headerfunction = h$update)
  h$value()
  csv = read.csv(text = doc,header = FALSE,encoding="UTF-8")
  names(csv) = c("id","size","postcode","output")
  csv$size = csv$size / 1000
  csv$output = csv$output / 1000
  csv$date=h$value()["X-Payload-Date"]%>%as.Date(format="%Y%m%d")
  dbWriteTable(connection, "output", csv,append=TRUE,row.name=FALSE)  
  print("Sourcing done!")
}

haveData_mysql=function(d){
  N=dbGetQuery(connection,paste0("SELECT count(*) FROM output WHERE date='",d,"';"))
  unname(unlist(N))>0
}


getPVdata1Day=function(d){
  dbGetQuery(connection,paste0("SELECT * FROM output WHERE date='",d,"';"))
}

getPVdataWindow=function(begin,end){
  dbGetQuery(connection,paste0("SELECT * FROM output WHERE date>='",begin,"' AND date<='",end,"';"))
}

getDailyEff=function(begin,end){
  dbGetQuery(connection,paste0("SELECT date, AVG(output/size) as Efficiency FROM output WHERE date>='",begin,"' AND date<='",end,"' GROUP BY date;"))
}
