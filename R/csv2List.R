csv2List <-
function(filename){
  data<- as.list(read.csv(filename, header=T))
  values<- lapply(data, function(x) x[!is.na(x)])
  items<- lapply(values,"*", 100)
  return(items)
}
