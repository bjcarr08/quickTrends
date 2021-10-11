PLOT <-
function(items=items, zoom="IN", rows=3, cols=5){
  par(mfrow=c(rows,cols), mar=c(2.5,2.5,2,0.5))
  for (i in c(1:length(items))){
y = items[i]
main = names(y)
lapply(y, zoom, main)
  }
}
