OUT <-
function(y, main) {
x = c(1:length(y))
my_loess <- loess(y^(1/2)~x, span=0.70, degree=2)
out<- predict(my_loess)^2
MSE = Metrics::mse(y, out)
infl <- c(FALSE, diff(diff(out)>0)!=0)
yMaxInfl<- out[ifelse(sum(x[infl]^2)==0,1,max(x[infl])):length(out)]
fit<- lm(yMaxInfl~c(1:length(yMaxInfl)))
slope<- fit$coefficients[[2]]
lineCol = ifelse(mean(yMaxInfl)<2, "black",
ifelse(slope>0.05, "#FF7F00",
ifelse(slope<(-0.05),"blue","black")))
boxLTY = ifelse(mean(yMaxInfl)<1,3,1)
boxCOL = ifelse(mean(yMaxInfl)<1,"white",1)
PCH = ifelse(MSE>75, 1, 16)
CEX = ifelse(MSE>75, 1.5, 1)
plot(y~x, ylim=c(0,100), xlab="", ylab="", main=main, cex=CEX, cex.main=0.9, pch=PCH, col=lineCol)
lines(out, col=lineCol)
points(x[infl], out[infl], pch=17, cex=2)
box(col=boxCOL, lty=boxLTY)
}
