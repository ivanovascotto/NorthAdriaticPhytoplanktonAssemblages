library("snpar")
quant.test( c(1,2,3,4,5,4,3,2,1),c(33,33,3,4)  )

a<-rnorm(2000, 11,5)
b<-rpois(2000, 10)
c<-quant.test( a,b  , exact=TRUE, correct = FALSE)

c$p.value

par(mfrow=c(1,3))
a<-R0515
b<-R1621
boxplot(list(a,b), main=paste0(round(median(a), digits=3),"    ",round(median(b), digits=3) ))
hist(a)
hist(b)

quant.test(agg.out.SUM[which(agg.out.SUM$Group.1<2016 & agg.out.SUM$Group.1>=2010),colforname],agg.out.SUM[which(agg.out.SUM$Group.1>=2016),colforname])
quant.test(c(0,0,0,0),c(0,0,0,0))
