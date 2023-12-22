library(ade4)
library(adespatial)
library(adegraphics)
library(spdep)
library(maptools)
library(xlsx)
library(vegan)
library(viridis)
library(dplyr)

allenv<-read.xlsx("my_starting_environement", sheetName="env_data")

#here the names of the column are changed
colnames(allenv)[which(colnames(allenv)=="Soca")]<-"River"
colnames(allenv)[which(colnames(allenv)=="Soca_var")]<-"River_var"

rownames(allenv)<-allenv[,1]
allenv[,1]<-NULL
allenv[["T_bot"]]<-NULL
allenv[["T_surf"]]<-NULL
allenv[["Wind_dir"]]<-NULL
allenv<-allenv[,c(which(colnames(allenv)=="T_air"),which(colnames(allenv)=="T_gradient"),which(colnames(allenv)=="Sal_surf") ,
                  which(colnames(allenv)=="River"),which(colnames(allenv)=="River_var"),which(colnames(allenv)=="Rain"),which(colnames(allenv)=="Rain_var"),
                  which(colnames(allenv)=="Wind_speed"),which(colnames(allenv)=="E.W"),which(colnames(allenv)=="N.S"))]

#here the sequence of dates is creatred
dates <- seq.Date(from = as.Date("2005-01-01"), to=as.Date("2017-11-01"), by = "month")
dates<-format(dates, "%Y-%m")
dates<-as.data.frame(dates)
dates<-cbind(dates, seq(1,nrow(dates),1))


#here the DISTANCES GRAPH is produced#
time_pos<-c()
pos_pos<-1
for (i in 1:nrow(dates)) {
  if(any(rownames(allenv)==(dates$dates)[i])){
    time_pos[pos_pos]<-dates$`seq(1, nrow(dates), 1)`[i]
    pos_pos<-pos_pos+1}}
mxy<-matrix(c(rep(0,length(time_pos)), time_pos), ncol = 2)

#here the moran maps are computed#

nbgab <- chooseCN(coordinates(mxy), type = 4, plot.nb = FALSE)
nb2listw(nbgab)
distgab <- nbdists(nbgab, mxy)
listwgab <- nb2listw(nbgab, glist = distgab) # the spatial weighting matrix is then created:


mem.gab <- mem(listwgab)
barplot(attr(mem.gab, "values"), 
        main = "Eigenvalues of the spatial weighting matrix", cex.main = 0.7)

moranI <- moran.randtest(mem.gab, listwgab, 99)


#####here the envrionemntal variables are decomposed using the Moran eigenvectors

envdata<-as.matrix(mem.gab[,1:(ncol(mem.gab))])
spdata<-as.matrix(scale(allenv, scale = TRUE))

MC.env <- moran.randtest(spdata, listwgab, nrepet = 999)
env.maps <- s1d.barchart(MC.env$obs, labels = MC.env$names, plot = FALSE, xlim = c(-1.1,1.1), paxes.draw = TRUE, pgrid.draw = FALSE)
addline(env.maps, v = c(-1,1,1.1), plot = TRUE, pline.col = 'red', pline.lty = 3)

mem.gab.sel <- mem.select(spdata, listw = listwgab,  MEM.autocor="all")
mem.gab.sel$global.test

pca.hell<-dudi.pca(spdata, scale = FALSE, scannf = FALSE, nf = ncol(spdata))
rda.hell <- pcaiv(pca.hell, mem.gab.sel$MEM.select, scannf = FALSE)
test.rda <- randtest(rda.hell)
plot(test.rda)

envdata<-mem.gab.sel$MEM.select
mspa.hell <- mspa(pca.hell, listwgab, scannf = FALSE, nf = 2)
g.mspa <- scatter(mspa.hell, posieig = "topright", plot = FALSE)

par(mfrow=c(2,1), mar=c(2,2,2,2))
for(i in 1:ncol(allenv)){
  scalo <- scalogram(allenv[,i], mem.gab)
  plot(scalo, main=colnames(allenv)[i])}

#####here the fitted and residuals values are calculated
envdata<-as.matrix(envdata)
spdata<-as.matrix(spdata)
B<-solve(t(envdata)%*%envdata)%*%t(envdata)%*%spdata
Yfit<-envdata%*%B
Syfit<-cov(Yfit)    
round(eigen(Syfit)$values, digits = 4)
centerYfit<-scale(Yfit, center = TRUE, scale=FALSE)
Z<-centerYfit%*%eigen(Syfit)$vector#-----------------POSITIONS OF FITTED IN CANONICAL SPACE
t(eigen(Syfit)$vector)%*%eigen(Syfit)$vector#--------scaled to 1
(t(Z)%*%Z)/(nrow(Z)-1)#------------------------------scaled to eigenvalues
RxZ<-cor(envdata, Z)#-------------------------------------correlation with the Z scores
Yres<-spdata-Yfit#-----------------------------------RESIDUALS
Ures<-eigen(cov(Yres))$vector
Ures_values<-eigen(cov(Yres))$values
Fres<-Yres%*%Ures#-----------------------------------POSITIONS OF RESIDUALS in NON CANONICAL SPACE

#--------------------------------BIPLOT 1/// 
TV<-sum(eigen(cov(spdata))$values)#TOTAL VARIANCE
RxZ_scaled<-(RxZ%*%diag(sqrt(eigen(Syfit)$values)))/sqrt(TV)
plot(Z)
for (i in 1:nrow(Z)) {
  arrows(0,0,RxZ_scaled[i,1]*sqrt(TV),RxZ_scaled[i,2]*sqrt(TV))}

Rsquare<-sum(eigen(cov(Yfit))$values)/sum(eigen(cov(spdata))$values)

#----------------------------------storage
Yres<-as.data.frame(Yres)
Yfit<-as.data.frame(Yfit)
rownames(Yfit)<-rownames(Yres)
write.xlsx(Yres, "my_environement_non_periodic.xlsx", sheetName="env_data")
write.xlsx(Yfit, "my_environement_periodic.xlsx", sheetName="env_data")

