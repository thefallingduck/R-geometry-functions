#### New XY corrordinants

setwd('G:/Dropbox/Dropbox/Embryonic Ammonite')

xyframe<-read.csv('SIMSpits.csv',header=TRUE)

J273<-as.data.frame(read.csv('J-273.csv',header=TRUE,stringsAsFactors=FALSE))
J273<-transform(J273,VPDB=as.numeric(VPDB))
J273.2<-as.data.frame(read.csv('J-273-22.csv',header=TRUE,stringsAsFactors=FALSE))
J273.2<-transform(J273.2,VPDB=as.numeric(VPDB),Number=as.numeric(Number))
J273<-rbind(J273,J273.2)
day.1.arg<-(-.425)

source('Anglefrompoints.r')
source('errorbar.r')
source('pointdist.r')
library(calibrate)
diageneticX<-J273$X[J273$Ontogeny=='D']
diageneticY<-J273$Y[J273$Ontogeny=='D']

#mid.x<-xyframe$X.1[1]
mid.x<-seq(from=1725,to=1825,by=2)
#mid.x<-1770
#mid.y<-xyframe$Y[1]
mid.y<-seq(from=1500,to=1650,by=2)
ammonite<-J273[J273$Ontogeny=='P'|J273$Ontogeny=='V'|J273$Ontogeny=='E'&J273$Number!='Inner',]
ammonite<-ammonite[ammonite$Number!='outer septa'&ammonite$Number!='inner septa',]
ammonite18O<-ammonite$VPDB+day.1.arg
ammoniteY<-ammonite$Y
ammoniteX<-ammonite$X

ammonite.ord<-ammonite[order(ammonite$VPDB),]
mid.x.sims<-diageneticX[1]
mid.y.sims<-diageneticY[1]

plot((diageneticX-mid.x.sims),(diageneticY-mid.y.sims))
points((xyframe$X.1[1:50] - mid.x[1:50]), (xyframe$Y[1:50] - mid.y[1:50]), col = 'red', pch = 5)

#####
centersolve<-mid.y
xymatrix<-matrix(ncol=2,nrow=(length(mid.x)*length(mid.y)))

comb<-expand.grid(mid.x,mid.y)
centersolve<-comb$Var1

new.radius <- vector()

for(k in 1:nrow(comb)){

for(j in 3:(length(ammoniteX)+1)){
pt.1<-c(comb[k,1],comb[k,2])
pt.2<-c(xyframe$X.1[j],xyframe$Y[j])

new.radius[j-2]<-distance(pt.1=pt.1,pt.2=pt.2)
}

new.data<-cbind(xyframe[3:208,1:3],new.radius)
dat<-cor(new.data,method='spearman')
centersolve[k]<-dat[4,1]

}
findit<-cbind(comb,centersolve)

plot(mid.y[1:length(mid.y)],centersolve[1:length(mid.y)])



plot(sort(radius.ammonite),new.radius)
ammonite<-cbind(ammonite,radius.ammonite)
ammonite<-ammonite[order(ammonite$radius.ammonite),]



index<-1:length(ammoniteX)
ammoniteanalysis<-as.data.frame(cbind(index,ammonite18O,ammoniteX,ammoniteY,radius.ammonite))
ammoniteanalysissorted<-ammoniteanalysis[order(ammoniteanalysis$radius.ammonite),]

newcordinants<-as.data.frame(cbind(xyframe$X.1[3:208],xyframe$Y[3:208],new.radius))
newcordinants<-newcordinants[order(newcordinants$new.radius),]
new.x<-newcordinants$V1
new.y<-newcordinants$V2
ammonite.new<-cbind(ammonite,new.x,new.y)

write.table(ammonite.new,'adjustedxyJ273.csv',sep=',')
newcord<-cbind(ammoniteanalysissorted,newcordinants$X.1[>2],newcordinants$Y[>2])
#####Use the ordering to help correlate across days.
#####Initial data is in order, so make index from it. Use to sort the radius?


##### Pit locations in x-y space.
pdf('pitnumbers.pdf',width=16,height=16)
plot(xyframe$X.1,xyframe$Y,cex=.25)
textxy(xyframe$X.1,xyframe$Y,xyframe$X)
dev.off()
