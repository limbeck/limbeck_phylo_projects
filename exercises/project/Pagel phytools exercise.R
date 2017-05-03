install.packages("phytools")
library(phytools)
library(geiger)
source("fitPagel.R")
.check.pkg<-phytools:::.check.pkg
##Didn't work for some reason-I think the code is likely out of date or has been update to not be included in the package anymore

##Simulate uncorrelated data
tree<-pbtree(n=300,scale=1)
Q<-matrix(c(0,1,1,0),2,2)
rownames(Q)<-colnames(Q)<-letters[1:2]
ttl<-sim.history(tree,Q)
tt2<-sim.history(tree,Q)

##These are uncorrelated, see:
par(mfrow=c(1,2))
plotSimmap(ttl,setNames(c("blue","red"),letters[1:2]),ftype="off",lwd=1)
plotSimmap(tt2,setNames(c("blue","red"),letters[1:2]),ftype="off",lwd=1,direction="leftwards")

x<-ttl$states
y<-tt2$states
fit.ape<-fitPagel(tree,x,y)
fit.ape

fit.geiger<-fitPagel(tree,x,y,method="fitDiscrete")
fit.geiger


##Let's try correlated data
#Need to simulate some data
Q<-matrix(c(0,0.5,0.5,0,2,0,0,2,2,0,0,2,0,0.5,0.5,0),4,4,byrow=TRUE)
rownames(Q)<-colnames(Q)<-c("aa","ab","ba","bb")
diag(Q)<--rowSums(Q)
tt<-sim.history(tree,t(Q))

tt1<-mergeMappedStates(tt,c("aa","ab"),"a")
tt1<-mergeMappedStates(tt1,c("ba","bb"),"b")
tt2<-mergeMappedStates(tt,c("aa","ba"),"a")
tt2<-mergeMappedStates(tt2,c("ab","bb"),"b")
## these data are correlated, see:
par(mfrow=c(1,2))
plotSimmap(tt1,setNames(c("blue","red"),letters[1:2]),ftype="off",lwd=1)
plotSimmap(tt2,setNames(c("blue","red"),letters[1:2]),ftype="off",lwd=1,direction="leftwards")

x<-getStates(tt1,"tips")
y<-getStates(tt2,"tips")
fit.ape<-fitPagel(tree,x,y)
fit.ape

fit.geiger<-fitPagel(tree,x,y,method="fitDiscrete")
fit.geiger

##This model is really slow-thought my computer was broken at points-is there a way to speed it up?
###Bad news that this exists already though and spits everything out that you could want. Totally cool to see it in code though.

##Try again with uncorrelated data
tree2<-pbtree(n=18,scale=1)
Q<-matrix(c(-1,1,1,-1),2,2)
rownames(Q)<-colnames(Q)<-letters[1:2]
tt3<-sim.history(tree2,Q)
tt4<-sim.history(tree2,Q)
par(mfrow=c(1,2))
plotSimmap(tt3,setNames(c("blue","red"),letters[1:2]),ftype="off",lwd=1)
plotSimmap(tt4,setNames(c("blue","red"),letters[1:2]),ftype="off",lwd=1,direction="leftwards")
x<-tt3$states
y<-tt4$states
fit.ape<-fitPagel(tree2,x,y)
fit.ape
fit.geiger<-fitPagel(tree2,x,y,method="fitDiscrete")
fit.geiger
