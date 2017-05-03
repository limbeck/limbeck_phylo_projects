{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)
set.seed(1859)
{r, include=TRUE, echo=TRUE, message=FALSE, warning=FALSE}
install.packages(c("ape", "TreeSim", "geiger", "diversitree", "devtools"))
library(ape)
library(TreeSim)
library(geiger)
library(diversitree)
devtools::install_github("thej022214/hisse")
library(hisse)
my.tree <- TreeSim::sim.bd.taxa(n=300, numbsim=1, lambda=0.1, mu=0)[[1]]
plot(my.tree)
ape::ltt.plot(my.tree)
ape::ltt.plot(my.tree, log="y")
yule.trees <- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=0.1, mu=0, complete=FALSE)
plot(yule.trees)
ape::mltt.plot(yule.trees, log="y", legend=FALSE)
bd.trees <- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=1, mu=.9, complete=FALSE)
depth.range <- range(unlist(lapply(yule.trees,ape::branching.times)), unlist(lapply(bd.trees,ape::branching.times)))
max.depth <- sum(abs(depth.range)) #ape rescales depths
plot(x=c(0, -1*max.depth), y=c(1, ape::Ntip(yule.trees[[1]])), log="y", type="n", bty="n", xlab="Time", ylab="N")
colors=c(rgb(1,0,0,0.5), rgb(0, 0, 0, 0.5))
list.of.both <- list(bd.trees, yule.trees)
for (i in sequence(2)) {
  tree.list <- list.of.both[[i]]
  for (j in sequence(length(tree.list))) {
    ape::ltt.lines(tree.list[[j]], col=colors[[i]])   
  }
}
legend("topleft", legend=c("Birth Death", "Yule"), fill=colors)
depth.range <- range(unlist(lapply(yule.trees,ape::branching.times)), unlist(lapply(bd.trees,ape::branching.times)))
max.depth <- sum(abs(depth.range)) #ape rescales depths
plot(x=c(0, -5), y=c(200, ape::Ntip(yule.trees[[1]])), log="y", type="n", bty="n", xlab="Time", ylab="N")
colors=c(rgb(1,0,0,0.5), rgb(0, 0, 0, 0.5))
list.of.both <- list(bd.trees, yule.trees)
for (i in sequence(2)) {
  tree.list <- list.of.both[[i]]
  for (j in sequence(length(tree.list))) {
    ape::ltt.lines(tree.list[[j]], col=colors[[i]])   
  }
}
legend("topleft", legend=c("Birth Death", "Yule"), fill=colors)

##TO DO
#Difference Constant
my.trees1 <- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=.2, mu=.1, complete=FALSE)
ape::mltt.plot(my.trees, log="y", legend=FALSE)
my.trees2<- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=.4, mu=.3, complete=FALSE)
ape::mltt.plot(my.trees, log="y", legend=FALSE)
#Sum Constant
my.treesA<- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=.4, mu=.3, complete=FALSE)
ape::mltt.plot(my.trees, log="y", legend=FALSE)
my.treesB<- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=.5, mu=.2, complete=FALSE)
ape::mltt.plot(my.trees, log="y", legend=FALSE)



#HiSSE
install.packages("hisse")
speciation.rates <- c(0.1, 0.1, 0.1, 0.2) #0A, 1A, 0B, 1B
extinction.rates <- rep(0.03, 4)
transition.rates <- c(0.01,0.01,0, 0.01, 0, 0.01, 0.01,0,0.01, 0,0.01,0.01)
pars <- c(speciation.rates, extinction.rates, transition.rates)
phy <- tree.musse(pars, max.taxa=50, x0=1, include.extinct=FALSE)
sim.dat.true <- data.frame(names(phy$tip.state), phy$tip.state)
sim.dat <- sim.dat.true
# Now to hide the "hidden" state
sim.dat[sim.dat[,2]==3,2] = 1
sim.dat[sim.dat[,2]==4,2] = 2
# and convert states 1,2 to 0,1
sim.dat[,2] = sim.dat[,2] - 1
plot(phy)
knitr::kable(cbind(sim.dat, true.char=sim.dat.true$phy.tip.state))
turnover.anc = c(1,1,0,0)
eps.anc = c(1,1,0,0)
turnover.anc = c(1,2,0,0)
turnover.anc = c(1,2,3,4)
eps.anc = c(0,0,0,0)
trans.rates = TransMatMaker(hidden.states=TRUE)
trans.rates

