devtools::install_github("thej022214/hisse")
library(hisse)
speciation.rates2 <- c(0.2, 0.3, 0.2, 0.3) #0A, 1A, 0B, 1B
extinction.rates2 <- rep(0.1, 2)
transition.rates2 <- c(0.01,0.01,0, 0.01, 0, 0.01, 0.01,0,0.01, 0,0.01,0.01)
pars2 <- c(speciation.rates2, extinction.rates2, transition.rates2)
phy2 <- diversitree::tree.musse(pars2, max.taxa=50, x0=1, include.extinct=FALSE)
sim.dat.true2 <- data.frame(names(phy$tip.state), phy$tip.state)
sim.dat2 <- sim.dat.true2
# Now to hide the "hidden" state
sim.dat[sim.dat[,2]==3,2] = 1
sim.dat[sim.dat[,2]==4,2] = 2
# and convert states 1,2 to 0,1
sim.dat[,2] = sim.dat[,2] - 1

plot(phy2)

knitr::kable(cbind(sim.dat2, true.char=sim.dat.true$phy.tip.state))

turnover.anc = c(1,1,0,0)
eps.anc = c(1,1,0,0)
turnover.anc = c(1,2,0,0)
turnover.anc = c(1,2,3,4)
eps.anc = c(0,0,0,0)
trans.rates = TransMatMaker(hidden.states=TRUE)
trans.rates
trans.rates.nodual = ParDrop(trans.rates, c(3,5,8,10))
trans.rates.nodual
trans.rates.nodual.equal16 = ParEqual(trans.rates.nodual, c(1,6))
trans.rates.nodual.equal16
trans.rates.nodual.allequal = ParEqual(trans.rates.nodual, c(1,2,1,3,1,4,1,5,1,6,1,7,1,8))
trans.rates.nodual.allequal
trans.rates.nodual.allequal = trans.rates.nodual
trans.rates.nodual.allequal[!is.na(trans.rates.nodual.allequal) & !trans.rates.nodual.allequal == 0] = 1
trans.rates.nodual.allequal
trans.rates.bisse = TransMatMaker(hidden.states=FALSE)
trans.rates.bisse
pp = hisse(phy, sim.dat, f=c(1,1), hidden.states=TRUE, turnover.anc=turnover.anc, 
           eps.anc=eps.anc, trans.rate=trans.rates.nodual.allequal)
turnover.anc = c(1,2,0,3)
eps.anc = c(1,2,0,3)
trans.rates <- TransMatMaker(hidden.states=TRUE)
trans.rates.nodual.no0B <- ParDrop(trans.rates, c(2,3,5,7,8,9,10,12))
trans.rates.nodual.no0B
pp = hisse(phy, sim.dat, f=c(1,1), hidden.states=TRUE, turnover.anc=turnover.anc, 
           eps.anc=eps.anc, trans.rate=trans.rates.nodual.allequal, output.type="net.div")
turnover.anc = c(1,1,2,2)
eps.anc = c(1,1,2,2)
trans.rates = TransMatMaker(hidden.states=TRUE)
trans.rates.nodual = ParDrop(trans.rates, c(3,5,8,10))
trans.rates.nodual.allequal = ParEqual(trans.rates.nodual, c(1,2,1,3,1,4,1,5,1,6,1,7,1,8))
trans.rates.nodual.allequal
# Now we want three specific rates:
trans.rates.nodual.threerates <- trans.rates.nodual
# Set all transitions from 0->1 to be governed by a single rate:
to.change <- cbind(c(1,3), c(2,4))
trans.rates.nodual.threerates[to.change] = 1
# Now set all transitions from 1->0 to be governed by a single rate:
to.change <- cbind(c(2,4), c(1,3))
trans.rates.nodual.threerates[to.change] = 2
# Finally, set all transitions between the hidden state to be a single rate (essentially giving 
# you an estimate of the rate by which shifts in diversification occur:
to.change <- cbind(c(1,3,2,4), c(3,1,4,2))
trans.rates.nodual.threerates[to.change] = 3
trans.rates.nodual.threerates
library(hisse)
pp = hisse(phy, sim.dat, f=c(1,1), hidden.states=TRUE, turnover.anc=turnover.anc, 
           eps.anc=eps.anc, trans.rate=trans.rates.nodual.allequal)
{r global_options, include=FALSE}
knitr::opts_chunk$set(fig.width=7, fig.height=5)
load("testrecon1.Rsave")
class(pp.recon)
pp.recon
plot.hisse.states(pp.recon, rate.param="net.div", show.tip.label=FALSE)
plot.hisse.states(pp.recon, rate.param="net.div", show.tip.label=FALSE, rate.range=c(0,0.072))
pp.recon$aic
pp.recon = MarginRecon(phy, sim.dat, f=c(1,1), hidden.states=TRUE, pars=pp$solution, 
                       aic=pp$aic, n.cores=2)
hisse.results.list = list()
load("testrecon1.Rsave")
hisse.results.list[[1]] = pp.recon
load("testrecon2.Rsave")
hisse.results.list[[2]] = pp.recon
load("testrecon3.Rsave")
hisse.results.list[[3]] = pp.recon
# Now supply the list the plotting function
plot.hisse.states(hisse.results.list, rate.param="net.div", show.tip.label=FALSE, rate.range=c(0,0.072))
# First, suck in all the files with .Rsave line ending in your working directory:
files = system("ls -1 | grep .Rsave", intern=TRUE)
# Create an empty list object
hisse.results.list = list()
# Now loop through all files, adding the embedded pp.recon object in each
for(i in sequence(length(files))){
  load(files[i])
  hisse.results.list[[i]] = pp.recon
  rm(pp.recon)
}