### Load the libs 

source("functions.r")

outdir <- "plots/"
sel.k <- c(NA, 3, 20, 100)
runtime.algs <- c("PP", "PM")
ymax <- 30

e150.runs <- load.and.preprocess("empty-unbounded-r150-sgoverlap", maxk=100)
e75.runs <- load.and.preprocess("empty-unbounded-r75", maxk=100)
fb75.runs <- load.and.preprocess("four-boxes-r75", maxk=100)

#### Success ####

e150.success <- successrate.nagents(e150.runs[is.element(e150.runs$k, sel.k),])
e75.success <- successrate.nagents(e75.runs[is.element(e75.runs$k, sel.k),])
fb75.success <- successrate.nagents(fb75.runs[is.element(fb75.runs$k, sel.k),])

success <- arrangeGrob(
  e150.success + theme(legend.position="none") + ggtitle("Success rate on Scenario A"), 
  e75.success + theme(legend.position="none") + ggtitle("Success rate on Scenario B"),
  fb75.success + theme(legend.position="none") + ggtitle("Success rate on Scenario C"),
  ncol=1) 

legend <- get.legend(e150.success + theme(legend.position="bottom"))
lheight <- sum(legend1$heights)
success.w.legend <- arrangeGrob(arrangeGrob(success, legend, heights=unit.c(unit(1, "npc") - lheight, lheight), ncol=1))
ggsave(filename=paste(outdir, "success.pdf", sep=""), plot=success, width=4, height=8)
ggsave(filename=paste(outdir, "success-with-legend.pdf", sep=""), plot=success.w.legend, width=7, height=8)

#### Suboptimality ####

e150.subopt <- suboptimality.vs.k(runs=e150.runs, optalg="OD", algs=c("OD", "PP", "PM"))
e75.subopt <- suboptimality.vs.k(runs=e75.runs, optalg="OD", algs=c("OD", "PP", "PM"))
fb75.subopt <- suboptimality.vs.k(runs=fb75.runs, optalg="OD", algs=c("OD", "PP", "PM"))

legend <- get.legend(e150.subopt + theme(legend.position="bottom"))
lheight <- sum(legend1$heights)

subopt <- arrangeGrob(
  e150.subopt + theme(legend.position="none") + ggtitle("Suboptimality on Scenario A"), 
  e75.subopt + theme(legend.position="none") + ggtitle("Suboptimality on Scenario B"),
  fb75.subopt + theme(legend.position="none") + ggtitle("Suboptimality on Scenario C"),
  ncol=1) 

subopt.w.legend <- arrangeGrob(arrangeGrob(subopt, legend, heights=unit.c(unit(1, "npc") - lheight, lheight), ncol=1))

ggsave(filename=paste(outdir, "subopt.pdf", sep=""), plot=subopt, width=4, height=8)
ggsave(filename=paste(outdir, "subopt-with-legend.pdf", sep=""), plot=subopt.w.legend, width=4, height=8)

#### Average cost ####

avg.cost <- avg.cost.vs.k(e75.runs[e75.runs$nagents==10,], algs=c("PM","PP","ORCA"), divide.cost.by=1000) +
  theme(legend.position="bottom") +
  ggtitle("Time out of goal, Scenario B, 10 robots")
ggsave(filename=paste(outdir, "avgcost.pdf", sep=""), plot=avg.cost, width=4, height=4)

#### Runtime ####

runtime <- avg.runtime.vs.nagents(e75.runs[is.element(e75.runs$k, sel.k),], runtime.algs, ymax=ymax, common.instances.only=TRUE) +
  theme(legend.position="bottom") +
  ggtitle("CPU runtime in Scenario B")

runtime
ggsave(filename=paste(outdir, "runtime.pdf", sep=""), plot=runtime, width=4, height=4)