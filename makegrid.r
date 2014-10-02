### Load the libs 

source("functions.r")

outdir <- "/home/capino/projects/deconfliction/repo-doc/icra-2015-penalty-method/plots/"
sel.k <- c(NA, 3, 20, 100)
runtime.algs <- c("PP", "PM")
ymax <- 30

e150.runs <- load.and.preprocess("empty-unbounded-r150-sgoverlap", maxk=100)
e75.runs <- load.and.preprocess("empty-unbounded-r75", maxk=100)
fb75.runs <- load.and.preprocess("four-boxes-r75", maxk=100)
cp75.runs <- load.and.preprocess("chokepoint-r75", maxk=100)


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

























############################# OLD GRAPHS ####################################### 

e150.success <- successrate.nagents(e150.runs[is.element(e150.runs$k, sel.k),])
e150.relq.od1 <- relquality.vs.k(runs=e150.runs[e150.runs$nagents == 2, ], refalg="OD", optalg="OD", algs=c("OD", "PP", "PM"))

e150.relq.od2 <- relquality.vs.k(runs=e150.runs[e150.runs$nagents == 3, ], refalg="OD", optalg="OD", algs=c("OD", "PP", "PM"))
e150.relq.pp <- relquality.vs.k(runs=e150.runs[e150.runs$nagents == 7, ], refalg="PP", optalg="OD", algs=c("PP", "PM"))
e150.relq.orca <- relquality.vs.k(runs=e150.runs[e150.runs$nagents == 7, ], refalg="ORCA", optalg="OD", algs=c("PP", "PM", "ORCA"))
e150.runtime <- avg.runtime.vs.nagents(e150.runs[is.element(e150.runs$k, sel.k),], runtime.algs, ymax=ymax, common.instances.only=TRUE)

e75.success <- successrate.nagents(e75.runs[is.element(e75.runs$k, sel.k),])
e75.subopt <- suboptimality.vs.k(runs=e75.runs, optalg="OD", algs=c("OD", "PP", "PM"))
e75.relq.od1 <- relquality.vs.k(runs=e75.runs[e75.runs$nagents == 2, ], refalg="OD", optalg="OD", algs=c("OD", "PP", "PM"))
e75.relq.od2 <- relquality.vs.k(runs=e75.runs[e75.runs$nagents == 3, ], refalg="OD", optalg="OD", algs=c("OD", "PP", "PM"))
e75.relq.pp <- relquality.vs.k(runs=e75.runs[e75.runs$nagents == 10, ], refalg="PP", optalg="OD", algs=c("PP", "PM"))
e75.relq.orca <- relquality.vs.k(runs=e75.runs[e75.runs$nagents == 10, ], refalg="ORCA", optalg="OD", algs=c("PP", "PM", "ORCA"))
e75.runtime <- avg.runtime.vs.nagents(e75.runs[is.element(e75.runs$k, sel.k),], runtime.algs, ymax=ymax, common.instances.only=TRUE)


width <- 4
height <- 14

e150.grid <- arrangeGrob(
  e150.success + theme(legend.position="none") , 
  e150.relq.od1 + theme(legend.position="none"),
  e150.relq.od2 + theme(legend.position="none"),
  e150.relq.pp + theme(legend.position="none"),
  e150.relq.orca + theme(legend.position="none"),
  e150.runtime + theme(legend.position="none"),
  ncol=1)
  


e75.grid <- arrangeGrob(
  e75.success + theme(legend.position="none"), 
  e75.relq.od1 + theme(legend.position="none"),
  e75.relq.od2 + theme(legend.position="none"),
  e75.relq.pp + theme(legend.position="none"),
  e75.relq.orca + theme(legend.position="none"),
  e75.runtime + theme(legend.position="none"),
  ncol=1)

ggsave(filename=paste(outdir, "e75.pdf", sep=""), plot=e75.grid, width=width, height=height)

fb75.grid <- arrangeGrob(
  fb75.success + theme(legend.position="none"), 
  fb75.relq.od1 + theme(legend.position="none"),
  fb75.relq.od2 + theme(legend.position="none"),
  fb75.relq.pp + theme(legend.position="none"),
  fb75.relq.orca + theme(legend.position="none"),
  fb75.runtime + theme(legend.position="none"),
  ncol=1)

ggsave(filename=paste(outdir, "fb75.pdf", sep=""), plot=fb75.grid, width=width, height=height)

cp75.grid <- arrangeGrob(
  cp75.success + theme(legend.position="none"), 
  cp75.relq.od1 + theme(legend.position="none"),
  cp75.relq.od2 + theme(legend.position="none"),
  cp75.relq.pp + theme(legend.position="none"),
  cp75.relq.orca + theme(legend.position="none"),
  cp75.runtime + theme(legend.position="none"),
  ncol=1)

ggsave(filename=paste(outdir, "cp75.pdf", sep=""), plot=cp75.grid, width=width, height=height)

cp75.grid


######## ---- legend -----


legend1 <- get.legend(e150.success + theme(legend.position="bottom"))
legend2 <- get.legend(e150.relq.od1 + theme(legend.position="bottom"))

lwidth <- sum(legend1$width)
lheight <- sum(legend1$heights)

grid.legend <- arrangeGrob(legend1, 
  legend2,
  heights=c(1,1),
  ncol=1)

grid.legend
ggsave(filename=paste(outdir, "legend.pdf", sep=""), plot=grid.legend)



ggsave(filename=paste(outdir, "legend.pdf", sep=""), plot=grid.legend)

grid.legend <- arrangeGrob(arrangeGrob(cp75.grid, legend1, heights=unit.c(unit(1, "npc") - lheight, lheight), ncol=1))
ggsave(filename=paste(outdir, "legend.pdf", sep=""), plot=grid.legend)
