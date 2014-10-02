library(plyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

load.and.preprocess <- function(env, maxk=1000) {
  dir <- paste("instances/",env, sep="")  
  runs <- read.csv(file=paste(dir, "/data.out.head", sep=""), head=TRUE, sep=";")
  runs$k[is.na(runs$k)] <- ""
  runs$k <- as.numeric(runs$k)
  runs <- runs[order(runs$instance, runs$alg, runs$k),]
  # rename KSFO to DPM
  runs$alg <- revalue(runs$alg, c("ODCN"="OD", "KDPMD"="PM"))
  
  # make combined field algk rename DPM to DPM(k)
  algk <- as.character(runs$alg)
  algk[algk=="PM"] <- paste("PM(k=", sprintf("%.0f", runs$k[algk=="PM"]), ")", sep="")
  levels <-unique(algk)
  runs$algk <- factor(algk, levels<-levels)
  
  # make string variants of K and nAgents
  kstrs <- as.character(runs$k)
  kstrs[is.na(runs$k)] <- "" 
  kstrs[!is.na(runs$k)] <- paste("k:", runs$k[!is.na(runs$k)])
  runs$kstr <- factor(kstrs, levels <- mysort(unique(kstrs)))
  
  nstrs <- paste("robots:", runs$nagents, sep="")
  runs$nstr <- factor(nstrs, levels <- mysort(unique(nstrs)))  
  
  # Remove DPM(1) and ORCA
  runs <- runs[!(runs$alg=="PM" & runs$k==1), ]
  runs <- runs[!(runs$alg=="PM" & runs$k==2), ]
  runs <- runs[is.na(runs$k) | runs$k <= maxk,]
  
  return(runs)
}

##### Colors and shapes

dpm.palette <- rev(brewer.pal(9, "PuRd"))[1:9]
others.palette <- brewer.pal(4, "Set2")[1:4]

get.color <- function(algs) {
  pal <- c()
  for (alg in algs) {
    if (is.na(alg)) {
      pal <- c(pal, "#888888")
    } else if (alg == "PM") {
      pal <- c(pal, dpm.palette[1])
    } else if (alg == "PM(k=3)") {
      pal <- c(pal, dpm.palette[1])
    } else if (alg == "PM(k=4)") {
      pal <- c(pal, dpm.palette[2])
    } else if (alg == "PM(k=5)") {
      pal <- c(pal, dpm.palette[3])
    } else if (alg == "PM(k=10)") {
      pal <- c(pal, dpm.palette[4])
    } else if (alg == "PM(k=20)") {
      pal <- c(pal, dpm.palette[5])
    } else if (alg == "PM(k=50)") {
      pal <- c(pal, dpm.palette[6])
    } else if (alg == "PM(k=100)") {
      pal <- c(pal, dpm.palette[7])
    } else if (alg == "PM(k=200)") {
      pal <- c(pal, dpm.palette[8])
    } else if (alg == "PM(k=250)") {
      pal <- c(pal, dpm.palette[8])
    } else if (alg == "PP") {
      pal <- c(pal, others.palette[2])
    } else if (alg == "OD") {
      pal <- c(pal, others.palette[1])
    } else if (alg == "ORCA") {
      pal <- c(pal, others.palette[3])
    } else {
      pal <- c(pal, "#000000")
    }    
  }
  return(pal)
}

get.shape <- function(algs) {
  pal <- c()
  for (alg in algs) {
    if (is.na(alg)) {
      pal <- c(pal, "13")
    } else if (substring(alg,1,2) == "PM") {
      pal <- c(pal, 1)
    } else if (alg == "PP") {
      pal <- c(pal, 2)
    } else if (alg == "OD") {
      pal <- c(pal, 3)
    } else if (alg == "ORCA") {
      pal <- c(pal, 4)
    } else {
      pal <- c(pal, 14)
    }  
  }
  return(pal)
}

## successrate ~ nagents ##

successrate.nagents <- function(runs) {
  
  xbreaks <- unique(runs$nagents)
  
  successrate <- ddply(runs, .(nagents, algk, alg, kstr), summarise,                     
                       successrate = sum(status=="SUCCESS") / length(unique(instance))
  )
  
  plot <- ggplot(successrate, aes(nagents, successrate*100, color=algk, shape=algk)) + 
    geom_point(size=3) + geom_line() +
    scale_y_continuous(name=("Instances solved [%]")) +  
    scale_x_continuous(name=("No of robots"), breaks=xbreaks) +
    scale_color_manual(values=get.color(unique(successrate$algk)), name="method") +
    scale_shape_manual(values=get.shape(unique(successrate$algk)), name="method") +
    ggtitle("Success rate") +
    theme_bw()
  
  return(plot)
}

successcount.nagents <- function(runs) {
  
  xbreaks <- unique(runs$nagents)
  
  successcount <- ddply(runs, .(nagents, algk, alg, kstr), summarise,                     
                        successcount = sum(status=="SUCCESS")
  )
  
  plot <- ggplot(successcount, aes(nagents, successcount, color=algk, shape=algk)) + 
    geom_point(size=3) + geom_line() +
    scale_y_continuous(name=("No of instances solved [-]")) +  
    scale_x_continuous(name=("No of robots"), breaks=xbreaks) +
    scale_color_manual(values=get.color(unique(successcount$algk)), name="method") +
    scale_shape_manual(values=get.shape(unique(successcount$algk)), name="method") +
    ggtitle("Success rate") +
    theme_bw()
  
  return(plot)
}

premature.termination.count.nagents <- function(runs) {
  
  successcount <- ddply(runs, .(nagents, algk, alg, kstr), summarise,                     
                        successcount = sum(status=="TIMEOUT" | status=="OUTOFMEMORY")
  )
  
  plot <- ggplot(successcount, aes(nagents, successcount, color=algk, shape=algk)) + 
    geom_point(size=3) + geom_line() +
    scale_y_continuous(name=("No of premature terminations [-]")) +  
    scale_x_continuous(name=("No of robots")) +
    scale_color_manual(values=get.color(unique(successcount$algk)), name="method") +
    scale_shape_manual(values=get.shape(unique(successcount$algk)), name="method") +
    theme_bw()
  
  return(plot)
}

# successrate.nagents <- function(runs, palette) {
#   
#   successrate <- ddply(runs, .(nagents, alg, k), summarise,                     
#                        successrate = (sum(is.finite(cost)) / length(unique(instance)))
#   )
#   
#   ggplot(successrate,aes(nagents,successrate, color=alg, linetype=factor(k), shape=factor(k))) + 
#     geom_point() + geom_line() +
#     scale_y_continuous(name=("Instances solved [%]")) +  
#     scale_x_discrete(name=("No of robots")) +
#     scale_shape_discrete(name="method") + 
#     scale_linetype_discrete(name="k") + 
#     scale_color_manual(values=palette, name="method") +
#     theme_bw()
# }

avg.runtime.vs.nagents <- function(runs, algs, ymax, common.instances.only=TRUE) {
  xmax <- max(runs$nagents)
  xbreaks <- unique(runs$nagents)
  
  if (common.instances.only == TRUE) {
    solved.by.all <- select.solved.by.all(runs, algs)
    common.runs <- runs[is.element(runs$instance, solved.by.all) & (is.element(runs$alg,algs)), ] 
  } else {
    common.runs <- runs[is.element(runs$alg,algs) & runs$status=="SUCCESS", ]
  } 
  
  agr <- ddply(common.runs, .(nagents, algk), summarise,                     
               runtime.mean = mean(runtime, na.rm=TRUE) / 1000,
               runtime.se = (sd(runtime, na.rm=TRUE)/sqrt(length(runtime)))/ 1000
  )  
  
  plot <- ggplot(agr, aes(x=nagents, y=runtime.mean, color=algk, shape=algk, ymin=runtime.mean-runtime.se, ymax=runtime.mean+runtime.se)) + 
    geom_point() + geom_line() + geom_errorbar(width=0.5) +
    scale_y_continuous(name=("Avg. CPU runtime [s]")) +  
    scale_x_continuous(name=("No of robots"), breaks=xbreaks) +
    scale_linetype_discrete(name="k") + 
    scale_color_manual(values=get.color(unique(agr$algk)), name="") +
    scale_shape_manual(values=get.shape(unique(agr$algk)), name="") +
    coord_cartesian(ylim = c(0, ymax), xlim = c(0, xmax)) +
    theme_bw() +
    ggtitle("Avg. runtime to solution")
  
  return(plot)
}

avg.runtime.per.replan.vs.nagents <- function(runs, algs, ymax, common.instances.only=TRUE) {
  if (common.instances.only == TRUE) {
    solved.by.all <- select.solved.by.all(runs, algs)
    common.runs <- runs[is.element(runs$instance, solved.by.all) & (is.element(runs$alg,algs)), ] 
  } else {
    common.runs <- runs[is.element(runs$alg,algs) & runs$status=="SUCCESS", ]
  } 
  
  common.runs$k[common.runs$alg=="PP"] <- 1
  
  agr <- ddply(common.runs, .(nagents, algk), summarise,                     
               runtime.mean = mean(runtime/(nagents*k), na.rm=TRUE) / 1000,
               runtime.se = (sd(runtime/(nagents*k), na.rm=TRUE)/sqrt(length(runtime)))/ 1000
  )  
  
  plot <- ggplot(agr, aes(x=nagents, y=runtime.mean, color=algk, shape=algk, ymin=runtime.mean-runtime.se, ymax=runtime.mean+runtime.se)) + 
    geom_point() + geom_line() + geom_errorbar(width=0.1) +
    scale_y_continuous(name=("Avg. runtime per replan [s]")) +  
    scale_x_continuous(name=("No of robots")) +
    scale_linetype_discrete(name="k") + 
    scale_color_manual(values=get.color(unique(agr$algk)), name="method") +
    scale_shape_manual(values=get.shape(unique(agr$algk)), name="method") +
    coord_cartesian(ylim = c(0, ymax)) +
    theme_bw()
  
  return(plot)
}

successrate.vs.runtime.nagents <- function(runs, maxtime, step=500, palette) {  
  succ.rate <- data.frame()
  algs <- unique(runs$alg)
  i <- 1
  for (nagents in unique(runs$nagents)) {
    print(paste("nagents: ", nagents))
    ninstances <- length(unique(runs[runs$nagents == nagents,"instance"]))
    for (t in seq(0, maxtime, by=step)) {    
      print(t)      
      for (alg in algs) {
        for (k in unique(runs[runs$alg==alg,"k"])) {        
          succ.rate[i, "time"] <- t    
          succ.rate[i, "nagents"] <- paste("robots: ", nagents)
          succ.rate[i, "alg"] <- alg  
          succ.rate[i, "k"] <- k
          if (is.na(k)) {
            succ.rate[i, "algk"] <- unique(runs$algk[runs$alg==alg])
          } else {
            succ.rate[i, "algk"] <- unique(runs$algk[runs$alg==alg & runs$k==k]) 
          }          
          succ.rate[i, "successrate"] <- length(runs[runs$alg==alg & runs$nagents==nagents & runs$k==k & is.finite(runs$cost) & runs$runtime <= t, "instance"]) # / ninstances    
          i <- i + 1        
        }
      }
    }
  }
  
  succ.rate$nagents <-factor(succ.rate$nagents,  levels<-mysort(unique(succ.rate$nagents)))
  
  ggplot(succ.rate, aes(time/1000, successrate, color=algk, shape=alg)) + 
    scale_x_continuous(name="runtime [s]") + scale_y_continuous(name="no of instances solved") +
    scale_shape_discrete(name="method") + scale_linetype_discrete(name="k") + scale_color_manual(values=palette, name="method") + 
    geom_line() + geom_point() +facet_wrap(~nagents, ncol=4) + theme_bw()
}

successrate.vs.runtime.gridstep <- function(runs, algs, maxtime, step=500) {  
  succ.rate <- data.frame()
  
  i <- 1
  for (gridstep in unique(runs$gridstep)) {
    print(paste("gridstep: ", gridstep))
    for (gridpattern in unique(runs$gridpattern)) {
      print(paste("gridpattern: ", gridpattern))
      ninstances <- length(unique(runs[runs$gridstep == gridstep & runs$gridpattern == gridpattern, "instance"]))
      for (t in seq(0, maxtime, by=step)) {    
        print(t)      
        for (alg in algs) {
          for (k in unique(runs[runs$alg==alg,"k"])) {        
            succ.rate[i, "time"] <- t    
            succ.rate[i, "gridstep"] <- gridstep
            succ.rate[i, "gridpattern"] <- gridpattern
            succ.rate[i, "alg"] <- alg  
            succ.rate[i, "k"] <- k
            succ.rate[i, "algk"] <- paste(alg, sprintf("%04d", k))
            succ.rate[i, "successrate"] <- length(runs[runs$alg==alg & runs$gridstep==gridstep & runs$gridpattern==gridpattern &runs$k==k & is.finite(runs$cost) & runs$runtime <= t, "instance"])
            i <- i + 1        
          }
        }
      }
    }
  }  
  
  ggplot(succ.rate, aes(time/1000, successrate, color=algk, linetype=factor(k), shape=factor(alg))) +
    scale_shape_discrete(solid=FALSE) + scale_linetype_discrete(name="k") + 
    geom_point() + geom_line() + facet_grid(gridpattern~gridstep) 
}

successrate.vs.runtime <- function(runs, maxtime, algs, step=500) {
  ninstances <- length(unique(runs[,"instance"]))
  
  succ.rate <- data.frame()
  
  i <- 1
  for (t in seq(0, maxtime, by=step)) {    
    print(t)
    for (alg in algs) {
      for (k in unique(runs[runs$alg==alg,"k"])) {        
        succ.rate[i, "time"] <- t    
        succ.rate[i, "alg"] <- alg  
        succ.rate[i, "k"] <- k
        succ.rate[i, "successrate"] <- length(runs[runs$alg==alg & runs$k==k & is.finite(runs$cost) & runs$runtime <= t, "instance"]) / ninstances    
        i <- i + 1
      }
    }
  }
  
  ggplot(succ.rate, aes(time, successrate, color=alg, linetype=factor(k), shape=factor(k))) + geom_point() + geom_line()
}


### first and best solution for the algorithms
successrate.vs.expansions <- function(runs, maxexpansions, algs, step=500) {
  ninstances <- length(unique(runs[,"instance"]))
  
  succ.rate <- data.frame()
  
  i <- 1
  for (exp in seq(0, maxexpansions, by=step)) {    
    for (alg in algs) {
      for (k in unique(runs[runs$alg==alg,"k"])) {        
        succ.rate[i, "exp"] <- exp    
        succ.rate[i, "alg"] <- alg  
        succ.rate[i, "k"] <- k
        succ.rate[i, "successrate"] <- length(runs[runs$alg==alg & runs$k==k & is.finite(runs$cost) & runs$expansions <= exp, "instance"]) / ninstances    
        i <- i + 1
      }
    }
  }
  
  ggplot(succ.rate, aes(exp, successrate, color=alg, linetype=factor(k))) + geom_point() + geom_line()  
}

cost.per.instance <- function(runs) {
  firstinstance <- min(runs$instance, na.rm=TRUE)
  costs <- data.frame(id=runs$instance-firstinstance, cost=runs$cost, instance=runs$instance, alg=runs$alg, k=as.factor(runs$k))
  costs <- costs[is.finite(costs$cost),]
  ggplot(costs, aes(id, cost, shape=alg, color=k)) + scale_x_discrete(name="") + scale_shape_discrete(solid=F) + geom_point(size=5) + geom_text(aes(y=cost+40, size=1, label=instance))
}

runtime.per.instance <- function(runs) {
  firstinstance <- min(runs$instance, na.rm=TRUE)
  runtimes <- data.frame(id=runs$instance-firstinstance, runtime=runs$runtime, instance=runs$instance, alg=runs$alg, k=as.factor(runs$k))
  ggplot(runtimes, aes(id, runtime, shape=alg, color=k)) + scale_x_discrete(name="") + scale_shape_discrete(solid=F) + geom_point(size=5) + geom_text(aes(y=runtime+40, size=1, label=instance))
}

expansions.per.instance <- function(runs) {
  firstinstance <- min(runs$instance, na.rm=TRUE)
  expansions <- data.frame(id=runs$instance-firstinstance, expansions=runs$expansions, instance=runs$instance, alg=runs$alg, k=as.factor(runs$k))
  ggplot(expansions, aes(id, expansions, shape=alg, color=k)) + scale_x_discrete(name="") + scale_shape_discrete(solid=F) + geom_point(size=5) + geom_text(aes(y=expansions+40, size=1, label=instance))
}

select.solved.by.all <- function(runs, algs) {
  solved.by.all <- unique(runs[,"instance"])
  for (alg in algs) {
    ks <- unique(runs[runs$alg==alg,"k"])
    if (length(ks) == 1) {
      solved.by.all <- intersect(solved.by.all, unique(runs[runs$alg==alg & is.finite(runs$cost), "instance"]))  
    } else {
      for (k in ks) {
        solved.by.all <- intersect(solved.by.all, unique(runs[runs$alg==alg & runs$k == k & is.finite(runs$cost), "instance"]))                              
      }
    }
  }
  return(solved.by.all)
}

suboptimality.vs.k <- function(runs, optalg="OD", algs=c("PP")) {
  algs.no.k <- unique(runs[(is.element(runs$alg, algs)| runs$alg == optalg) & is.na(runs$k), "alg"])
  
  solved.by.all <- select.solved.by.all(runs, algs)  
  common.runs <- runs[is.element(runs$instance, solved.by.all) & (is.element(runs$alg,algs) | runs$alg == optalg), ] 
  common.runs$k[is.na(common.runs$k)] <- 0
  
  for (instance in solved.by.all) {
    optquality <- common.runs[common.runs$instance == instance & common.runs$alg == optalg, "cost"]
    common.runs$rel.cost[common.runs$instance == instance] <- 
      (common.runs$cost[common.runs$instance == instance] - optquality) / optquality
  }
  
  agr <- ddply(common.runs, .(alg, k, kstr), summarise,                     
               relcost.mean = mean(-rel.cost)*100,
               relcost.se = (sd(-rel.cost)/sqrt(length(rel.cost)))*100,
               n = length(rel.cost)
  )  
  
  agr$relcost.mean.hline[is.element(agr$alg,algs.no.k)] <- agr$relcost.mean[is.element(agr$alg,algs.no.k)]
  agr$annotation[is.element(agr$alg,algs.no.k)] <- as.character(agr$alg[is.element(agr$alg,algs.no.k)])
  agr$annotation[agr$alg==optalg] <- "OPTIMUM"  
  
  plot <- ggplot(agr, aes(x=k, y=relcost.mean, color=alg, shape=alg, ymin=relcost.mean-relcost.se, ymax=relcost.mean+relcost.se)) + 
    geom_point() + geom_line() + geom_errorbar(width=2) + 
    geom_hline(aes(yintercept=relcost.mean.hline, color=alg), linetype="dashed") +
    scale_y_continuous(name=paste("Suboptimality [%]"))+
    scale_x_continuous(name="k")+
    scale_color_manual(values=get.color(unique(agr$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(agr$alg)), name="method") +
    geom_text(aes(y=relcost.mean.hline, label=annotation), x=50, show_guide=F) +
    annotate("text", x = max(agr$k, na.rm=TRUE)/2, y = min(agr$relcost.mean-agr$relcost.se), label=paste("(avg. from ", length(solved.by.all)  ," instances)", sep="")) +
    theme_bw()
    
    plot
  
    plot <- plot + ggtitle("Suboptimality")
  
  return(plot)
}

avg.cost.vs.k <- function(runs, algs=c("PP"), divide.cost.by) {
  algs.no.k <- unique(runs[(is.element(runs$alg, algs)) & is.na(runs$k), "alg"])
  
  solved.by.all <- select.solved.by.all(runs, algs)  
  common.runs <- runs[is.element(runs$instance, solved.by.all) & is.element(runs$alg,algs), ] 
  common.runs$k[is.na(common.runs$k)] <- 0
  
  common.runs$cost <-  common.runs$cost / divide.cost.by
  
  agr <- ddply(common.runs, .(alg, k, kstr), summarise,                     
               cost.mean = mean(cost),
               cost.se = (sd(cost)/sqrt(length(cost))),
               n = length(cost)
  )  
  
  agr$cost.mean.hline[is.element(agr$alg,algs.no.k)] <-agr$cost.mean[is.element(agr$alg,algs.no.k)]
  
  agr$annotation[is.element(agr$alg,algs.no.k)] <- as.character(agr$alg[is.element(agr$alg,algs.no.k)])
  
  plot <- ggplot(agr, aes(x=k, y=cost.mean, color=alg, shape=alg, ymin=cost.mean-cost.se, ymax=cost.mean+cost.se)) + 
    geom_point() + geom_line() + geom_errorbar(width=2) + 
    geom_hline(aes(yintercept=cost.mean.hline, color=alg), linetype="dashed") +
    geom_text(aes(y=cost.mean.hline, label=annotation), x=50, show_guide=F) +
    scale_y_continuous(name="Avg. time outside goal [s]")+
    scale_x_continuous(name="k")+
    scale_color_manual(values=get.color(unique(agr$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(agr$alg)), name="method") +
    annotate("text", x = max(agr$k, na.rm=TRUE)/2, y = max(agr$cost.mean+agr$cost.se), label=paste("(avg. from ", length(solved.by.all)  ," instances)", sep="")) +
    theme_bw()
  
  return(plot)
}

relquality.vs.k <- function(runs, refalg="OD", optalg="OD", algs=c("PP")) {
  algs.no.k <- unique(runs[(is.element(runs$alg, algs)| runs$alg == optalg) & is.na(runs$k), "alg"])
  
  solved.by.all <- select.solved.by.all(runs, algs)  
  common.runs <- runs[is.element(runs$instance, solved.by.all) & (is.element(runs$alg,algs) | runs$alg == optalg), ] 
  
  for (instance in solved.by.all) {
    refquality <- common.runs[common.runs$instance == instance & common.runs$alg == refalg, "cost"] 
    optquality <- common.runs[common.runs$instance == instance & common.runs$alg == optalg, "cost"]
    common.runs$rel.cost[common.runs$instance == instance] <- 
      (common.runs$cost[common.runs$instance == instance] - refquality) / refquality
  }
  
  agr <- ddply(common.runs, .(nstr, alg, k, kstr), summarise,                     
               relcost.mean = mean(rel.cost)*100,
               relcost.se = (sd(rel.cost)/sqrt(length(rel.cost)))*100,
               n = length(rel.cost)
  )  
  
  agr$relcost.mean.hline[is.element(agr$alg,algs.no.k)] <-agr$relcost.mean[is.element(agr$alg,algs.no.k)]
  agr$nstrsamples <- paste(agr$nstr, " instances:", agr$n, "", sep="")
  
  plot <- ggplot(agr, aes(x=k, y=relcost.mean, color=alg, shape=alg, ymin=relcost.mean-relcost.se, ymax=relcost.mean+relcost.se)) + 
    geom_point() + geom_line() + geom_errorbar(width=0.2) + 
    geom_hline(aes(yintercept=relcost.mean.hline, color=alg), linetype="dashed") +
    scale_y_continuous(name=paste("Avg. difference to", refalg, "[%]"))+
    scale_x_continuous(name="k")+
    scale_color_manual(values=get.color(unique(agr$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(agr$alg)), name="method") +
    facet_grid(nstrsamples ~ .) + theme_bw()
    
  if (refalg=="OD") {
    plot <- plot + ggtitle("Suboptimality")
  } else {
    plot <- plot + ggtitle(paste("Relative quality to",refalg))
  } 
  
  return(plot)
}

relative.quality <- function(runs, algs=c("PP","KSFO")) {
  
  solved.by.all <- unique(runs[,"instance"])
  for (alg in algs) {
    ks <- unique(runs[runs$alg==alg,"k"])
    for (k in ks) {
      solved.by.all <- intersect(solved.by.all, 
                                 unique(runs[runs$alg==alg & runs$k == k & is.finite(runs$cost), "instance"]))                              
    }                          
  }
  
  common.runs <- runs[is.element(runs$instance, solved.by.all) & is.element(runs$alg,algs), ] 
  
  for (instance in solved.by.all) {
    refquality <- common.runs[common.runs$instance == instance 
                              & common.runs$alg == algs[1], "cost"]   
    common.runs$cost[common.runs$instance == instance] <- 
      (common.runs$cost[common.runs$instance == instance] - refquality) / refquality
  }
  
  ggplot(common.runs, aes(x=cost, fill=alg )) + geom_histogram(binwidth=.02) + facet_grid(alg~k)
  #  ggplot(common.runs, aes(instance, cost, color=alg, shape=factor(k) )) + geom_point()
  
  
}

relative.quality.nagents <- function(runs, algs=c("PP","PM"), binwidth, palette) {
  
  solved.by.all <- unique(runs[,"instance"])
  for (alg in algs) {
    ks <- unique(runs[runs$alg==alg, "k"])
    if (is.na(ks[1])) {
      solved.by.all <- intersect(solved.by.all, 
                                 unique(runs[runs$alg==alg & is.finite(runs$cost), "instance"]))
    } else {
      for (k in ks) {     
        solved.by.all <- intersect(solved.by.all, 
                                   unique(runs[runs$alg==alg & runs$k == k & is.finite(runs$cost), "instance"]))      
      } 
    }                        
  }
  
  common.runs <- runs[is.element(runs$instance, solved.by.all) & is.element(runs$alg,algs), ] 
  
  ref.alg <- algs[1]
  
  for (instance in solved.by.all) {    
    refquality <- common.runs[common.runs$instance == instance & common.runs$alg == ref.alg, "cost"]   
    common.runs$cost[common.runs$instance == instance] <- 
      ((common.runs$cost[common.runs$instance == instance] - refquality) / refquality) * 100  
  }
  
  breakfun <-function(lim) {
    if (abs(lim[1]) < 20) {
      return(c(0,trunc(lim[2]/10)*10))
    } else {
      lim <- lim*0.9
      return(c(0,trunc(lim/10)*10))
    }
  }
  
  ggplot(common.runs, aes(x=cost, fill=algk )) +
    scale_x_continuous(name=paste("solution cost relative to", ref.alg, "[%]"), breaks=breakfun) + 
    scale_y_continuous(name="no of instances in a bin") +
    geom_vline(xintercept = binwidth/2, linetype = "longdash", colour="black", alpha=0.5) + 
    geom_histogram(binwidth=binwidth, position="identity") + 
    scale_fill_manual(values=palette, name="method") +
    facet_grid(kstr~nstr) + theme_bw()  
}

relative.quality.gridstep <- function(runs, algs=c("PP","PM"), binwidth) {
  
  solved.by.all <- unique(runs[,"instance"])
  for (alg in algs) {
    ks <- unique(runs[runs$alg==alg, "k"])
    for (k in ks) {
      solved.by.all <- intersect(solved.by.all, 
                                 unique(runs[runs$alg==alg & runs$k == k & is.finite(runs$cost), "instance"]))                              
    }                          
  }
  
  common.runs <- runs[is.element(runs$instance, solved.by.all) & is.element(runs$alg,algs), ] 
  
  ref.alg <- algs[1]
  
  for (instance in solved.by.all) {
    
    refquality <- common.runs[common.runs$instance == instance 
                              & common.runs$alg == ref.alg, "cost"]   
    common.runs$cost[common.runs$instance == instance] <- 
      (common.runs$cost[common.runs$instance == instance] - refquality) / refquality  
    
    # add to the data frame dummy rows with the reference algorithm --
    dummy <- common.runs[common.runs$instance==instance & common.runs$alg==ref.alg, ]
    
    for (k in setdiff(ks,c(1))) {
      dummy.k <- dummy
      dummy.k$k <- k      
      common.runs <- rbind(common.runs, dummy.k)
    }    
  }
  
  common.runs$alg <- factor(common.runs$alg, levels=c(algs))
  
  ggplot(common.runs, aes(x=cost, fill=alg )) + 
    geom_vline(xintercept = binwidth/2, linetype = "longdash", colour="black", alpha=0.5) + 
    geom_histogram(binwidth=binwidth, position="identity") + facet_wrap(k~gridstep)  
}

mysort <- function(x) {
  f <- function(x) {
    if (x=="") {
      return(0)
    }
    if (length(grep(" ",x)) != 0) {
      return(strsplit(x, " ")[[1]][2])
    } else {
      return(999)
    }    
  }
  numbers <- as.numeric(unlist(lapply(x, f)))
  xsorted <- x[order(numbers)]
  return(xsorted)
}

get.legend<-function(plot){
  tmp <- ggplotGrob(plot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

###################################################
#  Experiments
###################################################

expansions.per.replanning <- function(runs) {
  
  succ.runs <- runs[is.finite(runs$cost), ]
  succ.runs$exp.per.repl <- succ.runs$expansions / (succ.runs$k*succ.runs$nagents)
  
  ggplot(succ.runs, aes(instance, exp.per.repl, shape=factor(k), color=alg)) + scale_y_continuous(limits=c(0,500)) +
    scale_x_discrete(name="") + geom_point(size=4) + scale_shape_discrete(solid=F) + facet_grid(k~nagents)
}

#expansions.per.replanning(runs[(runs$alg=="KSFO" | runs$alg=="PP")  & runs$radius==70,])

runtime.per.replanning <- function(runs) {
  maxruntime <- max(runs$runtime)
  succ.runs <- runs[is.finite(runs$cost),]
  succ.runs$rt.per.repl <- succ.runs$runtime / (succ.runs$k*succ.runs$nagents)
  succ.runs$maxtime.per.repl <- maxruntime / (succ.runs$k*succ.runs$nagents)
  
  ggplot(succ.runs, aes(instance, rt.per.repl, shape=factor(k), color=alg)) +
    scale_x_discrete(name="") + 
    #scale_y_continuous(limits=c(0,1000)) +
    geom_point(size=2) + 
    scale_shape_discrete(solid=F) + 
    geom_line(aes(instance, maxtime.per.repl), color="blue") + 
    facet_grid(k~nagents)
}

avg.runtime.per.replanning <- function(runs) {
  maxruntime <- max(runs$runtime)
  succ.runs <- runs[is.finite(runs$cost),]
  succ.runs$rt.per.repl <- succ.runs$runtime / (succ.runs$k*succ.runs$nagents)
  succ.runs$maxtime.per.repl <- maxruntime / (succ.runs$k*succ.runs$nagents)
  
  ggplot(succ.runs, aes(instance, rt.per.repl, shape=factor(k), color=alg)) +
    scale_x_discrete(name="") + 
    #scale_y_continuous(limits=c(0,1000)) +
    geom_point(size=2) + 
    scale_shape_discrete(solid=F) + 
    geom_line(aes(instance, maxtime.per.repl), color="blue") + 
    facet_grid(k~nagents)
}

#runtime.per.replanning(runs[(runs$alg=="KSFO" | runs$alg=="PP")  & runs$radius==70,])