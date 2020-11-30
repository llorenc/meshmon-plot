## Format csv data gathered from qMp nodes in GuifiSants
## http://dsg.ac.upc.edu/qmpsu/index.php
## meshmon-links.R
## (c) Llorenç Cerdà-Alabern, Nov 2020.

source("meshmon-lib.R")
source("meshmon-plot.R")
library(Hmisc) # latex

options("width"=200)
csvdir <- "../csv/"

## mmdata <- read.csv(fname, header=T, row.names="rp", sep=',', na.strings = "", stringsAsFactors=F))

uid.db <- fromJSON(file="../mmdata-uid-db.json")

date <- "20-09"
mmdata.df <- list()
mmdata.list <- list()
## read csv as data.frame
mmdata.df[['wifi']] <-
  read.rds.file(paste0("RData/", date, "-mmdata-wifi-df.rds"),
                call("build.mmdata.df", paste0(csvdir, date, "-meshmon-links_wifi.csv.gz")))
mmdata.df[['eth']] <-
  read.rds.file(paste0("RData/", date, "-mmdata-eth-df.rds"),
                call("build.mmdata.df", paste0(csvdir, date, "-meshmon-links_eth.csv.gz")))
mmdata.df[['ifaces']] <-
  read.rds.file(paste0("RData/", date, "-mmdata-ifaces-df.rds"),
                call("build.mmdata.df", paste0(csvdir, date, "-meshmon-ifaces.csv.gz")))
mmdata.df[['state']] <-
  read.rds.file(paste0("RData/", date, "-mmdata-state-df.rds"),
                call("build.mmdata.df", paste0(csvdir, date, "-meshmon-state.csv.gz")))

## group link data as list[[src]][[dst]]
mmdata.list[['wifi']] <- 
  read.rds.file(paste0("RData/", date, "-mmdata-wifi-list.rds"),
                call("split.link.by.node", mmdata.df[['wifi']]))
mmdata.list[['eth']] <- 
  read.rds.file(paste0("RData/", date, "-mmdata-eth-list.rds"),
                call("split.link.by.node", mmdata.df[['eth']]))
mmdata.list[['ifaces']] <- 
  read.rds.file(paste0("RData/", date, "-mmdata-ifaces-list.rds"),
                call("split.link.by.node", mmdata.df[['ifaces']]))
## group node data as list[[src]]
mmdata.list[['state']] <- 
  read.rds.file(paste0("RData/", date, "-mmdata-state-list.rds"),
                call("split.state.by.node", mmdata.df[['state']]))

##
links.wifi <- choose.links("20-09", mmdata.list[['wifi']], count=50)
links.wifi
links.ifaces <- choose.links("20-09", mmdata.list[['ifaces']], count=50)
links.ifaces

mmdata.ifaces <- get.links(mmdata.list[['ifaces']], links.ifaces)

head(mmdata.list[['ifaces']][[1]][[1]])

links.wifi.r <- choose.links.by.routes("20-09", mmdata.list[['wifi']], count=25)
links.wifi.r
links.eth.r <- choose.links.by.routes("20-09", mmdata.list[['eth']], count=25)
links.eth.r

## mmdata.list[['ifaces']] <- split.by.node(mmdata.df[['ifaces']])

##
## wifi table
##
link <- get.links.mean(mmdata.list[['wifi']][['66']], c('routes', 'txRate', 'rxRate', 'throughput'))
for(m in c('routes', 'txRate', 'rxRate', 'throughput')) {
  link[,m] <- latex.format(link[,m])
}
link <- cbind(name=sapply(link$dst, uid2label), link)
headings <- c('name', 'dst.uid', 'Mbps', 'routes', 'txRate', 'rxRate', 'samples')
rownames(link) <- NULL
latex(link[,c('name', 'dst', 'throughput', 'routes', 'txRate', 'rxRate', 'throughput.samples')],
      booktabs=TRUE, size="small", table.env=FALSE, , colheads=headings)

##
## ifaces table
##
link <- get.links.mean(mmdata.list[['ifaces']][['66']], c('throughput'))
link[,'dst'] <- gsub('_', '\\\\_', link[,'dst'])
detokenize(link[,'dst'])
for(m in c('throughput')) {
  link[,m] <- latex.format(link[,m])
}
headings <- c('iface', 'Mbps', 'samples')
rownames(link) <- NULL
latex(link[,c('dst', 'throughput', 'throughput.samples')],
      booktabs=TRUE, size="small", table.env=FALSE, , colheads=headings)

## latex(g.sum, booktabs=TRUE, size="small", col.just=rep('c', ncol(g.sum)),
##       cdec=rep(1,length(g.sum)),
##       rowlabel="zone", here=T, file="", halign=paste(rep('c',ncol(g.sum)),collapse=''))

nrow(mmdata.df[['wifi']])
head(mmdata.df[['wifi']])
names(mmdata.list[['wifi']][[1]])
head(mmdata.list[['wifi']][[1]][[1]])
nrow(mmdata.list[['wifi']][[1]][[1]])

head(mmdata.list[['eth']][[1]][[1]])

## date2POSIXct(mmdata.df[['wifi']]$date[1:10])
## date2epoch(mmdata.df[['wifi']]$date[1:10])

## csv.ede files
for(n in names(mmdata.list$wifi)) {
  message("node ", n)
  res <- build.wifi.csv.ede(date, mmdata.list$wifi, n)
}
## res <- build.wifi.csv.ede(date, mmdata.list$wifi, '9')

for(n in names(mmdata.list$eth)) {
  message("node ", n)
  res <- build.eth.csv.ede(date, mmdata.list$eth, n)
}
for(n in names(mmdata.list$ifaces)) {
  message("node ", n)
  res <- build.ifaces.csv.ede(date, mmdata.list$ifaces, n)
}
for(n in names(mmdata.list$state)) {
  message("node ", n)
  res <- build.state.csv.ede(date, mmdata.list$state, n)
}
## res <- build.wifi.csv.ede(date, mmdata.list$wifi, '66')
## res <- build.eth.csv.ede(date, mmdata.list$eth, '66')
## res <- build.ifaces.csv.ede(date, mmdata.list$ifaces, '66')
## res <- build.state.csv.ede(date, mmdata.list$state, '66')

## report
for(n in links.ifaces$src[1:20]) {
  message("node ", n)
  build.report(date, mmdata.list, n)
}
## build.report(date, mmdata.list, '66')
## res <- build.report(date, mmdata.list, '9')

## report.tables
for(n in links.ifaces$src[1:20]) {
  message("node ", n)
  build.report.tables(mmdata.list, n)
}
## build.report.tables(mmdata.list, '66')
## build.report.tables(mmdata.list, '24')

from <- '66' ; to <- '1'
plot.par.ts.stat(date, mmdata.list[['wifi']], par="Mbps", from, to) #, save=TRUE)

mmdata.rtx <- get.rtx.rate.by.node.list(mmdata.list[['wifi']])
plot.par.ts.stat(date, mmdata.rtx, from, to, par="rtx.rate") #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['wifi']], from, to, par="throughput") #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['wifi']], from, to, par="dBm", bins=20) #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['wifi']], from, to, par="txRate") #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['wifi']], from, to, par="routes") #, save=TRUE)

## eth
from.eth <- '7' ; to.eth <- '66'
plot.par.ts.stat(date, mmdata.list[['eth']], from.eth, to.eth, par="txRate") #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['eth']], from.eth, to.eth, par="rxRate") #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['eth']], from.eth, to.eth, par="routes") #, save=TRUE)

## ifaces
from.ifaces <- links.ifaces$src[1] ; to.ifaces <- as.character(links.ifaces$dst[1])

plot.par.ts.stat(date, mmdata.list[['ifaces']], from.ifaces, to.ifaces, par="throughput") #, save=TRUE)

## state
head(mmdata.df[['state']])
from.state <- 24

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="loadavg.m1") #, save=TRUE)

## http://www.linuxhowtos.org/System/procstat.htm
## Time units are in USER_HZ or Jiffies (typically hundredths of a second).
## user: normal processes executing in user mode
## nice: niced processes executing in user mode
## system: processes executing in kernel mode
## idle: twiddling thumbs
## iowait: waiting for I/O to complete
## irq: servicing interrupts
## softirq: servicing softirqs
## "intr" line gives counts of interrupts serviced since boot time,
## "ctxt" line gives the total number of context switches across all CPUs.
## "btime" line gives the time at which the system booted, in seconds since
## processes" line gives the number of processes and threads created, which includes (but is not limited to) those created by calls to the fork() and clone() system calls.

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="system", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="idle", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="user", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="nice", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="btime", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="irq", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="ctxt", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="softirq", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="iowait", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="intr", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="processes") #, save=TRUE)

## https://www.thegeekdiary.com/understanding-proc-meminfo-file-analyzing-memory-utilization-in-linux/
## taken from munin:
## apps        Memory used by user-space applications.
## page_tables Memory used to map between virtual and physical memory addresses.
## swap_cache  A piece of memory that keeps track of pages that have been fetched from swap but not yet been modified.
## slab_cache  Memory used by the kernel (major users are caches like inode, dentry, etc).
## vmalloc_used 'VMalloc' (kernel) memory used
## committed   The amount of memory allocated to programs. Overcommitting is normal, but may indicate memory leaks.
## mapped      All mmap()ed pages.
## active      Memory recently used. Not reclaimed unless absolutely necessary.
## inactive    Memory not currently used.
## shmem       Shared Memory (SYSV SHM segments, tmpfs).
## cached       Parked file data (file content) cache.
## buffers     Block device (e.g. harddisk) cache. Also where "dirty" blocks are stored until written.
## swap        Swap space used.
## free        Wasted memory. Memory that is not used for anything at all.

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="nr_slab_unreclaimable", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="workingset_refault", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="nr_anon_pages", type='rate') #, save=TRUE)

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="active_file")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="cached")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="apps")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="mapped")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="active_anon")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="free")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="swap_cache")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="page_tables")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="inactive")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="shmem")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="committed")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="active")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="vmalloc_used")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="slab_cache")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="buffers")

plot.par.ts.stat(date, mmdata.list[['state']], from.state, par="swap")

## build.figures.report(date, mmdata.list[['wifi']], from, to)

##
## testing
##
## throughput
mmdata.norm <- get.throughput.by.node.list(mmdata.list[['wifi']])

mmdata.norm.ifaces <- get.throughput.by.node.list(mmdata.list[['ifaces']])

means.df <- get.node.means(mmdata.norm, 'throughput')
max.mean.links <- get.max.mean.links(means.df)
max.mean.links <- cbind(max.mean.links, id=1:nrow(max.mean.links))
max.mean.links

head(max.mean.links)

## df.links <- get.max.mean.links.ts(mmdata.norm, max.mean.links, 25)
df.links <- get.links(mmdata.norm, links)

df.links.ifaces <- get.links(mmdata.norm.ifaces, links.ifaces)

head(df.links)
head(as.POSIXct(df.links$date))

plot.grid.ts.throughput(date, df.links) #, save=TRUE)

plot.grid.stats.throughput(date, df.links) #, save=TRUE)

plot.grid.stats.log10.throughput(date, df.links) #, save=TRUE)

## Mbps
mmdata.links <- get.links(mmdata.list[['wifi']], links)

plot.grid.ts.Mbps(date, mmdata.links) #, save=TRUE)

plot.grid.stats.Mbps(date, rtx.df.links) #, save=TRUE)

## rtx
mmdata.rtx <- get.rtx.rate.by.node.list(mmdata.list[['wifi']])

rtx.means.df <- get.node.means(mmdata.rtx, 'rtx.rate')
rtx.max.mean.links <- get.max.mean.links(rtx.means.df)
rtx.max.mean.links <- cbind(rtx.max.mean.links, id=1:nrow(rtx.max.mean.links))
rtx.max.mean.links

rtx.df.links <- get.links(mmdata.rtx, links)

plot.grid.ts.rtx.rate(date, rtx.df.links) #, save=TRUE)

plot.grid.stats.rtx.rate(date, rtx.df.links) #, save=TRUE)

## dBm
plot.grid.ts.par(date, df.links, par="dBm") #, save=TRUE)

## txRate
plot.grid.ts.par(date, df.links, par="txRate") #, save=TRUE)

plot.grid.stats.par(date, df.links, par="txRate") #, save=TRUE)

##
##
##
nrow(max.mean.links)

ggplot(max.mean.links, aes(x=id, y=mean)) +
  geom_step()



ggplot(mmdata.norm[[2]][[1]], aes(x=t, y=log10(traffic))) +
  geom_step()



nr <- nrow(mmdata.fill)
mmdata.fill$traffic <- (mmdata.fill$traffic[2:nr]-mmdata.fill$traffic[1:(nr-1)])/
  (mmdata.fill$t[2:nr]-mmdata.fill$t[1:(nr-1)])



traffic.df <- data.frame(t=max.traffic$V1[2:nr]-max.traffic$V1[2],
                         traffic=8*(max.traffic$V4[2:nr]-max.traffic$V4[1:(nr-1)])/
                           (max.traffic$V1[2:nr]-max.traffic$V1[1:(nr-1)])/1e6)
traffic.df <- traffic.df[traffic.df$traffic>0,]


dst.max <- data.frame(t(sapply(unique(mmdata.fill$V2), function(src)
  unlist(mmdata.fill[which.max(mmdata.fill$V4[mmdata.fill$V2==src]),]))))
names(dst.max) <- c('t', 'src', 'dst', 'traffic')
head(dst.max)



dst.max <- cbind(dst.max,
                 mean=
                   sapply(1:nrow(dst.max), function(d) {
                     src <- dst.max$src[d]
                     dst <- dst.max$dst[d]
                     message("src: ", src, ", dst: ", dst)
                     mean(mmdata.fill$V4[(mmdata.fill$V2==src) & (mmdata.fill$V3==dst)])
                   }))

dst.max <- dst.max[order(dst.max$mean, decreasing=T),]
dst.max <- cbind(dst.max, id=1:nrow(dst.max))
head(dst.max)

ggplot(dst.max, aes(x=id, y=log10(mean))) +
  geom_line()



max.traffic.idx <- which.max(mmdata.fill$V4)
max.traffic.src <- mmdata.fill$V2[max.traffic.idx]
max.traffic.dst <- mmdata.fill$V3[max.traffic.idx]


max.traffic <- mmdata.fill[(mmdata.fill$V2==max.traffic.src) & (mmdata.fill$V3==max.traffic.dst),]

head(max.traffic)
nrow(max.traffic)

nr <- nrow(max.traffic)
traffic.df <- data.frame(t=max.traffic$V1[2:nr]-max.traffic$V1[2],
                         traffic=8*(max.traffic$V4[2:nr]-max.traffic$V4[1:(nr-1)])/
                           (max.traffic$V1[2:nr]-max.traffic$V1[1:(nr-1)])/1e6)
traffic.df <- traffic.df[traffic.df$traffic>0,]

head(traffic.df, 100)
tail(traffic.df, 100)
nrow(traffic.df)

max(max.traffic$V1)

ggplot(traffic.df, aes(x=t, y=traffic)) +
  geom_step()

ggplot(traffic.df, aes(x=log10(traffic))) +
  geom_histogram(binwidth=0.1)
## geom_histogram(binwidth=1e3, colour="black", fill="white")

ggplot(traffic.df, aes(x=traffic)) +
  geom_histogram(binwidth=0.1)
## geom_histogram(binwidth=1e3, colour="black", fill="white")

which.max(traffic.df$dt)

traffic.df$dt[(7359-50):(7359+50)]

traffic.df$traffic[(7359-50):(7359+50)]

## Local Variables:
## mode: R
## ess-default-style: RStudio
## ess-indent-offset: 2
## End:
