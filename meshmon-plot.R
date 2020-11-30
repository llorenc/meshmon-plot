## Format csv data gathered from qMp nodes in GuifiSants
## http://dsg.ac.upc.edu/qmpsu/index.php
## meshmon-plot.R
## (c) Llorenç Cerdà-Alabern, Nov 2020.

##
## meshmon plotting
##
library(plyr) ## rename, count
library(gridExtra)
library(gtable)
library(grid) # grid.xspline, unit

## Add axis tick-marks on top and to the right to a ggplot
## https://stackoverflow.com/questions/28949001/add-axis-tick-marks-on-top-and-to-the-right-to-a-ggplot
tick.marks.transform <- function(gplot) {
    grid.newpage()
    ## Convert the plot to a grob
    gt <- ggplotGrob(gplot)
    ## Get the position of the panel in the layout
    panel <-c(subset(gt$layout, name=="panel", se=t:r))
    ## For the bottom axis
    ## Get the row number of the bottom axis in the layout
    rn <- which(gt$layout$name == "axis-b")
    ## Extract the axis (tick marks only)
    axis.grob <- gt$grobs[[rn]]
    axisb <- axis.grob$children[[2]]  # Two children - get the second
    axisb  # Note: two grobs - tick marks and text
    ## Get the tick marks
    xaxis = axisb$grobs[[1]]  # NOTE: tick marks first
    if(!is.null(xaxis$y)) {
        xaxis$y = xaxis$y - unit(0.25, "cm")  # Position them inside the panel
    }
    ## Add a new row to gt, and insert the revised xaxis grob into the new row.
    ## gt <- gtable_add_rows(gt, unit(0, "lines"), panel$t-1)
    ## gt <- gtable_add_grob(gt, xaxis, l = panel$l, t = panel$t, r = panel$r, name = "ticks")
    ## Repeat for the left axis
    ## Get the row number of the left axis in the layout
    panel <-c(subset(gt$layout, name=="panel", se=t:r))
    rn <- which(gt$layout$name == "axis-l")
    ## Extract the axis (tick marks and axis text)
    axis.grob <- gt$grobs[[rn]]
    axisl <- axis.grob$children[[2]]  # Two children - get the second
    axisl  # Note: two grobs -  text and tick marks
    ## Get the tick marks
    yaxis = axisl$grobs[[2]] # NOTE: tick marks second
    #yaxis$x = yaxis$x - unit(0.25, "cm") # Position them inside the panel
    ## Add a new column to gt, and insert the revised yaxis grob into the new column.
    gt <- gtable_add_cols(gt, unit(0, "lines"), panel$r)
    gt <- gtable_add_grob(gt, yaxis, t = panel$t, l = panel$r+1, name = "ticks")
    ## Turn clipping off
    gt$layout[gt$layout$name == "ticks", ]$clip = "off"
    ## draw it
    grid.draw(gt)
    ## return the grid object
    gt
}

detokenize <- function(p) {
  p <- gsub('\\.', '_', p)
  p <- gsub('-', '_', p)
  p
}

get.file.name.table <- function(date, pref, node, ext='.tex') {
  fname <- paste0('tables/', paste(date, pref, node, sep='-'), ext)
  fname
}

get.file.name <- function(date, pref, type, plot, src, dst, par, ext='.pdf') {
  par <- detokenize(par)
  if(is.null(dst)) {
    fname <- paste0('figures/', paste(date, pref, src, type, plot, par, sep='-'), ext)
  } else {
    dst <- detokenize(dst)
    fname <- paste0('figures/', paste(date, pref, src, dst, type, plot, par, sep='-'), ext)
  }
  fname
}

get.links.mean.throughput <- function(l) {
  res.means <- unlist(sapply(l, function(dst) {
    mean(compute.throughput(dst)$throughput)
  }))
  res.samples <- unlist(sapply(l, function(dst) {
    nrow(dst)
  }))
  if(any(complete.cases(res.means))) {
    res.means <- res.means[complete.cases(res.means)]
    res.samples <- res.samples[names(res.means)]
    df <- data.frame(stringsAsFactors=FALSE,
                     dst=names(res.means),
                     mean=res.means,
                     samples=res.samples)
    df  <- df[order(df$mean, decreasing=TRUE),]
    return(df)
  }
  return(NULL)
}

get.links.mean <- function(l, par) {
  get.means <- function(l, par) {
    if(par == 'throughput') {
      unlist(sapply(l, function(dst) {
        mean(compute.throughput(dst)$throughput)
      }))
    } else {
      unlist(sapply(l, function(dst) {
        mean(dst[,par])
      }))
    }
  }
  get.samples <- function(l, par) {
    unlist(sapply(l, function(dst) {
      nrow(dst)
    }))
  }
  df <- NULL
  sortby <- NULL
  for(p in par) {
    res.means <- get.means(l, p)
    if(any(complete.cases(res.means))) {
      res.samples <- get.samples(l, p)
      res.samples <- res.samples[names(res.means)]
      if(is.null(df)) {
        df <- data.frame(stringsAsFactors=FALSE,
                         dst=names(res.means),
                         mean=res.means,
                         samples=res.samples)
        sortby <- p
        df <- rename(df, c(mean=sortby, samples=paste0(p, '.samples')))
      } else {
        df <- df[df$dst == names(res.means),]
        df <- cbind(df,
                    data.frame(stringsAsFactors=FALSE,
                               mean=res.means,
                               samples=res.samples))
        df <- rename(df, c(mean=p, samples=paste0(p, '.samples')))
      }
    }
  }
  if(!is.null(sortby)) {
    df  <- df[order(df[,sortby], decreasing=TRUE),]
  }
  return(df)
}

build.wifi.figures <- function(date, l, from, to) {
  plot.par.ts.stat(date, l, par="Mbps", from, to, pref='wifi', save=TRUE)
  mmdata.rtx <- get.rtx.rate.by.node.list(l)
  plot.par.ts.stat(date, mmdata.rtx, from, to, par="rtx.rate", pref='wifi', save=TRUE)
  plot.par.ts.stat(date, l, from, to, par="throughput", pref='wifi', save=TRUE)
  plot.par.ts.stat(date, l, from, to, par="dBm", bins=20, pref='wifi', save=TRUE)
  plot.par.ts.stat(date, l, from, to, par="routes", pref='wifi', save=TRUE)
  plot.par.ts.stat(date, l, from, to, par="txRate", pref='wifi', save=TRUE)
  plot.par.ts.stat(date, l, from, to, par="rxRate", pref='wifi', save=TRUE)
}

build.eth.figures <- function(date, l, from, to) {
  plot.par.ts.stat(date, l, from, to, par="routes", pref='eth', save=TRUE)
  plot.par.ts.stat(date, l, from, to, par="txRate", pref='eth', save=TRUE)
  plot.par.ts.stat(date, l, from, to, par="rxRate", pref='eth', save=TRUE)
}

build.ifaces.figures <- function(date, l, from, to) {
  plot.par.ts.stat(date, mmdata.list[['ifaces']], from, to, par="throughput", pref='ifaces', save=TRUE)
}

build.state.figures <- function(date, l, n) {
  plot.par.ts.stat(date, l, n, pref='state', par="system", type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="idle", type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="user", type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="nice", type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="btime", type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="irq", type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="ctxt", type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="softirq", type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="iowait", type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="intr", type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="processes", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="nr_slab_unreclaimable", save=TRUE) # type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="workingset_refault", save=TRUE) # , type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="nr_anon_pages", save=TRUE) # , type='rate', save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="active_file", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="cached", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="apps", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="mapped", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="active_anon", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="free", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="swap_cache", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="page_tables", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="inactive", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="shmem", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="committed", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="active", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="vmalloc_used", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="slab_cache", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="buffers", save=TRUE)
  plot.par.ts.stat(date, l, n, pref='state', par="swap", save=TRUE)
}

build.report <- function(date, mmdata.list, node) {
  nn <- uid2label(node)
  ## wifi
  if(length(mmdata.list[['wifi']][[node]]) > 0) {
    m <- get.links.mean.throughput(mmdata.list[['wifi']][[node]])
    if(!is.null(m)) {
      build.wifi.figures(date, mmdata.list[['wifi']], from=node, to=m$dst[1])
    }
  }
  ## eth
  if(length(mmdata.list[['eth']][[node]]) > 0) {
    m <- get.links.mean(mmdata.list[['eth']][[node]], 'routes')
    if(!is.null(m)) {
      build.eth.figures(date, mmdata.list[['eth']], from=node, to=m$dst[1])
    }
  }
  ## ifaces
  if(length(mmdata.list[['ifaces']][[node]]) > 0) {
    m <- get.links.mean.throughput(mmdata.list[['ifaces']][[node]])
    if(!is.null(m)) {
      for(d in 1:min(4, length(m$dst)))
        build.ifaces.figures(date, mmdata.list[['ifaces']], from=node, to=m$dst[d])
    }
  }
  ## state
  build.state.figures(date, mmdata.list[['state']], node)
}

build.wifi.table <- function(l, f) {
  link <- get.links.mean(l, c('routes', 'txRate', 'rxRate', 'throughput'))
  for(m in c('routes', 'txRate', 'rxRate', 'throughput')) {
    link[,m] <- latex.format(link[,m])
  }
  link <- cbind(name=sapply(link$dst, uid2label), link)
  headings <- c('name', 'dst.uid', 'Mbps', 'routes', 'txRate', 'rxRate', 'samples')
  rownames(link) <- NULL
  latex(link[,c('name', 'dst', 'throughput', 'routes', 'txRate', 'rxRate', 'throughput.samples')],
        file=f, booktabs=TRUE, size="small", table.env=FALSE, , colheads=headings)
}

build.eth.table <- function(l, f) {
  link <- get.links.mean(l, c('routes', 'txRate', 'rxRate'))
  for(m in c('routes', 'txRate', 'rxRate')) {
    link[,m] <- latex.format(link[,m])
  }
  link <- cbind(name=sapply(link$dst, uid2label), link)
  headings <- c('name', 'dst.uid', 'routes', 'txRate', 'rxRate', 'samples')
  rownames(link) <- NULL
  latex(link[,c('name', 'dst', 'routes', 'txRate', 'rxRate', 'routes.samples')],
        file=f, booktabs=TRUE, size="small", table.env=FALSE, , colheads=headings)
}

build.ifaces.table <- function(l, f) {
  iface <- get.links.mean(l, c('throughput'))
  iface[,'dst'] <- gsub('_', '\\\\_', iface[,'dst'])
  for(m in c('throughput')) {
    iface[,m] <- latex.format(iface[,m])
  }
  headings <- c('name', 'Mbps', 'samples')
  rownames(iface) <- NULL
  latex(iface[,c('dst', 'throughput', 'throughput.samples')],
        file=f, booktabs=TRUE, size="small", table.env=FALSE, , colheads=headings)
}

build.report.tables <- function(mmdata.list, node) {
  nn <- uid2label(node)
  ## wifi
  if(!is.null(mmdata.list[['wifi']][[node]])) {
    fn <- get.file.name.table(date, pref='wifi', node)
    build.wifi.table(mmdata.list[['wifi']][[node]], fn)
  }
  ## eth
  if(!is.null(mmdata.list[['eth']][[node]])) {
    fn <- get.file.name.table(date, pref='eth', node)
    build.eth.table(mmdata.list[['eth']][[node]], fn)
  }
  ## ifaces
  if(!is.null(mmdata.list[['ifaces']][[node]])) {
    fn <- get.file.name.table(date, pref='ifaces', node)
    build.ifaces.table(mmdata.list[['ifaces']][[node]], fn)
  }
  ## state
  ## build.state.table(mmdata.list[['state']], node)
}

##
## build all pdfs
##
build.figures.report <- function(date, mmdata.list, from, to) {
  plot.ts.Mbps(date, mmdata.list, from, to, save=TRUE)
  plot.stats.Mbps(date, mmdata.list, from, to, save=TRUE)
  ##
  mmdata.rtx <- get.rtx.rate.by.node.list(mmdata.list)
  plot.ts.rtx.rate(date, mmdata.rtx, from, to, save=TRUE)
  plot.stats.rtx.rate(date, mmdata.rtx, from, to, save=TRUE)
  ##
  mmdata.norm <- get.throughput.by.node.list(mmdata.list)
  plot.ts.throughput(date, mmdata.norm, from, to, save=TRUE)
  plot.stats.throughput(date, mmdata.norm, from, to, save=TRUE)
  ##
  plot.ts.par(date, mmdata.list, from, to, par="dBm", save=TRUE)
  plot.stats.par(date, mmdata.list, from, to, par="dBm", bins=20, save=TRUE)
  ##
  plot.ts.par(date, mmdata.list, from, to, par="txRate", save=TRUE)
  plot.stats.par(date, mmdata.list, from, to, par="txRate", bins=20, save=TRUE)
  ##
  plot.ts.par(date, mmdata.list, from, to, par="rxRate", save=TRUE)
  plot.stats.par(date, mmdata.list, from, to, par="rxRate", bins=20, save=TRUE)
}

##
## plot par
##
get.title <- function(src.uid, dst.uid=NULL) {
  title <- 
    if(is.null(dst.uid)) {
      uid2label(src.uid)
    } else {
      paste(uid2label(src.uid), uid2label(dst.uid), sep='~')
    }
  title
}

get.df <- function(mmdata.list, par, src.uid, dst.uid=NULL) {
  df <- if(is.null(dst.uid)) {
          mmdata.list[[src.uid]]
        } else {
          mmdata.list[[src.uid]][[dst.uid]]
        }
  names(df)[names(df) == par] <- "val"
  df
}

plot.par <- function(date, mmdata.list, par, src.uid, dst.uid=NULL, bins=NULL,
                     type='value', # value/rate/throughput
                     plot='ts',    # ts/stat
                     title=TRUE,
                     print=TRUE,
                     save=FALSE) {
  build.aes <- function(type, plot) {
    aes <- switch(
      plot,
      ts="aes(x=as.POSIXct(date), y=",
      stat="aes(x=")
    aes <-
      paste0(aes, switch(type, value='val', rate='rate', throughput='throughput'), ')')
    eval(parse(text=aes))
  }
  if(par == 'throughput') {
    type='throughput'
  }
  plot.df <- switch(
    type,
    value=get.df(mmdata.list, par, src.uid, dst.uid),
    rate=compute.rate(get.df(mmdata.list, par, src.uid, dst.uid), 'val'),
    throughput=compute.throughput(mmdata.list[[src.uid]][[dst.uid]]),
    stop("plot.par: type?"))
  gs <- ggplot(plot.df, build.aes(type, plot))
  if(title) {
    gtit <- get.title(src.uid, dst.uid)
    gs <- gs + ggtitle(paste(gtit, date, sep=', '))
  }
  if(plot == 'ts') {
    gs <- gs +
      geom_step() +
      scale_x_datetime(date_breaks=paste(7, 'day'), date_labels = "%d/%m") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab("day") + ylab(par)
  } else {
    gs <- gs +
      geom_histogram(aes(y=..ncount..), color="black", fill="white", bins=bins) +
      geom_step(aes(y = 1 - ..y..), stat='myecdf', size=1, color='red') +
      ylab("scaled histogram/Complementary ECDF") + xlab(par)
  }
  if(print) print(gs)
  gs
}

plot.par.ts.stat <- function(date, mmdata.list, par, src.uid, dst.uid=NULL, bins=NULL,
                             type='value', # value/rate/throughput
                             pref='n', save=FALSE) {
  p1 <- plot.par(date, mmdata.list, par, src.uid, dst.uid, type=type, print=FALSE, save=FALSE)
  p2 <- plot.par(date, mmdata.list, par, src.uid, dst.uid, type=type, plot='stat', bins=bins, title=FALSE, print=FALSE, save=FALSE)
  p1.t <- tick.marks.transform(p1)
  p2.t <- tick.marks.transform(p2)
  gl <- list(p1.t, p2.t)
  ga <- do.call(rbind, c(gl, size="first"))
  ga$widths = do.call(unit.pmax, lapply(gl, "[[", "widths"))
  grid.newpage()
  ## ga <- ggplot_gtable(ggplot_build(p))
  ga$layout$clip[ga$layout$name == "panel"] <- "off" # override clipping
  grid.draw(ga)
  if(save) {
    fname <- get.file.name(date, pref, type, plot='ts-stat', src.uid, dst.uid, par)
    message('building ', fname)
    ggsave(fname, ga)
  }
}

##
## grid
plot.grid.ts.par  <- function(date, df.links, par, save=FALSE) {
  plot.df <- data.frame(date=df.links$date,
                        node=df.links$node,
                        val=df.links[,par])
  ## time series
  gs <-
    ggplot(plot.df, aes(x=as.POSIXct(date), y=val)) +
    ggtitle(paste0("Busiest WiFi links time series, ", date)) +
    facet_wrap(~node, scales="free_y", nrow=5) +
    geom_step() +
    scale_x_datetime(date_breaks=paste(7, 'day'), date_labels = "%d/%m") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("day") + ylab(par)
  print(gs)
}

plot.grid.stats.par <- function(date, df.links, par, bins=NULL, save=FALSE) {
  plot.df <- data.frame(date=df.links$date,
                        node=df.links$node,
                        val=df.links[,par])
  gs <-
    ggplot(plot.df, aes(x=val)) +
    ggtitle(paste0("Busiest WiFi links statistics, ", date)) +
    facet_wrap(~node, scales="free_y", nrow=5) +
    geom_histogram(aes(y=..ncount..), color="black", fill="white") +
    geom_step(aes(y = 1 - ..y..), stat='myecdf', size=1, color='red') +
    ## scale_x_continuous(breaks=seq(-10,10,by=1), lim=c(1,3)) +
    ylab("scaled histogram/Complementary ECDF") + xlab(par)
  print(gs)
}

## Histogram/CECDF log10 scale
plot.grid.stats.log10.throughput <- function(date, df.links, save=FALSE) {
  gs <-
    ggplot(df.links, aes(x=log10(throughput))) +
    ggtitle(paste0("Busiest WiFi links statistics, ", date)) +
    facet_wrap(~node, scales="free_y", nrow=5) +
    geom_histogram(aes(y=log10(..ncount..)), color="black", fill="white", bins=50) +
    geom_step(aes(y = log10(1 - ..y..)), stat='myecdf', size=1, color='red') +
    scale_y_continuous(breaks=seq(-10,10,by=1), lim=c(-4,0)) +
    scale_x_continuous(breaks=seq(-10,10,by=1), lim=c(-4,2)) +
    ylab("scaled histogram/Complementary ECDF") + xlab("throughput [Mbps], log10 scale")
  print(gs)
}

##
## testing
##
if(FALSE) {
  ## Histogram
  ggplot(df.links, aes(x=log10(throughput))) +
    facet_wrap(~node, scales="free_y", nrow=5) +
    geom_histogram(color="black", fill="white")
  ## geom_histogram(binwidth=1e3, colour="black", fill="white")
  
  ## CECDF
  ggplot(df.links, aes(x=log10(throughput))) +
    facet_wrap(~node, scales="free_y", nrow=5) +
    geom_step(aes(y = 1 - ..y..), stat='myecdf', size=1, color='red') +
    scale_x_continuous(breaks=seq(-10,10,by=1), lim=c(-3,1))
}

## Local Variables:
## mode: R
## ess-default-style: RStudio
## ess-indent-offset: 2
## End:
