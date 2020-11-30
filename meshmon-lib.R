## Format csv data gathered from qMp nodes in GuifiSants
## http://dsg.ac.upc.edu/qmpsu/index.php
## meshmon-lib.R
## (c) Llorenç Cerdà-Alabern, Nov 2020.

library(ggplot2)
library(plyr) # melt, rename, count
library(tidyr) # fill
library(rjson)
library(lubridate) # Get/set component of a date-time

get.file.name.csv.ede <- function(date, pref, node, ext='.csv.gz') {
  fname <- paste0('csv-ede/', paste(date, pref, node, sep='-'), ext)
  fname
}

build.wifi.csv.ede <- function(date, l, from, save=TRUE) {
  l <- get.rtx.rate.by.node.list(l)
  res <- data.frame()
  for(to in names(l[[from]])) {
    df <- field.to.rate(l[[from]][[to]], c('ltxb', 'lrxb', 'ltxp', 'lrxp'))
    res <- rbind.data.frame(res, df)
  }
  if(save) {
    fname <- get.file.name.csv.ede(date, "wifi", from)
    if(nrow(res) > 0) {
      write.csv(res, row.names=FALSE, file=gzfile(fname))
    } else {
      message("No wifi links, skipping file ", fname)
    }
  }
  res
}

build.eth.csv.ede <- function(date, l, from, save=TRUE) {
  res <- data.frame()
  for(to in names(l[[from]])) {
    res <- rbind.data.frame(res, l[[from]][[to]])
  }
  if(save) {
    fname <- get.file.name.csv.ede(date, "eth", from)
    if(nrow(res) > 0) {
      write.csv(res, row.names=FALSE, file=gzfile(fname))
    } else {
      message("No wifi links, skipping file ", fname)
    }
  }
  res
}

build.ifaces.csv.ede <- function(date, l, from, save=TRUE) {
  res <- data.frame()
  for(to in names(l[[from]])) {
    df <- field.to.rate(l[[from]][[to]], c('txe', 'rxe', 'txb', 'rxb', 'txp', 'rxp'))
    res <- rbind.data.frame(res, df)
  }
  if(save) {
    fname <- get.file.name.csv.ede(date, "ifaces", from)
    if(nrow(res) > 0) {
      write.csv(res, row.names=FALSE, file=gzfile(fname))
    } else {
      message("No ifaces, skipping file ", fname)
    }
  }
  res
}

build.state.csv.ede <- function(date, l, from, save=TRUE) {
  res <- field.to.rate(l[[from]], c("system", "idle", "user", "nice", "btime",
                                    "irq", "ctxt", "softirq", "iowait", "intr"
                                    ## ,"processes", "nr_slab_unreclaimable", "workingset_refault",
                                    ## "nr_anon_pages"
                                    ))
  if(save) {
    fname <- get.file.name.csv.ede(date, "state", from)
    if(nrow(res) > 0) {
      write.csv(res, row.names=FALSE, file=gzfile(fname))
    } else {
      message("No state, skipping file ", fname)
    }
  }
  res
}

to.math <- function(s) {
  paste0('$',s,'$')
}
f2 <- function(x) {
  if(is.null(x) || is.na(x)) 'NA'
  else to.math(format(x, big.mark = ",", decimal.mark = ".", digits=2))
}
f2c <- function(x) {
  if(is.null(x) || is.na(x)) 'NA'
  else to.math(format(x, big.mark = ",", decimal.mark = ".",
                      digits=2, scientific=T, trim=T, justify="none"))
}
f0 <- function(x) {
  to.math(format(x, big.mark = ",", digits=0))
}
format.t <- function(t) {
  f2 <- function(x) {
    format(x, big.mark = ",", decimal.mark = ".", digits=2)
  }
  if(is.null(t) || is.na(t)) 'NA'
  else {
    if(t > 24*3600) {
      paste0('$', f2(t/(24*3600)), "$d")
    } else if(t > 3600) {
      paste0('$', f2(t/3600), "$h")
    } else if(t > 60) {
      paste0('$', f2(t/60), "$m")
    } else {
      paste0('$', f2(t), "$s")
    }
  }
}
latex.format <- function(x) {
  f2c <- function(x) {
    if((x<0.01) || (x>1000))
      format(x, big.mark = ",", decimal.mark = ".",
             digits=2, scientific=T, trim=T, justify="none")
    else
      format(x, big.mark = ",", decimal.mark = ".", digits=2)
  }
  if(length(x) > 1) return(sapply(x, latex.format))
  if(is.null(x) || is.na(x)) return('NA')
  if(abs(x) < 1e-10) return(to.math('0.0'))
  x.s <- f2c(x)
  x.s <- gsub("(.*)e([-+])(.*)", "\\1\\\\,{\\\\scriptscriptstyle10}^{\\2\\3}", x.s)
  x.s <- gsub("\\+0*", "", x.s)
  x.s <- gsub("\\-0*", "-", x.s)
  to.math(x.s)
}

choose.links <- function(date, mmdata.list, count, ignore='UPC') {
  mmdata.norm <- get.throughput.by.node.list(mmdata.list)
  means.df <- get.node.means(mmdata.norm, 'throughput')
  max.mean.links <- get.max.mean.links(means.df)
  res <- data.frame()
  for(l in order(max.mean.links$mean, decreasing=TRUE)) {
    src.name <- uid2label(max.mean.links$src[l])
    if(!grepl(ignore, src.name)) {
      dst.name <- uid2label(max.mean.links$dst[l])
      res <- rbind(res,
                   cbind(src.name=src.name,dst.name=dst.name, max.mean.links[l,]))
      count <- count - 1
      if(count == 0) {
        break
      }
    }
  }
  res
}

get.routes.by.node.list <- function(l) {
  res <- list()
  for(src in names(l)) {
    for(dst in names(l[[src]])) {
      nr <- nrow(l[[src]][[dst]])
      if(nr > 10) {
        res[[src]][[dst]] <-
          cbind(l[[src]][[dst]],
                second=l[[src]][[dst]]$epoch-l[[src]][[dst]]$epoch[1],
                routes=l[[src]][[dst]]$routes)
      }
    }
  }
  res
}

choose.links.by.routes <- function(date, mmdata.list, count, ignore='UPC') {
  mmdata.norm <- get.routes.by.node.list(mmdata.list)
  means.df <- get.node.means(mmdata.norm, 'routes')
  max.mean.links <- get.max.mean.links(means.df)
  res <- data.frame()
  for(l in order(max.mean.links$mean, decreasing=TRUE)) {
    src.name <- uid2label(max.mean.links$src[l])
    if(!grepl(ignore, src.name)) {
      dst.name <- uid2label(max.mean.links$dst[l])
      res <- rbind(res,
                   cbind(src.name=src.name,dst.name=dst.name, max.mean.links[l,]))
      count <- count - 1
      if(count == 0) {
        break
      }
    }
  }
  res
}

build.mmdata.df <- function(csvf) {
  mmdata <- read.csv(csvf, header=TRUE, sep=';', na.strings = "", stringsAsFactors=F)
  mmdata.fill <- fill(mmdata, date, .direction="down")
  ## names(mmdata.fill) <- c('date', 'src', 'dst', 'name.src', 'id.src', 'id.dst', 
  ##                         'name.dst', 'traffic')
  mmdata.fill
}

read.rds.file <- function(fname, callf) {
  if(file.access(fname) == 0) {
    message("reading file: ", fname)
    return(readRDS(fname))
  }
  message("building file: ", fname)
  res <- eval(callf)
  saveRDS(res, file=fname)
  res
}

date2POSIXct <- function(d) {
  gsub("(\\d\\d-\\d\\d-\\d\\d)_(\\d\\d)-(\\d\\d)-(\\d\\d)", "20\\1 \\2:\\3:\\4", d)
}

date2epoch <- function(d) {
  as.integer(as.POSIXct(d))
}

uid2label <- function(uid) {
  for(node in uid.db[[1]]) {
    if(node$uid == uid) {
      return(node$hname[1])
      ## return(paste(uid, node$hname[1], sep=': '))
    }
  }
  return(uid)
}

## split.link.by.node <- function(d) {
##   res <- list() ;
##   for(src in unique(d$src.uid)) {
##     for(dst in unique(d$dst.uid[d$src.uid==src])) {
##       res[[as.character(src)]][[as.character(dst)]] <-
##         d[(d$src.uid==src) & (d$dst.uid==dst),]
##       date <- date2POSIXct(res[[as.character(src)]][[as.character(dst)]]$date)
##       res[[as.character(src)]][[as.character(dst)]]$date <- date
##       res[[as.character(src)]][[as.character(dst)]] <-
##         cbind(res[[as.character(src)]][[as.character(dst)]],
##               epoch=date2epoch(date))
##     }
##   }
##   res
## }

## split.by.node <- function(d) {
##   res <- list() ;
##   for(src in unique(d$uid)) {
##     for(dst in unique(d$if.name[d$uid==src])) {
##       res[[as.character(src)]][[dst]] <- d[(d$uid==src) & (d$if.name==dst),]
##       date <- date2POSIXct(res[[as.character(src)]][[dst]]$date)
##       res[[as.character(src)]][[dst]]$date <- date
##       res[[as.character(src)]][[dst]] <-
##         cbind(res[[as.character(src)]][[dst]], epoch=date2epoch(date))
##     }
##   }
##   res
## }

get.field <- function(f, d) {
  for(t in f) {
    if(t %in% names(d)) {
      return(t)
    }
  }
  stop("split.link.by.node: f.src?")
}

split.link.by.node <- function(d) {
  res <- list() ;
  f.dst <- get.field(c('dst.uid', 'if.name'), d)
  f.src <- get.field(c('src.uid', 'uid'), d)
  if(is.null(f.src)) {
    stop("split.link.by.node: f.src?")
  }
  for(src in unique(d[,f.src])) {
    for(dst in unique(d[,f.dst][d[,f.src]==src])) {
      res[[as.character(src)]][[as.character(dst)]] <-
        d[(d[,f.src]==src) & (d[,f.dst]==dst),]
      date <- date2POSIXct(res[[as.character(src)]][[as.character(dst)]]$date)
      res[[as.character(src)]][[as.character(dst)]]$date <- date
      res[[as.character(src)]][[as.character(dst)]] <-
        cbind(res[[as.character(src)]][[as.character(dst)]],
              epoch=date2epoch(date))
    }
  }
  res
}

split.state.by.node <- function(d) {
  res <- list() ;
  f.src <- get.field(c('uid'), d)
  if(is.null(f.src)) {
    stop("split.state.by.node: f.src?")
  }
  for(src in unique(d[,f.src])) {
    res[[as.character(src)]] <- d[d[,f.src]==src,]
    date <- date2POSIXct(res[[as.character(src)]]$date)
    res[[as.character(src)]]$date <- date
    res[[as.character(src)]] <- cbind(res[[as.character(src)]],
                                      epoch=date2epoch(date))
  }
  res
}

field.to.rate <- function(data, field) {
  nr <- nrow(data)
  data[1:(nr-1),field] <-
    (data[2:nr,field]-data[1:(nr-1),field])/
    (data$epoch[2:nr]-data$epoch[1:(nr-1)])
  data <- data[1:(nr-1),]
  for(f in field) {
    data <- data[data[,f]>=0,]
    names(data)[names(data) == f] <- paste0(f, '.rate')
  }
  data
}
  
compute.rate <- function(data, field, name=NULL, scale=1) {
  nr <- nrow(data)
  res <- cbind.data.frame(data[1:(nr-1),],
                          second=data$epoch[1:(nr-1)]-data$epoch[1],
                          rate=scale*(data[2:nr,field]-data[1:(nr-1),field])/
                            (data$epoch[2:nr]-data$epoch[1:(nr-1)]),
                          deltat=data$epoch[2:nr]-data$epoch[1:(nr-1)])
  res <- res[res$rate>=0,]
  if(!is.null(name)) {
    res <- rename(res, c(rate=name))
  }
  res
}

##
get.throughput.bytes.field <- function(l) {
  for(t in c('ltxb', 'txb')) {
    if(t %in% names(l)) {
      return(t)
    }
  }
  stop("get.throughput.bytes.field: txb?")
}

## default scale: bytes to Mbps
compute.throughput <- function(data, field=NULL) {
  if(is.null(field)) {
    field <- get.throughput.bytes.field(data)
  }
  compute.rate(data, field, name='throughput', scale=8e-6)
}

## Compute throughput in Mbps from txb
get.throughput.by.node.list <- function(l) {
  res <- list()
  txb <- NULL
  for(src in names(l)) {
    for(dst in names(l[[src]])) {
      nr <- nrow(l[[src]][[dst]])
      if(nr > 10) {
        if(is.null(txb)) {
          tbx <- get.throughput.bytes.field(l[[src]][[dst]])
        }
        res[[src]][[dst]] <- compute.throughput(l[[src]][[dst]], txb)
      }
    }
  }
  res
}

## Compute rtx.rate from ltxp
get.rtx.rate.by.node.list <- function(l) {
  ## res <- list()
  for(src in names(l)) {
    for(dst in names(l[[src]])) {
      nr <- nrow(l[[src]][[dst]])
      if(nr > 2) {
        l[[src]][[dst]][1:(nr-1), 'rtx'] <-
          (l[[src]][[dst]]$rtx[2:nr]-l[[src]][[dst]]$rtx[1:(nr-1)])/
          (l[[src]][[dst]]$ltxp[2:nr]-l[[src]][[dst]]$ltxp[1:(nr-1)])
        l[[src]][[dst]]$rtx[is.nan(l[[src]][[dst]]$rtx)] <- 0
        l[[src]][[dst]] <- l[[src]][[dst]][l[[src]][[dst]]$rtx >= 0,]
      }
    }
  }
  for(src in names(l)) {
    for(dst in names(l[[src]])) {
      if('rtx' %in% names(l[[src]][[dst]]))
        l[[src]][[dst]] <- rename(l[[src]][[dst]], c(rtx='rate.rtx'))
    }
  }
  l
}

get.node.means <- function(nn, par) {
  df <- data.frame()
  sapply(names(nn), function(src) {
    res.means <- unlist(sapply(nn[[src]], function(dst) {
      mean(dst[,par])
    }))
    res.samples <- unlist(sapply(nn[[src]], function(dst) {
      length(dst[,par])
    }))
    if(any(complete.cases(res.means))) {
      res.means <- res.means[complete.cases(res.means)]
      res.samples <- res.samples[names(res.means)]
      df <<- rbind(df, data.frame(stringsAsFactors=FALSE, src=src, dst=names(res.means),
                                  mean=res.means, samples=res.samples))
    }
  })
  df
}

get.max.mean.links <- function(mm) {
  max.df <- data.frame()
  for(src in unique(mm$src)) {
    src.links <- mm[mm$src==src,]
    dst <- src.links[which.max(src.links$mean),]
    max.df <- rbind(max.df, data.frame(src=src, dst=dst$dst, mean=dst$mean, samples=dst$samples))
  }
  max.df[order(max.df$mean, decreasing=TRUE),]
}

get.max.mean.links.ts <- function(ts, mm, n) {
  res <- data.frame()
  for(l in order(mm$mean, decreasing=TRUE)[1:n]) {
    res <- rbind(res,
                 cbind(node=uid2label(mm$src[l]),
                       ts[[as.character(mm$src[l])]][[as.character(mm$dst[l])]]))
  }
  res
}

get.links <- function(ts, links) {
  res <- data.frame()
  for(l in 1:nrow(links)) {
    if(nrow(ts[[as.character(links$src[l])]][[as.character(links$dst[l])]]) > 0) {
      res <- rbind(res,
                   cbind(ts[[as.character(links$src[l])]][[as.character(links$dst[l])]],
                         node=paste(links$src.name[l], links$dst.name[l], sep='~')))
    } else {
      message("no data in link ", paste(links$src.name[l], links$dst.name[l], sep='~'))
    }
  }
  res
}

## https://github.com/tidyverse/ggplot2/issues/1467
## get rid of points at y=0 and y=1 in ecdf ggplot
stat_myecdf <- function(mapping = NULL, data = NULL, geom = "step",
                        position = "identity", n = NULL, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, direction="vh", ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatMyecdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      na.rm = na.rm,
      direction=direction,
      ...
    )
  )
}

StatMyecdf <-
  ggproto("StatMyecdf", Stat,
          compute_group = function(data, scales, n = NULL) {
            ## If n is NULL, use raw values; otherwise interpolate
            if (is.null(n)) {
              ## Dont understand why but this version needs to sort the values
              xvals <- sort(unique(data$x))
            } else {
              xvals <- seq(min(data$x), max(data$x), length.out = n)
            }
            y <- ecdf(data$x)(xvals)
            x1 <- max(xvals)
            y0 <- 0                      
            data.frame(x = c(xvals, x1), y = c(y0, y))
          },
          default_aes = aes(y = ..y..),
          required_aes = c("x")
          )


## Local Variables:
## mode: R
## ess-default-style: RStudio
## ess-indent-offset: 2
## End:
