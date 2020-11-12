#' \code{strip.SRDL} makes a stripchart of SRDL dive data
#'
#' @param dive Logical, should detailed dive data be plotted?
#' @param c.dat Logical, should max depth from CTD data be plotted?
#' @param s.dat Logical, should dive summary data be plotted?
#' @param QQ Logical, should dive quality check be performed. Note! Only works for 4-point dive data!
#' @return Returns a stripchart plot, either as standard R plot
#'   or an interactive dygraph (if \code{type} is set to 'dygraph')
#' @details The dygraphs option depends on the \code{\link{dygraph}} and \code{\link{xts}}
#'   functions, and the relevant packages (\code{dygraphs} & \code{xts}) must be installed for the dygraphs option to work.
#' @family SMRU SRDL functions
#' @seealso \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLdb}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @examples
#' strip.SRDLdb(dive, ctd, summ)
#' strip.SRDLdb(dive, ctd, summ, type='dygraph')
#' #' @importFrom xts xts
#' @importFrom dygraphs dygraph
#' @importFrom tidyr gather
#' require(dplyr)
#' @export

strip.SRDL <- function(dat=hp4, ctd=F, d.sum=T, theRef=levels(hp4$dive$REF)[1],
                       type='normal', tz='UTC', QQ=F) {

  d.dat <- dat$dive[which(dat$dive$REF==theRef),]
  if(ctd) c.dat <- dat$ctd[which(dat$ctd$REF==theRef),]
  if(d.sum) s.dat <- dat$summary[which(dat$summary$REF==theRef),]

  if(QQ) d.dat <- qcDive(d.dat, flag.only=F)
  if(tz!='local') {
    if(type=='normal') {
      plot(-MAX.DEP~DE.DATE, data=d.dat, type='n')
      if(d.sum) {
        segments(s.dat$S.DATE, -s.dat$MAX.DEPTH, s.dat$E.DATE, -s.dat$MAX.DEPTH, col=3)
      }
      if(ctd) {
        points(-MAX.DBAR~END.DATE, data=c.dat, pch='+', col=2)
      }
      points(-MAX.DEP~DE.DATE, data=d.dat, type='h')
    } else {
      require(tidyr)
      require(dygraphs)
      require(xts)
      inflect <- match(paste('D', c(1:25), sep=''), names(d.dat))
      inflect <- inflect[which(!is.na(inflect))]
      inflect <- inflect[unlist(lapply(inflect, function(x) !all(is.na(d.dat[,x]))))]

      d.pts <- inflect
      t.pts <- match(gsub('D', 'T', names(d.dat)[inflect]), names(d.dat))

      for(i in 1:length(t.pts)) {
        d.dat[,t.pts[i]] <- d.dat$DE.DATE-((0.01*(100-d.dat[,t.pts[i]]))*d.dat$DIVE.DUR)
      }

      d.dat$T0 <- d.dat$DE.DATE-d.dat$DIVE.DUR
      new.tname <- paste('T', length(t.pts)+1, sep='')
      if(!is.na(match(new.tname, names(d.dat)))) {
        d.dat[match(new.tname, names(d.dat))] <- d.dat$DE.DATE
      } else {
        names(d.dat)[match('DE.DATE', names(d.dat))] <- new.tname
      }
      d.dat$D0 <- rep(0, nrow(d.dat))
      d.dat$DE <- rep(0, nrow(d.dat))
      new.dname <- paste('D', length(t.pts)+1, sep='')
      if(!is.na(match(new.dname, names(d.dat)))) {
        d.dat[match(new.dname, names(d.dat))] <- d.dat$DE
      } else {
        names(d.dat)[length(d.dat)] <- paste('D', length(t.pts)+1, sep='')
      }

      t.pts <- c(match('T0', names(d.dat)), t.pts, match(paste('T', length(t.pts)+1, sep=''), names(d.dat)))
      d.pts <- c(match('D0', names(d.dat)), d.pts, match(paste('D', length(d.pts)+1, sep=''), names(d.dat)))

      options(warn=-1)
      t.t <- gather(d.dat[,t.pts], T.key, T.val, num_range('T', c(0:(length(t.pts)-1))))
      t.d <- gather(d.dat[,d.pts], D.key, D.val, num_range('D', c(0:(length(d.pts)-1))))
      options(warn=1)
      names(t.t) <- c('T.point', 'DE.DATE')
      t.t$DEPTH <- t.d$D.val
      t.t$DE.DATE <- as.POSIXct(t.t$DE.DATE, origin='1970-01-01 00:00:00', tz='UTC')

      t.t <- t.t[order(t.t$DE.DATE),]
      t.t <- t.t[which(!is.na(t.t$DE.DATE)),]
      d.xt <- xts(-t.t$DEPTH, order.by=t.t$DE.DATE)
      p.opt <- ifelse(length(t.pts)>10, F, T)
      d <- dygraph(d.xt) %>%
        dyOptions(drawPoints = p.opt) %>%
        dyRangeSelector()

      if(d.sum) {

        s.d <- rbind(s.dat[,match(c('S.DATE', 'MAX.DEPTH'), names(s.dat))],
                     s.dat[,match(c('S.DATE', 'MAX.DEPTH'), names(s.dat))])
        e.d <- rbind(s.dat[,match(c('E.DATE', 'MAX.DEPTH'), names(s.dat))],
                     s.dat[,match(c('E.DATE', 'MAX.DEPTH'), names(s.dat))])
        names(s.d)<- names(e.d) <- c('DATE', 'DEPTH')

        s.d$s.num <- rep(c(1:nrow(s.dat)), 2)
        e.d$s.num <- rep(c(1:nrow(s.dat)), 2)
        e.d$p.num <- s.d$p.num <- c(rep(1, nrow(s.dat)), rep(2, nrow(s.dat)))
        e.d$p.num <- e.d$p.num+2
        s.d$DEPTH[c(1:nrow(s.dat))] <- 0
        e.d$DEPTH[c((nrow(s.dat)+1):nrow(e.d))] <- 0
##        s.d$DATE[c((nrow(s.dat)+1):nrow(s.d))] <- s.d$DATE[c((nrow(s.dat)+1):nrow(s.d))]+1
##        e.d$DATE[c(1:nrow(s.dat))] <- e.d$DATE[c(1:nrow(s.dat))]-1

        b.d <- rbind(s.d, e.d)
        b.d <- b.d[order(b.d$s.num, b.d$p.num),]

        s.a <- merge(t.t, b.d, by.x='DE.DATE', by.y='DATE', all.x=T, all.y=T)
        reorder <- order(s.a$s.num[which(!is.na(s.a$s.num))], s.a$p.num[which(!is.na(s.a$s.num))])
        s.a[which(!is.na(s.a$s.num)),] <- s.a[which(!is.na(s.a$s.num)),][reorder,]
        s.a <- s.a[,-c(2,5,6)]
        s.a[,-1] <- -s.a[,-1]
        a.xt <- xts(s.a[,-1], order.by=s.a$DE.DATE)
        d <- dygraph(a.xt) %>%
          dyOptions(connectSeparatedPoints=T) %>%
          dySeries('DEPTH.x', label='Depth', drawPoints=p.opt, pointSize=2) %>%
          dySeries('DEPTH.y', label='6-hr max') %>%
          dyAxis("y", label = "Depth (m)") %>%
          dyLegend(show = "follow") %>%
          dyRangeSelector()

      }

      if(ctd) {
        ## Not yet implemented
      }
      d
    }
  }
}
