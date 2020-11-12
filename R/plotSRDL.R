#' \code{qcDive} Checks consistency of dive records
#'
#' @param d.dat Dive data to be quality controlled
#' @param flag.only logical specifying if erroneous records should be flagged or deleted
#' @param max.v.speed Threshold for maximum allowable rate of change in depth
#' between inflection points (in m/s)
#' @return Returns a data frame with either an added column with qc flag
#' (0 for OK, 1 for error), or with erroneous records deleted
#'   or an interactive dygraph (if \code{type} is set to 'dygraph')
#' @details the function checks for consistency in time sequences between
#' inflections points and unrealistic depth changes between inflection points.
#' @family SMRU SRDL functions
#' @seealso \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLdb}} for querying and retrieving data from database table
#'   \code{\link{strip.SRDL}}makes a stripchart of SRDL dive data
#' @author Martin Biuw
#' @examples
#' dive.deleted <- qcDive(dive)
#' dive.flagged <- qcDive(flagged.only=T)
#' @export

qcDive <- function(d.dat=dive, flag.only=T, max.v.speed=5) {
  inflect <- match(paste('D', c(1:25), sep=''), names(d.dat))
  inflect <- inflect[which(!is.na(inflect))]
  inflect <- inflect[unlist(lapply(inflect, function(x) !all(is.na(d.dat[,x]))))]

  d.pts <- inflect
  t.pts <- match(gsub('D', 'T', names(d.dat)[inflect]), names(d.dat))

  deleted <- vector('numeric')
  for(i in 1:length(t.pts)) {
    deleted <- c(deleted, which(d.dat[,t.pts[1]]==100 | d.dat[,t.pts[1]]==0))
  }

  deleted <- c(deleted, which(apply(d.dat[,t.pts], 1, function(x) any(diff(x)<0))))

  for(i in 1:nrow(d.dat)) {
    x <- unlist(d.dat[i,c(t.pts, d.pts)])
    if(any(is.na(x))) {
      deleted <- c(deleted, i)
    } else {
      t.ind <- c(1:(length(x)/2))
      d.ind <- c(((length(x)/2)+1):length(x))
      df.t <- diff(unlist(x[t.ind]))
      df.d <- diff(unlist(x[d.ind]))
      if(any(df.t==0)) {
        deleted <- c(deleted, i)
      }
    }
  }

  t.diff <- lapply(c(1:(length(t.pts)-1)), function(y) {
    apply(as.matrix(d.dat[,t.pts]), 1, function(x) diff(x)[y])
  })

  d.diff <- lapply(c(1:(length(d.pts)-1)), function(y) {
    apply(as.matrix(d.dat[,d.pts]), 1, function(x) diff(x)[y])
  })

  t.diff <- as.matrix(as.data.frame(t.diff))
  t.diff <- (0.01*t.diff)*as.numeric(d.dat$DIVE.DUR)
  d.diff <- as.matrix(as.data.frame(d.diff))

  deleted <- c(deleted, which(apply(t.diff, 1, min)<0))
  speed <- abs(d.diff/t.diff)
  deleted <- c(deleted, which(apply(speed, 1, function(x) any(x>max.v.speed))))
  deleted <- c(deleted, which(apply(speed, 1, function(x) any(!is.finite(x)))))

  deleted <- sort(unique(deleted))
  if(flag.only) {
    d.dat$D.MASK <- rep(0, nrow(d.dat))
    d.dat$D.MASK[deleted] <- 1
    d.dat
  } else {
    d.dat[-deleted,]
  }
}


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


#' \code{map.SRDL} makes a map of SRDL position (diag) data
#' @param di.dat Position data to be mapped
#' @param simple Should a simple map be plotted (default)
#'   or a leaflet map (set \code{type='leaflet'})
#' @param theRef Which animal tracks should be plotted.
#'   Default is to plot all animals, otherwise give vector of ref names
#' @param filter Filtering to be used for mapping. Default is to use the V.MASK filter.
#'   Otherwise specify the column name containing the filter.
#' @param include Give filter value for observation to include
#' @param tail Specify length (in days) of recent track that should be highlighted
#'   (in colour and thick)
#' @param coverage Should map cover all historical tracks ('All') or only tail? Only implemented for simple plot.
#' @param ref.time Should reference time be current ('now') or last recorded time in data ('last')?
#' @param save.file If NA, plots map in Rstudio viewer, if filename is supplied it saves an html file.
#' @return Returns a map, either as standard with coastline from '\code{worldHires} in \code{mapdata}
#'   or an interactive leaflet (if \code{type='leaflet})
#' @details The leaflet option depends on the \code{\link{leaflet} function, and the relevant
#'   package (\code{leaflet} must be installed for this option to work.
#' @family SMRU SRDL functions
#' @seealso \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLdb}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @examples
#' map.SRDLdb(hp4$diag)
#' map.SRDLdb(hp4$diag, type='leaflet')
#' @importFrom leaflet leaflet
#' @importFrom mapdata worldHires
#' @importFrom  maps map
#' @importFrom randomcoloR randomColor
#' @export

map.SRDL <- function(di.dat=hp4, type='simple', theRef='All', filter='v.mask',
                     include=0, tail=10, coverage='All', ref.time='now', save.file=NA) {
  if('diag' %in% names(di.dat)) {
    di.dat <- di.dat$diag
  } else {
    di.dat <- di.dat$gps
  }
  names(di.dat) <- tolower(names(di.dat))

  names(di.dat) <- tolower(names(di.dat))
  if(theRef!='All') {
    which.refs <- which(di.dat$ref %in% theRef)
    di.dat <- di.dat[which.refs,]
    di.dat$ref <- as.factor(as.character(di.dat$ref))
  }
  if(type=='simple') {
    library(mapdata)
    if(!is.na(filter)) {
      f.col <- grep(filter, names(di.dat), ignore.case=T)
      di.dat <- subset(di.dat, di.dat[,f.col]==include)
    }
    if(coverage=='All') {
      if(length(unique(sign(di.dat$lon)))==2) {
        theWrap=c(0, 360)
        lat <- di.dat$lat
        lon <- di.dat$lon
        lon[which(lon<0)] <- lon[which(lon<0)]+360
      } else {
        theWrap <- c(-180, 180)
        lat <- di.dat$lat
        lon <- di.dat$lon
      }
      plot(lon, lat, type='n')
      map('worldHires', add=T, col='grey', fill=T, wrap=theWrap, xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
      for(i in c(1:nlevels(di.dat$ref))) {
        which.ref <- which(di.dat$ref==levels(di.dat$ref)[i])
        lines(lon[which.ref], lat[which.ref], col='grey')
      }
    }
    set.seed(123)
    theCol <- randomcoloR::randomColor(nlevels(di.dat$ref), luminosity='bright')
    if(!is.na(tail)) {
      tail <- tail*86400
      if(ref.time=='now') {
        max.time <- Sys.time()
      } else {
        max.time <- max(di.dat$d.date)
      }
      tail.dat <- subset(di.dat, di.dat$d.date>=max.time-tail)
      tail.lon <- lon[which(di.dat$d.date>=max.time-tail)]
      tail.lat <- lat[which(di.dat$d.date>=max.time-tail)]
      tail.dat$lon <- tail.lon
      tail.dat$lat <- tail.lat
      tail.cex <- as.numeric((max.time-tail.dat$d.date))
      tail.cex <- 2*(1-(tail.cex/max(tail.cex)))
      if(coverage!='All') {
        xRange <- range(tail.dat$lon, na.rm=T)
        yRange <- range(tail.dat$lat, na.rm=T)
        plot(lat~lon, data=tail.dat, type='n', xlim=xRange, ylim=yRange, xlab='lon', ylab='lat')
        map('worldHires', add=T, col='grey', fill=T, wrap=theWrap,
            xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
        by(tail.dat, tail.dat$ref, function(x) lines(lat~lon, data=x, col='grey'))
      }
      by(tail.dat, tail.dat$ref, function(x) lines(lat~lon, data=x, col=theCol[x$ref[1]]))
      by(tail.dat, tail.dat$ref, function(x) points(lat~lon, data=x, pch=19, col=theCol[x$ref[1]],
                                                    cex=tail.cex))
      by(tail.dat, tail.dat$ref, function(x) text(x$lon[nrow(x)], x$lat[nrow(x)], x$ref[nrow(x)], col=theCol[x$ref[1]]))
    }
  } else {
    require(leaflet)
    require(RColorBrewer)
    require(randomcoloR)
    require(htmltools)
    require(htmlwidgets)
    require(webshot)
    require(fields)
    require(argosfilter)
    require(plyr)
    require(xslt)
    require(XML)

    m <- leaflet() %>%
      addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
               attribution='Copyright: IMR (Even Moland, Kjell T. Nilssen, Carla Freitas, Martin Biuw)') %>%
      mapOptions(zoomToLimits = "always")

    if(length(unique(sign(di.dat$lon)))>1) {
      di.dat$lon[which(di.dat$lon<0)] <- di.dat$lon[which(di.dat$lon<0)] + 360
    }
    cols <- unname(distinctColorPalette(nlevels(di.dat$ref)))

    for(i in 1:nlevels(di.dat$ref)) {
      grp <- levels(di.dat$ref)[i]
      tmp <- subset(di.dat, di.dat$ref==levels(di.dat$ref)[i])
      tmp <- tmp[order(tmp$d.date),]
      if('v.mask' %in% names(tmp)) {
        tmp <- tmp[which(tmp$v.mask==0),]
      }
      ##    tmp <- subset(tmp, !is.na(tmp$Latitude) & tmp$locationClass!='A')
      if(ref.time=='now') {
        max.time <- Sys.time()
      } else {
        max.time <- max(di.dat$d.date)
      }
      tmp.pts <- subset(tmp, tmp$d.date>= max.time-(tail*86400))
      m <- addPolylines(m, tmp$lon, tmp$lat, weight=1, col=cols[i], opacity=0.5, group=grp)
      if(nrow(tmp.pts)>0) {
        psize <- (tmp.pts$d.date-min(tmp.pts$d.date, na.rm=T))/as.numeric(max(tmp.pts$d.date-min(tmp.pts$d.date, na.rm=T), na.rm=T))
        d.ago <- round(difftime(max.time, tmp.pts$d.date, units='days'), digits=1)
        m <- addCircleMarkers(m, tmp.pts$lon, tmp.pts$lat, radius=4*psize, col=cols[i],
                              opacity=1, fillOpacity=0.8,
                              popup=paste('<center><b>', tmp.pts$ref, '</b><br>', tmp.pts$d.date, '<br>', d.ago, 'days ago', '</center>'),
                              ##                          label=lapply(tmp$locationDate, function(x) HTML(paste('<center><b>', grp, '</b><br>', x, '</center>', sep=''))),
                              ##                          labelOptions=labelOptions(textsize="14px"),
                              group=grp)
        if('lq' %in% names(di.dat)) {
          m <- addMarkers(m, tail(tmp.pts$lon, 1), tail(tmp.pts$lat, 1),
                          popup=paste('<center><b>', tail(tmp.pts$ref, 1), '</b><br>', tail(tmp.pts$d.date, 1), '<br>',
                                      'LC:', tail(tmp.pts$lq, 1), '</center>'))
        } else {
          m <- addMarkers(m, tail(tmp.pts$lon, 1), tail(tmp.pts$lat, 1),
                          popup=paste('<center><b>', tail(tmp.pts$ref, 1), '</b><br>', tail(tmp.pts$d.date, 1), '</center>'))
        }

      } else {
        m <- addCircleMarkers(m, tail(tmp$lon, 1), tail(tmp$lat, 1), color='black', fillColor=cols[i],
                              opacity=1, fillOpacity=0.8, weight=2, radius=7,
                              popup=paste('<center><b>', tmp$ref, '</b><br>', tmp$d.date, '</center>'))
      }
    }

    m <- addLayersControl(m, position='bottomleft',
                          overlayGroups =levels(di.dat$ref),
                          options = layersControlOptions(collapsed=FALSE))


    if(!is.na(save.file)) {
      saveWidget(m, save.file, selfcontained=F)
    } else {
      m
    }
  }
}
