#' \code{map.SRDL} makes a map of SRDL position (diag) data
#' @param di.dat Position data to be mapped
#' @param simple Should a simple map be plotted (default)
#'   or a leaflet map (set \code{type="leaflet"})
#' @param theRef Which animal tracks should be plotted.
#'   Default is to plot all animals, otherwise give vector of ref names
#' @param filter Filtering to be used for mapping. Default is to use the V.MASK filter.
#'   Otherwise specify the column name containing the filter.
#' @param include Give filter value for observation to include
#' @param tail Specify length (in days) of recent track that should be highlighted
#'   (in colour and thick)
#' @param coverage Should map cover all historical tracks ("All") or only tail? Only implemented for simple plot.
#' @param ref.time Should reference time be current ("now") or last recorded time in data ("last")?
#' @param save.file If NA, plots map in Rstudio viewer, if filename is supplied it saves an html file.
#' @return Returns a map, either as standard with coastline from \code{worldHires} in \code{mapdata}
#'   or an interactive leaflet (if \code{type='leaflet'})
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
#' map.SRDLdb(hp4$diag, type="leaflet")
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
  if(theRef!="All") {
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
      if(diff(range(di.dat$lon, na.rm=T))>300) {
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

    if(diff(range(di.dat$lon, na.rm=T))>300) {
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
