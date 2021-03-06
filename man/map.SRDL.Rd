% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapSRDL.R
\name{map.SRDL}
\alias{map.SRDL}
\title{\code{map.SRDL} makes a map of SRDL position (diag) data}
\usage{
map.SRDL(
  di.dat = hp4,
  type = "simple",
  theRef = "All",
  filter = "v.mask",
  include = 0,
  tail = 10,
  coverage = "All",
  ref.time = "now",
  save.file = NA
)
}
\arguments{
\item{di.dat}{Position data to be mapped}

\item{theRef}{Which animal tracks should be plotted.
Default is to plot all animals, otherwise give vector of ref names}

\item{filter}{Filtering to be used for mapping. Default is to use the V.MASK filter.
Otherwise specify the column name containing the filter.}

\item{include}{Give filter value for observation to include}

\item{tail}{Specify length (in days) of recent track that should be highlighted
(in colour and thick)}

\item{coverage}{Should map cover all historical tracks ("All") or only tail? Only implemented for simple plot.}

\item{ref.time}{Should reference time be current ("now") or last recorded time in data ("last")?}

\item{save.file}{If NA, plots map in Rstudio viewer, if filename is supplied it saves an html file.}

\item{simple}{Should a simple map be plotted (default)
or a leaflet map (set \code{type="leaflet"})}
}
\value{
Returns a map, either as standard with coastline from \code{worldHires} in \code{mapdata}
  or an interactive leaflet (if \code{type='leaflet'})
}
\description{
\code{map.SRDL} makes a map of SRDL position (diag) data
}
\details{

}
\examples{
map.SRDLdb(hp4$diag)
map.SRDLdb(hp4$diag, type="leaflet")
}
\seealso{
\code{\link{fields.SRDLdb}} for table field names,
  \code{\link{ref.SRDLdb}} for ref ID codes,
  \code{\link{dep.SRDLdb}} for retrieving deployments metadata,
  \code{\link{get.SRDLdb}} for querying and retrieving data from database table

Other SMRU SRDL functions: 
\code{\link{qcDive}()},
\code{\link{strip.SRDL}()}
}
\author{
Martin Biuw
}
\concept{SMRU SRDL functions}
