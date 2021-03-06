% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSRDLpg.R
\name{fields.SRDLpg}
\alias{fields.SRDLpg}
\title{\code{fields.SRDLpg} lists field names form specified table in a SMRU Access database}
\usage{
fields.SRDLpg(
  theDB = "SRDL",
  theTable = "dive",
  theHost = "localhost",
  thePort = 5432,
  theUser = "postgres",
  thePwd = "12345"
)
}
\arguments{
\item{theDB}{Name of database}

\item{theTable}{Name of database table}
}
\value{
Returns a character vector with field names from specified table
}
\description{
\code{fields.SRDLpg} lists field names form specified table in a SMRU Access database
}
\seealso{
\code{\link{tables.SRDLpg}} for database table names,
  \code{\link{ref.SRDLpg}} for ref ID codes,
  \code{\link{dep.SRDLpg}} for retrieving deployments metadata,
  \code{\link{get.SRDLpg}} for querying and retrieving data from database table

Other SMRU SRDL database functions: 
\code{\link{fields.SRDLdb}()},
\code{\link{ref.SRDLdb}()},
\code{\link{ref.SRDLpg}()},
\code{\link{tables.SRDLdb}()},
\code{\link{tables.SRDLpg}()}
}
\author{
Martin Biuw
}
\concept{SMRU SRDL database functions}
