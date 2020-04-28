#' \code{tables.SRDLpg} lists the tables in a SMRU PostgreSQL database
#'
#' @param theDB Name of (or path to) the pg database
#' @return Returns a character vector with database table names
#' @details
#' @family SMRU SRDL database functions
#' @seealso \code{\link{fields.SRDLpg}} for table field names,
#'   \code{\link{ref.SRDLpg}} for ref ID codes,
#'   \code{\link{dep.SRDLpg}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLpg}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @example
#' tables.SRDLpg('SRDL')
#' @importFrom DBI dbConnect dbListTables dbDriver dbListFields dbGetQuery dbDisconnect
#' @importFrom RPostgreSQL dbConnect dbListTables dbListFields dbDataType dbListTables dbWriteTable dbSendQuery dbDisconnect
#' @export

tables.SRDLpg <- function(theDB='SRDL',
                          theHost='localhost', thePort=5432, theUser='postgres', thePwd='12345') {
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = theDB,
                   host = theHost, port = thePort,
                   user = theUser, password = thePwd)
  db.tables <- dbListTables(con)
  dbDisconnect(con)
  db.tables
}


#' \code{fields.SRDLpg} lists field names form specified table in a SMRU Access database
#'
#' @param theDB Name of database
#' @param theTable Name of database table
#' @return Returns a character vector with field names from specified table
#' @details
#' @family SMRU SRDL database functions
#' @seealso \code{\link{tables.SRDLpg}} for database table names,
#'   \code{\link{ref.SRDLpg}} for ref ID codes,
#'   \code{\link{dep.SRDLpg}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLpg}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @example
#' fields.SRDLpg('SRDL', 'dive')
#' @importFrom DBI dbConnect dbListTables dbDriver dbListFields dbGetQuery dbDisconnect
#' @importFrom RPostgreSQL dbConnect dbListTables dbListFields dbDataType dbListTables dbWriteTable dbSendQuery dbDisconnect
#' @export

fields.SRDLpg <- function(theDB='SRDL', theTable='dive',
                          theHost='localhost', thePort=5432, theUser='postgres', thePwd='12345') {
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = theDB,
                   host = theHost, port = thePort,
                   user = theUser, password = thePwd)
  fld <- dbListFields(con, theTable)
  dbDisconnect(con)
  fld
}


#' \code{ref.SRDLpg} lists all ref codes (animal ID) in a SMRU PostgreSQL database
#'
#' @param theDB Name of database
#' @return Returns a character vector with all ref codes
#' @details
#' @family SMRU SRDL database functions
#' @seealso \code{\link{tables.SRDLpg}} for database table names,
#'   \code{\link{fields.SRDLpg}} for table field names,
#'   \code{\link{dep.SRDLpg}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLpg}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @example
#' ref.SRDLpg('SRDL')
#' @importFrom DBI dbConnect dbListTables dbDriver dbListFields dbGetQuery dbDisconnect
#' @importFrom RPostgreSQL dbConnect dbListTables dbListFields dbDataType dbListTables dbWriteTable dbSendQuery dbDisconnect
#' @export

ref.SRDLpg <- function(theDB='SRDL',
                       theHost='localhost', thePort=5432, theUser='postgres', thePwd='12345') {
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = theDB,
                   host = theHost, port = thePort,
                   user = theUser, password = thePwd)
  qry <- dbGetQuery(con, 'SELECT ref FROM deployments')
  dbDisconnect(con)
  qry
}


#' \code{dep.SRDLpg} retrieves the entire deployments table from a SMRU Access database
#'
#' @param theDB Name of database
#' @return Returns a data frame with all deployment metadata
#' @details
#' @seealso \code{\link{tables.SRDLpg}} for database table names,
#'   \code{\link{fields.SRDLpg}} for table field names,
#'   \code{\link{ref.SRDLpg}} for ref ID codes,
#'   \code{\link{get.SRDLpg}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @example
#' dep.SRDLpg('SRDL')
#' @importFrom DBI dbConnect dbListTables dbDriver dbListFields dbGetQuery dbDisconnect
#' @importFrom RPostgreSQL dbConnect dbListTables dbListFields dbDataType dbListTables dbWriteTable dbSendQuery dbDisconnect
#' @export

dep.SRDLpg <- function(theDB='SRDL',
                       theHost='localhost', thePort=5432, theUser='postgres', thePwd='12345') {
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = theDB,
                   host = theHost, port = thePort,
                   user = theUser, password = thePwd)
  qry <- dbGetQuery(con, 'SELECT * FROM deployments')
  dbDisconnect(con)
  qry$ref <- as.factor(qry$ref)
  qry
}


#' \code{get.SRDLpg} runs a query to fetch data from SMRU PostgreSQL database table, based on set criteria
#'
#' @param theDB Name of database
#' @param theTable Name of database table
#' @param theFields Vector of names or numbers representing which fields to be fetched,
#'   or "All" to get all tables.
#' @param theDep Either an exact name of a deployment program to be loaded,
#'   or a wildcard for similarly named deployment programs.
#' @param theRef ref code of specific animal for which data should be fetched.
#'   Can be either "All" for entire table, or one specific ref code.
#'   There is currently no support for subsets of multiple ref codes
#' @return Returns a data frame with data from table following specified selection criteria.
#' @details
#' @seealso \code{\link{tables.SRDLpg}} for database table names,
#'   \code{\link{fields.SRDLpg}} for table field names,
#'   \code{\link{ref.SRDLpg}} for ref ID codes,
#'   \code{\link{dep.SRDLpg}} for retrieving deployments metadata
#' @author Martin Biuw
#' @examples
#' dive <- get.SRDLpg('SRDL', 'dive', 'All', 'ct34-2447-08')
#' dive <- get.SRDLpg('SRDL', 'dive', c('ref', 'de_date', 'surf_dur', 'dive_dur', 'max_dep')
#' @importFrom DBI dbConnect dbListTables dbDriver dbListFields dbGetQuery dbDisconnect
#' @importFrom RPostgreSQL dbConnect dbListTables dbListFields dbDataType dbListTables dbWriteTable dbSendQuery dbDisconnect
#' @export

get.SRDLpg <- function(theDB='SRDL', theTable='dive', theFields='All', theDep='ct34', theRef='All',
                       theHost='localhost', thePort=5432, theUser='postgres', thePwd='12345') {
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = theDB,
                   host = theHost, port = thePort,
                   user = theUser, password = thePwd)
  ##  dbListFields(con, theTable)

  if(theFields[1]=='All') {
    sql <- paste('SELECT * FROM', theTable)
  } else {
    if(class(theFields)=='integer') {
      theFields <- paste(dbListFields(con, theTable)[theFields], collapse=',')
    }
    sql <- paste('SELECT', theFields, 'FROM', theTable)
  }

  if(theRef!='All') {
    dep <- dep.SRDLpg(theDB=theDB)
    thePTT <- dep$ptt[match(theRef, dep$ref)]
    if(is.na(thePTT)) stop(paste('No valid observations for ref', theRef, 'in table'))
    sql <- paste(sql, 'WHERE ptt =', thePTT)
  } else {
    if(theDep!='All') {
      sql <- paste(sql, ' WHERE ref LIKE \'', theDep, '%\'', sep='')
    }
  }
  res <- try(dbGetQuery(con, sql), silent=T)

  if(class(res)=='try-error') err.col <- vector('numeric')
  while(class(res)=='try-error') {
    err.c <- unlist(strsplit(attr(res, 'condition')$message,
                             'column number '))[[2]]
    err.col <- c(err.col, as.numeric(unlist(strsplit(err.c, ' '))[[1]]))
    theFields <- paste(dbListFields(con, theTable)[-err.col], collapse=',')

    sql <- paste('SELECT', theFields, 'FROM', theTable)
    if(theRef!='All') {
      dep <- dep.SRDLpg(theDB=theDB)
      thePTT <- dep$PTT[match(theRef, dep$ref)]
      sql <- paste(sql, 'WHERE ptt =', thePTT)
    }
    res <- try(dbGetQuery(con, sql), silent=T)
  }

  dbDisconnect(con)
  if(nrow(res)>0) {
    res$ref <- as.factor(res$ref)
    res
  } else {
    NA
  }
}

#' \code{get.all.SRDLpg} retrieves entire SMRU PostgreSQL database
#'
#' @param theDB Name of database
#' @return Returns a list of data frames, each corresponding to a SMRU Access data table.
#' @details DO not run this function without a specific deployment specified,
#' unless your PostgreSQL database is of reasonable size. Otherwise it will
#' take a very long time to load, or potentially crash.
#' @seealso \code{\link{tables.SRDLpg}} for database table names,
#'   \code{\link{fields.SRDLpg}} for table field names,
#'   \code{\link{ref.SRDLpg}} for ref ID codes,
#'   \code{\link{dep.SRDLpg}} for retrieving deployments metadata
#'   \code{\link{get.SRDLpg}} for retrieving single tables based on given selection criteria
#' @author Martin Biuw
#' @examples
#' dive <- get.all.SRDLpg('SRDL')
#' @importFrom DBI dbConnect dbListTables dbDriver dbListFields dbGetQuery dbDisconnect
#' @importFrom RPostgreSQL dbConnect dbListTables dbListFields dbDataType dbListTables dbWriteTable dbSendQuery dbDisconnect
#' @export

get.all.SRDLpg <- function(theDB='SRDL', theDep='ct34') {

  all.tabs <- tables.SRDLpg(theDB)
  if(length(all.tabs)>10) {

    valid.tabs <- c('cruise', 'ctd', 'deployments', 'diag', 'dive', 'gps',
                    'haulout', 'haulout_orig', 'sms', 'summary', 'tag_info', 'uplink')

    val.tabs <- match(valid.tabs, all.tabs)
    val.tabs <- val.tabs[which(!is.na(val.tabs))]
    all.tabs <- all.tabs[val.tabs]
  }
  for(i in all.tabs) {
    cat('Reading ', i, ' table (', match(i, all.tabs), ' of ', length(all.tabs), ')     \n', sep='')
    flush.console()
    eval(parse(text=paste(i, ' <- get.SRDLpg("', theDB, '", theTable="', i, '", theDep="', theDep, '")', sep='')))
  }
  cat('\nDone!\n')
  flush.console()
  eval(parse(text=paste('dat.ls <- list(', paste(all.tabs, collapse=', '), ')', sep='')))
  names(dat.ls) <- all.tabs
  which.dep <- match('deployments', names(dat.ls))
  tab.ls <- c(1:length(dat.ls))[-which.dep]
  dat.ls$deployments$ref <- as.factor(dat.ls$deployments$ref)
  for(t in tab.ls) {
    dat.ls[[t]]$ref <- factor(dat.ls[[t]]$ref, levels=levels(dat.ls$deployments$ref))
  }

  dat.ls
}
