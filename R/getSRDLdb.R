#' \code{tables.SRDLdb} lists the tables in a SMRU Access database
#'
#' @param theDB Name of database as defined in ODBC data sources
#' @param thePath Path to folder where database is located, if running in 64-bit R.
#' @return Returns a character vector with database table names
#' @details The database must first be set up as a DSN data source.
#'   Note! On Windows, by default only 32-bit data sources can be used
#'   and R (or RStudio) has to be run in 32-bit mode for these functions to work.
#'   However, functionality has now been extended (experimental for now) to run under 64-bit R.
#'   For this to worl, the path to the database must be specified.
#' @family SMRU SRDL database functions
#' @seealso \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLdb}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @example
#' tables.SRDLdb('ct34')
#' @importFrom DBI dbConnect dbListTables dbDisconnect
#' @importFrom odbc odbc
#' @export

tables.SRDLdb <- function(theDB='ct34', thePath='C:/MamVisAD/SMRUAccessDatabases') {
  require(DBI)
  require(odbc)
  running <- Sys.info()[["machine"]]
  if(running=="x86") {
    con <- dbConnect(odbc::odbc(), theDB)
  } else {
    theDB <- paste(thePath, theDB, sep='/')
    con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                                               theDB,
                                                               ";"))
  }
  tab <- dbListTables(con)
  dbDisconnect(con)
  tab
}


#' \code{fields.SRDLdb} lists field names form specified table in a SMRU Access database
#'
#' @param theDB Name of database as defined in ODBC data sources
#' @param theTable Name of database table
#' @param thePath Path to folder where database is located, if running in 64-bit R.
#' @param thePath Path to folder where database is located, if running in 64-bit R.
#' @return Returns a character vector with field names from specified table
#' @details The database must first be set up as a DSN data source.
#'   Note! On Windows, by default only 32-bit data sources can be used
#'   and R (or RStudio) has to be run in 32-bit mode for these functions to work.
#'   However, functionality has now been extended (experimental for now) to run under 64-bit R.
#'   For this to worl, the path to the database must be specified.
#' @family SMRU SRDL database functions
#' @seealso \code{\link{tables.SRDLdb}} for database table names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLdb}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @example
#' fields.SRDLdb('ct34', 'dive')
#' @importFrom DBI dbConnect dbListFields dbDisconnect
#' @importFrom odbc odbc
#' @export

fields.SRDLdb <- function(theDB='ct34', theTable='dive', thePath='C:/MamVisAD/SMRUAccessDatabases') {
  require(DBI)
  require(odbc)
  running <- Sys.info()[["machine"]]
  if(running=="x86") {
    con <- dbConnect(odbc::odbc(), theDB)
  } else {
    theDB <- paste(thePath, theDB, sep='/')
    con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                                               theDB,
                                                               ";"))
  }
  fld <- dbListFields(con, theTable)
  dbDisconnect(con)
  fld
}


#' \code{ref.SRDLdb} lists all ref codes (animal ID) in a SMRU Access database
#'
#' @param theDB Name of database as defined in ODBC data sources
#' @param thePath Path to folder where database is located, if running in 64-bit R.
#' @return Returns a character vector with all ref codes
#' @details The database must first be set up as a DSN data source.
#'   Note! On Windows, by default only 32-bit data sources can be used
#'   and R (or RStudio) has to be run in 32-bit mode for these functions to work.
#'   However, functionality has now been extended (experimental for now) to run under 64-bit R.
#'   For this to worl, the path to the database must be specified.
#' @family SMRU SRDL database functions
#' @seealso \code{\link{tables.SRDLdb}} for database table names,
#'   \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLdb}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @example
#' ref.SRDLdb('ct34')
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom odbc odbc
#' @export

ref.SRDLdb <- function(theDB='ct34') {
  require(DBI)
  require(odbc)
  running <- Sys.info()[["machine"]]
  if(running=="x86") {
    con <- dbConnect(odbc::odbc(), theDB)
  } else {
    theDB <- paste(thePath, theDB, sep='/')
    con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                                               theDB,
                                                               ";"))
  }
  qry <- dbGetQuery(con, 'SELECT ref FROM deployments')
  dbDisconnect(con)
  qry
}


#' \code{dep.SRDLdb} retrieves the entire deployments table from a SMRU Access database
#'
#' @param theDB Name of database as defined in ODBC data sources
#' @param thePath Path to folder where database is located, if running in 64-bit R.
#' @return Returns a data frame with all deployment metadata
#' @details The database must first be set up as a DSN data source.
#'   Note! On Windows, by default only 32-bit data sources can be used
#'   and R (or RStudio) has to be run in 32-bit mode for these functions to work.
#'   However, functionality has now been extended (experimental for now) to run under 64-bit R.
#'   For this to worl, the path to the database must be specified.
#' @seealso \code{\link{tables.SRDLdb}} for database table names,
#'   \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{get.SRDLdb}} for querying and retrieving data from database table
#' @author Martin Biuw
#' @example
#' dep.SRDLdb('ct34')
#' @importFrom DBI dbConnect dbListTables dbDisconnect
#' @importFrom odbc odbc
#' @export

dep.SRDLdb <- function(theDB='ct34', thePath='C:/MamVisAD/SMRUAccessDatabases') {
  require(DBI)
  require(odbc)
  running <- Sys.info()[["machine"]]
  if(running=="x86") {
    con <- dbConnect(odbc::odbc(), theDB)
  } else {
    theDB <- paste(thePath, theDB, sep='/')
    con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                                               theDB,
                                                               ";"))
  }
  qry <- dbGetQuery(con, 'SELECT * FROM deployments')
  dbDisconnect(con)
  qry
}


#' \code{get.SRDLdb} runs a query to fetch data from SMRU Access database table, based on set criteria
#'
#' @param theDB Name of database as defined in ODBC data sources
#' @param theTable Name of database table
#' @param theFields Vector of names or numbers representing which fields to be fetched,
#'   or "All" to get all tables.
#' @param theRef ref code of specific animal for which data should be fetched.
#'   Can be either "All" for entire table, or one specific ref code.
#'   There is currently no support for subsets of multiple ref codes
#' @param thePath Path to folder where database is located, if running in 64-bit R.
#' @return Returns a data frame with data from table following specified selection criteria.
#' @details The database must first be set up as a DSN data source.
#'   Note! On Windows, by default only 32-bit data sources can be used
#'   and R (or RStudio) has to be run in 32-bit mode for these functions to work.
#'   However, functionality has now been extended (experimental for now) to run under 64-bit R.
#'   For this to worl, the path to the database must be specified.
#' @seealso \code{\link{tables.SRDLdb}} for database table names,
#'   \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata
#' @author Martin Biuw
#' @examples
#' dive <- get.SRDLdb('ct34', 'dive', c(c(1:35), c(79:80)), 'ct34-2447-08')
#' dive <- get.SRDLdb('ct34', 'dive', c('ref', 'DE_DATE', 'SURF_DUR', 'DIVE_DUR', 'MAX_DEP')
#' @importFrom DBI dbConnect dbListFields dbGetQuery dbDisconnect
#' @importFrom odbc odbc
#' @export

get.SRDLdb <- function(theDB='ct34', theTable='dive', theFields='All',
                       theRef='ct34-2447-08', thePath='C:/MamVisAD/SMRUAccessDatabases') {
  require(DBI)
  require(odbc)
  running <- Sys.info()[["machine"]]
  if(running=="x86") {
    con <- dbConnect(odbc::odbc(), theDB)
  } else {
    theDB <- paste(thePath, theDB, sep='/')
    con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                                                               theDB,
                                                               ";"))
  }
  if(theFields[1]=='All') {
    sql <- paste('SELECT * FROM', theTable)
  } else {
    if(class(theFields)=='integer') {
      theFields <- paste(dbListFields(con, theTable)[theFields], collapse=',')
    }
    sql <- paste('SELECT', theFields, 'FROM', theTable)
  }

  if(theRef!='All') {
    dep <- dep.SRDLdb(theDB=theDB)
    thePTT <- dep$PTT[match(theRef, dep$ref)]
    if(is.na(thePTT)) stop(paste('No valid observations for ref', theRef, 'in table'))
    sql <- paste(sql, 'WHERE PTT =', thePTT)
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
      dep <- dep.SRDLdb(theDB=theDB)
      thePTT <- dep$PTT[match(theRef, dep$ref)]
      sql <- paste(sql, 'WHERE PTT =', thePTT)
    }
    res <- try(dbGetQuery(con, sql), silent=T)
  }

  dbDisconnect(con)
  names(res) <- gsub('_', '.', names(res), fixed=T)
  if(theTable=='dive') {
    match.lowercase <- match(c('lat', 'lon'), names(res))
    if(!any(is.na(match.lowercase))) names(res)[match(c('lat', 'lon'), names(res))] <- c('LAT', 'LON')
    res$L.DATE <-res$DE.DATE+((res$LON/360)*3600)
    res$L.TIME <- as.POSIXct(strptime(paste('1970-01-01',
                                            format(res$L.DATE, '%H:%M:%S')),
                                      '%Y-%m-%d %H:%M:%S'), tz='GMT')
    res$L.DAY <- as.POSIXct(strptime(format(res$L.DATE, '%Y-%m-%d'),
                                      '%Y-%m-%d'), tz='GMT')

  }
  if(theTable=='ctd') {
    if('LON' %in% names(res)) {
      res$L.DATE <-res$END.DATE+((res$LON/360)*3600)
    } else {
      res$L.DATE <-res$END.DATE+((res$lon/360)*3600)
    }
    res$L.TIME <- as.POSIXct(strptime(paste('1970-01-01',
                                            format(res$L.DATE, '%H:%M:%S')),
                                      '%Y-%m-%d %H:%M:%S'), tz='GMT')
    res$L.DAY <- as.POSIXct(strptime(format(res$L.DATE, '%Y-%m-%d'),
                            '%Y-%m-%d'), tz='GMT')
  }
  if(theTable=='diag') {
    res$L.DATE <-res$D.DATE+((res$LON/360)*3600)
    res$L.TIME <- as.POSIXct(strptime(paste('1970-01-01',
                                            format(res$L.DATE, '%H:%M:%S')),
                                      '%Y-%m-%d %H:%M:%S'), tz='GMT')
    res$L.DAY <- as.POSIXct(strptime(format(res$L.DATE, '%Y-%m-%d'),
                            '%Y-%m-%d'), tz='GMT')
  }
  if('REF' %in% names(res)) {
    res$REF <- as.factor(res$REF)
  }
  if('ref' %in% names(res)) {
    res$ref <- as.factor(res$ref)
  }
  res
}

#' \code{get.all.SRDLdb} retrieves entire SMRU Access database
#'
#' @param theDB Name of database as defined in ODBC data sources
#' @param thePath Path to folder where database is located, if running in 64-bit R.
#' @return Returns a list of data frames, each corresponding to a SMRU Access data table.
#' @details The database must first be set up as a DSN data source.
#'   Note! On Windows, by default only 32-bit data sources can be used
#'   and R (or RStudio) has to be run in 32-bit mode for these functions to work.
#'   However, functionality has now been extended (experimental for now) to run under 64-bit R.
#'   For this to worl, the path to the database must be specified.
#' @seealso \code{\link{tables.SRDLdb}} for database table names,
#'   \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata
#'   \code{\link{get.SRDLdb}} for retrieving single tables based on given selection criteria
#' @author Martin Biuw
#' @examples
#' dive <- get.all.SRDLdb('ct34')
#' @importFrom DBI dbConnect dbListTables dbListFields dbGetQuery dbDisconnect
#' @importFrom odbc odbc
#' @export

get.all.SRDLdb <- function(theDB='ct34', thePath='C:/MamVisAD/SMRUAccessDatabases') {
  all.tabs <- tables.SRDLdb(theDB, thePath)
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
    eval(parse(text=paste(i, ' <- get.SRDLdb("', theDB, '", theTable="', i, '", theRef="All", thePath="', thePath, '")', sep='')))
  }
  cat('\nDone!\n')
  flush.console()
  eval(parse(text=paste('dat.ls <- list(', paste(all.tabs, collapse=', '), ')', sep='')))
  names(dat.ls) <- all.tabs
  which.dep <- match('deployments', names(dat.ls))
  tab.ls <- c(1:length(dat.ls))[-which.dep]
  if('REF' %in% names(dat.ls$deployments)) {
    names(dat.ls$deployments)[match('REF', names(dat.ls$deployments))] <- 'ref'
  }
  if('ref' %in% names(dat.ls$deployments)) {
    dat.ls$deployments$ref <- as.factor(dat.ls$deployments$ref)
  }
  for(t in tab.ls) {
    if('REF' %in% names(dat.ls[[t]])) {
      dat.ls[[t]]$REF <- factor(dat.ls[[t]]$REF, levels=levels(dat.ls$deployments$ref))
    }
    if('ref' %in% names(dat.ls[[t]])) {
      dat.ls[[t]]$ref <- factor(dat.ls[[t]]$ref, levels=levels(dat.ls$deployments$ref))
    }
  }
  dat.ls
}


#' \code{mdb2pg} Write entire SMRU Access database to PostgreSQL
#'
#' @param mdb Either a character string giving the name of MS Access database
#' as defined in ODBC data sources or the name of an R list which is the result of
#' reading an entire SMRU Access deployments database (using the \code{\link{get.all.SRDLdb}} function)
#' @param pg Name of PostgreSQL database into which tables should be written
#' @param append Logical, indicating whether to create table (default) or append to existing table
#' @return Returns nothing, but re-creates a PostgreSQL representation of the MS Access database,
#' potentially merging several deployments into single set of tables.
#' @details The source MS Access database must first be set up as a DSN data source.
#'   Note! On Windows, only 32-bit data sources can be used,
#'   and R (or RStudio) has to be run in 32-bit mode for these functions to work.
#'   In addition, a PostgreSQL/PostGIS database must exist that can hold the tables.
#' @seealso \code{\link{tables.SRDLdb}} for database table names,
#'   \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata
#'   \code{\link{get.SRDLdb}} for retrieving single tables based on given selection criteria
#'   \code{\link{get.all.SRDLdb}} for retrieving entire SMRU Access database
#' @author Martin Biuw
#' @examples
#' mdb2pg('ct34')
#' for(i in all.mdb) {
#'   cat('Converting database', i, '(', match(i, all.mdb), 'of', length(all.mdb), ')\n')
#'   flush.console()
#'   if(match(i, all.db)==1) {
#'     mdb2pg(i)
#'   } else{
#'     mdb2pg(i)
#'   }
#' }
#' @importFrom DBI dbConnect dbDriver dbListTables dbListFields dbGetQuery dbDisconnect
#' @importFrom RPostgreSQL dbConnect dbListTables dbDataType dbListTables dbWriteTable dbSendQuery dbDisconnect
#' @importFrom odbc odbc
#' @export

mdb2pg <- function(mdb='ct34', pg='SRDL', append=F) {
  if(class(mdb)=='character') {
    db.in <- get.all.SRDLdb(theDB=mdb)
  } else {
    db.in <- mdb
  }
  for(i in 1:length(db.in)) names(db.in[[i]]) <- tolower(names(db.in[[i]]))
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = pg,
                   host = "localhost", port = 5432,
                   user = "postgres", password = '12345')
  db.tables <- dbListTables(con)
  if(!any(db.tables %in% names(db.in))) {
    for(i in 1:length(db.in)) {
      cat('Writing table', names(db.in)[i], ' to PostgreSQL database ',  pg, '(', i, 'of', length(db.in), ')\n')
      flush.console()
      field.types <- unlist(lapply(c(1:length(db.in[[i]])), function(x) dbDataType(drv, db.in[[i]][,x])))
      if(length(grep('time zone', field.types))>0) {
        field.types[grep('time zone', field.types)] <- gsub(' with time zone', '', field.types[grep('time zone', field.types)])
      }
      f.typestr <- paste('field.types<-list(', paste(paste(names(db.in[[i]]), '="', field.types, '"', sep=''), collapse=', '), ')', sep='')
      eval(parse(text=paste('dbWriteTable(con,"', names(db.in)[i], '", db.in$', names(db.in)[i], ', ', f.typestr, ', row.names=F)', sep='')))
    }
    cat('\nDone!\n')
    flush.console()
  } else {
    if(!append) {
      stop('One or more tables already exist in target database')
    } else {
      for(i in 1:length(db.in)) {
        cat('Writing table', names(db.in)[i], 'to PostgreSQL database ',  pg, '(', i, 'of', length(db.in), ')\n')
        flush.console()
        field.types <- unlist(lapply(c(1:length(db.in[[i]])), function(x) dbDataType(drv, db.in[[i]][,x])))
        if(length(grep('time zone', field.types))>0) {
          field.types[grep('time zone', field.types)] <- gsub(' with time zone', '', field.types[grep('time zone', field.types)])
        }
        if(names(db.in)[i] %in% db.tables) {
          mdb.names <- names(db.in[[i]])
          pg.names <- dbListFields(con, names(db.in)[i])
          no.exist <- which(!mdb.names %in% pg.names)

          if(length(no.exist)>0) {
            for(j in no.exist) {
              new.name <- names(db.in[[i]])[j]
              tab.sql <- paste('ALTER TABLE ',
                               names(db.in)[i],
                               ' ADD COLUMN "',
                               new.name, '" ',
                               field.types[j],
                               sep='')
              dbSendQuery(con, tab.sql)
            }
          }
          f.typestr <- paste('field.types<-list(', paste(paste(names(db.in[[i]]), '="', field.types, '"', sep=''), collapse=', '), ')', sep='')

          eval(parse(text=paste('dbWriteTable(con,"', names(db.in)[i], '", db.in$', names(db.in)[i], ', append=T, row.names=F)', sep='')))
        } else {
          f.typestr <- paste('field.types<-list(', paste(paste(names(db.in[[i]]), '="', field.types, '"', sep=''), collapse=', '), ')', sep='')

          eval(parse(text=paste('dbWriteTable(con,"', names(db.in)[i], '", db.in$', names(db.in)[i], ', append=F, row.names=F)', sep='')))
        }
      }
      cat('\nDone!\n')
      flush.console()
    }
  }
  dbDisconnect(con)
}

