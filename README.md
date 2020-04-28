# rSRDL
Package containing various functions for communicating with SMRU SRDL databases in Access format. 
It contains functions for interrogating SMRU Access databases for content (e.g. tables, individuals tagged etc) 
as well as reading tables (potentially subset by individual animals) or entire databases.
tables are read in as data frames, while a database is read in as a list of data frames.

The package also contains a function for converting from Access to PostgreSQL, 
and functions for communicating with PostgreSQL databases as described for SMRU Access databases above.

NOTE 1. The Access functions will only work on Windows (for now), while the PostgreSQL functions should be platform-independent.

NOTE 2. For the package to recognise the Access databases, they will have to be set up as 32-bit or 64-bit ODBC data sources.
See [here](https://support.office.com/en-us/article/administer-odbc-data-sources-b19f856b-5b9b-48c9-8b93-07484bfab5a7) for instructions. You may have to install 64-bit support for the 64-bit version to work- See [here](https://www.microsoft.com/en-us/download/details.aspx?id=13255).


NOTE 3. R (RStudio) can be run in 32-bit or 64-bit (experimental) mode.

# Installation
On PC's running Windows, ensure you have installed [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
Install devtools and its dependencies, and finally install the rSRDL package:
```R
install('devtools')
devtools::install_github("embiuw/rSRDL")
```
