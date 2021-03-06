% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_Citavi_ctv6.R
\name{update_Citavi_ctv6}
\alias{update_Citavi_ctv6}
\title{Write into Citavi database (via SQL update query)}
\usage{
update_Citavi_ctv6(
  CitDat = NULL,
  path = NULL,
  CitDBTableName = NULL,
  CitDatVarToCitDBTableVar = NULL,
  quiet = TRUE
)
}
\arguments{
\item{CitDat}{A table (likely originally read via \code{read_Citavi_ctv6}).}

\item{path}{Path to the local Citavi project file (.ctv6, \emph{i.e.} \code{CitDB}).}

\item{CitDBTableName}{Name of the table from the connected Citavi database to be updated.}

\item{CitDatVarToCitDBTableVar}{Names of Citavi fields to be updated for all references
in the Citavi project. Such a name must be both: (i) a column name in \code{CitDat}
and (ii) a respective field name in the \code{CitDB}. SQL queries will be created to update
all \code{CitDatVarToCitDBTableVar} for all references/\code{StaticIDs}.}

\item{quiet}{If \code{TRUE} (default), all output will be suppressed.}
}
\description{
Write into Citavi database (via SQL update query)
}
\details{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} \cr
The underlying core functions are \code{DBI::dbConnect()} \code{RSQLite::SQLite()} and \code{DBI::dbSendQuery()}.
}
\examples{
library(dplyr, warn.conflicts = FALSE)

# use this package's example Citavi project
example_path <- example_file("3dupsin5refs/3dupsin5refs.ctv6")
CitDat <- read_Citavi_ctv6(example_path)

# looks like this
CitDat \%>\% select(StaticIDs, Title, Year)

# let's set all years to 1990
CitDat \%>\%
  mutate(AlteredYear = "1990") \%>\%
  update_Citavi_ctv6(
    path = example_path,
    CitDBTableName = "Reference",
    CitDatVarToCitDBTableVar = c("AlteredYear" = "Year"),
    quiet = FALSE
  )

# it worked!
read_Citavi_ctv6(example_path) \%>\% select(StaticIDs, Title, Year)

# we should change it back
CitDat \%>\%
  mutate(OriginalYear = c("2018", "2019", "2019", "2019", "2019")) \%>\%
  update_Citavi_ctv6(
    path = example_path,
    CitDBTableName = "Reference",
    CitDatVarToCitDBTableVar = c("OriginalYear" = "Year"),
    quiet = FALSE
  )

# it worked!
read_Citavi_ctv6(example_path) \%>\% select(StaticIDs, Title, Year)

}
