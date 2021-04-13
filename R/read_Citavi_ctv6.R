#' @title Read table from Citavi database (via SQL)
#'
#' @param path Path to the local Citavi project file (.ctv6).
#' @param CitDBTableName Name of the table to be read from the connected Citavi database (via \code{DBI::dbReadTable()}).
#' Set to "Reference" by default. Shows all table names when set to NULL (via \code{DBI::dbListTables}).
#'
#' @details
#' `r lifecycle::badge("experimental")` \cr
#' The underlying core functions are \code{DBI::dbConnect()} \code{RSQLite::SQLite()}, \code{DBI::dbReadTable()} and \code{DBI::dbListTables}.
#'
#' @examples
#' # example Citavi project
#' example_path <- example_file("3dupsin5refs/3dupsin5refs.ctv6")
#'
#' # import reference (=default) table
#' CitDat <- read_Citavi_ctv6(example_path)
#' CitDat %>% dplyr::select(Title, Year, Abstract, DOI)
#'
#' # show table names
#' read_Citavi_ctv6(example_path, CitDBTableName = NULL)
#'
#' @return A tibble
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbListTables
#' @importFrom DBI dbReadTable
#' @importFrom dplyr as_tibble
#' @export

read_Citavi_ctv6 <- function(path = NULL, CitDBTableName = "Reference") {

    stopifnot(is.character(path)) # path required

    if (is.null(CitDBTableName)) {

      # connect, ListTables, disconnect -----------------------------------------
      Citcon <- DBI::dbConnect(RSQLite::SQLite(), path)
      CitDBTablesVector <- DBI::dbListTables(conn = Citcon)
      Citcon %>% DBI::dbDisconnect()

      CitDBTablesVector # return vector

    } else {

      # connect, ReadTable, disconnect ------------------------------------------
      Citcon <- DBI::dbConnect(RSQLite::SQLite(), path)
      CitDBTable <- DBI::dbReadTable(conn = Citcon, name = CitDBTableName)
      Citcon %>% DBI::dbDisconnect()


      # format ------------------------------------------------------------------
      CitDBTable <- CitDBTable %>%
        dplyr::as_tibble()


      # return tibble -----------------------------------------------------------
      CitDBTable
    }
  }
