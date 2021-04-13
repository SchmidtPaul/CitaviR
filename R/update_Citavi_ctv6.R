#' Write into Citavi database (via SQL update query)
#'
#' @param CitDat A table (likely originally read via \code{read_Citavi_ctv6}).
#' @param path Path to the local Citavi project file (.ctv6, *i.e.* \code{CitDB}).
#' @param CitDBTableName Name of the table from the connected Citavi database to be updated.
#' @param CitDatVarToCitDBTableVar Names of Citavi fields to be updated for all references
#' in the Citavi project. Such a name must be both: (i) a column name in \code{CitDat}
#' and (ii) a respective field name in the \code{CitDB}. SQL queries will be created to update
#' all \code{CitDatVarToCitDBTableVar} for all references/\code{StaticIDs}.
#' @param quiet If \code{TRUE} (default), all output will be suppressed.
#' @details
#' `r lifecycle::badge("experimental")` \cr
#' The underlying core functions are \code{DBI::dbConnect()} \code{RSQLite::SQLite()} and \code{DBI::dbSendQuery()}.
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # use this package's example Citavi project
#' example_path <- example_file("3dupsin5refs/3dupsin5refs.ctv6")
#' CitDat <- read_Citavi_ctv6(example_path)
#'
#' # looks like this
#' CitDat %>% select(StaticIDs, Title, Year)
#'
#' # let's set all years to 1990
#' CitDat %>%
#'   mutate(AlteredYear = "1990") %>%
#'   update_Citavi_ctv6(
#'     path = example_path,
#'     CitDBTableName = "Reference",
#'     CitDatVarToCitDBTableVar = c("AlteredYear" = "Year"),
#'     quiet = FALSE
#'   )
#'
#' # it worked!
#' read_Citavi_ctv6(example_path) %>% select(StaticIDs, Title, Year)
#'
#' # we should change it back
#' CitDat %>%
#'   mutate(OriginalYear = c("2018", "2019", "2019", "2019", "2019")) %>%
#'   update_Citavi_ctv6(
#'     path = example_path,
#'     CitDBTableName = "Reference",
#'     CitDatVarToCitDBTableVar = c("OriginalYear" = "Year"),
#'     quiet = FALSE
#'   )
#'
#' # it worked!
#' read_Citavi_ctv6(example_path) %>% select(StaticIDs, Title, Year)
#'
#' @import dplyr
#' @importFrom DBI dbConnect
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbClearResult
#' @importFrom DBI dbDisconnect
#' @importFrom tidyr replace_na
#' @export
update_Citavi_ctv6 <- function(CitDat = NULL, path = NULL, CitDBTableName = NULL, CitDatVarToCitDBTableVar = NULL, quiet = TRUE){

  # Preliminary checks on CitDB ---------------------------------------------
  stopifnot(is.character(CitDBTableName))

  # import CitDBTable to be altered further below
  CitDBTable <- read_Citavi_ctv6(path = path, CitDBTableName = CitDBTableName)

  # check imported CitDBTable to be altered further below
  stopifnot(
    !is.null(CitDat),
    !all(is.null(CitDatVarToCitDBTableVar)),
    !all(is.null(names(CitDatVarToCitDBTableVar))),
    all.equal(sort(CitDat$StaticIDs), sort(CitDBTable$StaticIDs)) # CitDBTable and CitDat have identical IDs?
    # all(CitDatVarToCitDBTableVar %in% names(CitDBTable)),       # all CitDatVars    present in CitDat?
    # all(names(CitDatVarToCitDBTableVar) %in% names(CitDat))     # all CitDBTableVar present in CitDBTable?
  )


  # connect database --------------------------------------------------------
  CitDB <- DBI::dbConnect(RSQLite::SQLite(), path)


  # SQL query for each variable given in CitDatVarToCitDBTableVar -----------
  if (!quiet) {
    cat("Sending SQL queries:\n")
  }

  for (var_index in 1:length(CitDatVarToCitDBTableVar)) {
    # for each ID/Reference
    for (row_index in 1:nrow(CitDat)) {
      # create query
      query <- paste0(
        "UPDATE ",
        CitDBTableName,
        # e.g. "Review"
        " SET ",
        CitDatVarToCitDBTableVar[var_index],
        # e.g. "Abstract"
        " = '",
        CitDat %>% slice(row_index) %>%
          pull(names(CitDatVarToCitDBTableVar)[var_index]) %>%
          tidyr::replace_na(""),
        # e.g. entry in "NewAbstract"
        "' WHERE StaticIDs = '",
        CitDat %>% slice(row_index) %>% pull("StaticIDs"),
        "'"
      )

      # print query for user
      if (!quiet) {
        cat(query, "\n")
      }

      # send query
      CitDB %>%
        DBI::dbSendQuery(query) %>%
        DBI::dbClearResult()

    }
  }


  # disconnect database -----------------------------------------------------
  DBI::dbDisconnect(CitDB)


}
