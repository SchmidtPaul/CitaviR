#' @title Helper function: Negation/Opposite of %in%
#' @keywords utility
#'
#' @description \code{ %not_in% <- function(a,b){ ! a %in% b } }
#'
#' @param a vector or NULL: the values to be matched.
#' @param b vector or NULL: the values to be matched against.
#'
#' @return A logical vector, indicating if a match was not located for each element of \code{a}.
#' The values are TRUE or FALSE and never NA.
#'
#' @examples
#' 1:5 %not_in% 3:5
#'
#' @export
`%not_in%` <- function(a,b){ ! a %in% b }
