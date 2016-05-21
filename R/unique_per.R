#' Utility to check if a data frame column (name) has unique values based on a given unique ID column (uid)
#'
#' @param d Data frame
#' @param name Quoted column name
#' @param uid Quoted column name of unique id
#' @export
unique_per <- function(d, name, uid = names(d)[1]) {

  multiples <- dplyr::filter(dplyr::count_(dplyr::distinct(dplyr::select_(d, uid, name)), uid), n > 1)

  if(nrow(muliples) > 0) {
    warning(deparse(substitute(d)), " columns ", name, " is not unique per ", uid)
    return(multiples)
  } else {
    return(TRUE)
  }
}
