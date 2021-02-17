#' pull_list
#' @description converting data frame to list for modeling purposes
#' @param .df   data frame
#' @param group grouping variable to pull as vectors
#' @param val.rm variables will be removed
#'
#' @import dplyr tidyr
#' @return list of vectors
#' @export


pull_list <- function(.df, group = "reg", val.rm = c("variable", "crop")){
  .df %>% dplyr::group_by_at(vars(one_of(c(group, val.rm)))) %>%
    dplyr::summarize(value = sum(value), .groups = 'drop') %>%  #, .groups = 'drop'
    dplyr::ungroup() %>%
    tidyr::spread(group, value) %>%
    dplyr::select_at(vars(-one_of(val.rm))) %>%
    as.list() }


#' Moving average
#' @description function to calculate moving average
#'
#' @param x A data frame contain the variable for calculation
#' @param periods An odd number of the periods in MA. The default is 5, i.e., 2 lags and 2 leads
#'
#' @return A data frame
#' @export

MA.n <- function(x, periods = 5){
  if ((periods %% 2) == 0) {
    stop("Periods should be an odd value")
  } else{
    (x +
       Reduce(`+`, lapply(seq(1, (periods -1 )/2), function(a){lag(x, n = a)})) +
       Reduce(`+`,lapply(seq(1, (periods -1 )/2), function(a){lead(x, n = a)}))
    )/periods
  }
}





