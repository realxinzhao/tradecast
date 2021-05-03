


#' logit.sw.cali
#' @description Calibrating share-weights to be used in relative cost logit sharing function
#' @param q A vector of quantities in the nest
#' @param p A vector of prices in the nest
#' @param logit.exponent A numeric value of logit exponent
#' @param sw_relative Options of scaling method in share-weights:
#'        "sw_share" returns share-weights summing to 1 and
#'        "sw_default" returns share-weights anchored to the first item
#' @import dplyr tibble
#' @return Calibrated share-weights
#' @export

logit.sw.cali <- function(q, p, logit.exponent, sw_relative = "sw_share"){

  # Silence package checks
  sw <- ID <- NULL

  if( length(q[!is.na(q)]) < 2 ) {
    stop( "argument 'q' must include at least 2 non NA rows" )
  } else if (is.null(logit.exponent)) {
    stop( "specifying logit.exponent is required" )
  }
  sw.cali <- tibble::tibble(q, p) %>%
    dplyr::mutate(ID = row_number()) %>%
    dplyr::arrange(desc(q)) %>%
    dplyr::mutate(
      sw = ifelse(is.finite(p * q^(1/logit.exponent)), p * q^(1/logit.exponent), 0),
      sw_default = sw / first(sw),
      sw_share = sw / sum(sw)) %>%
    dplyr::arrange(ID)
  return(sw.cali %>% dplyr::pull(sw_relative))
}





#volume share from logit
#' logit.share
#' @description A function generating relative cost logit sharing given parameters and prices
#' @param p A vector of prices in the nest
#' @param logit.exponent A numeric value of logit exponent
#' @param share.weight Share-weights to be used in the function
#' @return A vector of volume shares from the logit sharing function
#' @export

logit.share <- function(p, logit.exponent, share.weight){
  if (is.null(logit.exponent)) {
    stop( "specifying logit.exponent is required" )
  } else if (length(p[!is.na(p)]) != length(share.weight[!is.na(share.weight)])) {
    stop( "share-weights and p have different length" )
  }
  y <- numeric(length(p))
  y <- p^(-logit.exponent)*share.weight^(logit.exponent)/sum(p^(-logit.exponent)*share.weight^(logit.exponent))
  return(y)
}



#value share from CES
#' ces.share
#' @description A constant elasticity of substitution function generating value sharing
#' @param p A vector of prices in the nest
#' @param ces.exponent A numeric value of CES exponent
#' @param share.weight Share-weights to be used in the function
#' @return A vector of value shares from the logit sharing function
#' @export

ces.share <- function(p, ces.exponent, share.weight){
  if (is.null(ces.exponent)) {
    stop( "specifying ces.exponent is required" )
  } else if (length(p[!is.na(p)]) != length(share.weight[!is.na(share.weight)])) {
    stop( "share-weights and p have different length" )
  }
  y <- p^(1-ces.exponent)*share.weight^(ces.exponent)/sum(p^(1-ces.exponent)*share.weight^(ces.exponent))
  return(y)
}
