
#' run_hindcast_optim
#' @description A wrapper of the optim function to be used for hindcast optimization
#' @param PAR.START initial parameter in optim
#' @param FUN fn function in optim
#' @param ... Pass through varibles to be used in FUN
#' @param LOWER Lower bound of parameter
#' @param UPPER Uppler bound of parameter
#' @param HESSIAN If T, producing Hessian metrix
#' @param PGTOL Tolerance in optim for the "L-BFGS-B" method
#' @param MAXIT Number of max iteration in optim
#' @importFrom stats optim
#' @importFrom assertthat assert_that
#' @return Solutions from the optimization
#' @export

run_hindcast_optim <- function(PAR.START = c(1.46,1.50),
                               FUN = model_hindcast,
                               ...,
                               LOWER = c(0.1, 0.1),
                               UPPER = c(5, 10),
                               HESSIAN = F,
                               PGTOL = 0.0001,
                               MAXIT = 1000){

  assertthat::assert_that(is.numeric(PAR.START))
  assertthat::assert_that(is.function(FUN))
  assertthat::assert_that(is.numeric(LOWER))
  assertthat::assert_that(is.numeric(UPPER))
  assertthat::assert_that(is.logical(HESSIAN))
  assertthat::assert_that(is.numeric(PGTOL))
  assertthat::assert_that(is.numeric(MAXIT))
  assertthat::assert_that(MAXIT%%1==0)
  assertthat::assert_that(length(LOWER) == length(PAR.START))
  assertthat::assert_that(length(UPPER) == length(PAR.START))

  #*********************************************************#
  start_time <- Sys.time()
  optim(par = PAR.START,
        fn = FUN,
        ...,
        method = "L-BFGS-B",
        lower = LOWER,
        upper = UPPER,
        hessian = HESSIAN,
        control = list(pgtol = PGTOL, maxit = MAXIT)) -> sol
  end_time <- Sys.time()
  print(paste0("Optimization completed; time:", format(round(end_time - start_time,2))))

  return(
    list(sol = sol,
         time = end_time - start_time)
  )

  #*********************************************************#
  #*End
}
