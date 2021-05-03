
#' model.solve
#' @description Solver of the model, wrapped from nleqslv.
#'
#' @param XSTART A numeric vector with initial values for solving, e.g., rep(100,144)
#' @param BTOL Passed to btol in nleqslv
#' @param MAXIT Passed to maxit in nleqslv
#' @param CONFIG The configuration generated from model.shock.config
#' @param MESSAGE If T, print solution message
#' @param FUN A function (passed to nleqslv) of XSTART returning a vector of function values with the same length as XSTART
#'
#' @importFrom nleqslv nleqslv
#' @return A list of model solution (model.sol) and output data (outDB)
#' @export

model_solve <- function(CONFIG,
                        FUN,
                        XSTART = rep(100,144),
                        BTOL = .01,
                        MAXIT = 1000,
                        MESSAGE = T){


  #*********************************************************#
  #*Call solver
  nleqslv::nleqslv(x = XSTART,
                   fn = FUN,
                   CONFIG = CONFIG,
                   OUTPUT = F,
                   control = list(btol = BTOL,
                                  maxit = MAXIT)) -> model.sol


  #*********************************************************#
  #*print solution status
  if (MESSAGE == T) {
    print(model.sol$message)
  }


  #*********************************************************#
  #*generate output database based on solutions
  FUN(X = model.sol$x,
      CONFIG = CONFIG,
      OUTPUT = T) -> outDB


  #*********************************************************#
  return(
    list(
      model.sol = model.sol,
      outDB = outDB
    )) #returns solutions and related details

}




