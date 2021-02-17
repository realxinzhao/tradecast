
#' model.solve
#' @description Solver of the model, wrapped from nleqslv.
#'
#' @param xstart A numeric vector with initial values for solving, e.g., rep(100,144)
#' @param abtol Passed to btol in nleqslv
#' @param amaxit Passed to maxit in nleqslv
#' @param config config generated from model.shock.config
#' @param amessage If T, print solution message
#'
#' @importFrom nleqslv nleqslv
#' @return Model solution
#' @export

model.solve <- function(config,
                        xstart = rep(100,144),
                        abtol = .01, amaxit = 1000, amessage = T){

  #*********************************************************#
  #*Assign config to be used in model. Note environment here!
  assign("dslnex.config", config,
         envir = globalenv())

  #*********************************************************#
  #*Call solver
  nleqslv::nleqslv(xstart, fn = dslnex,
                   control=list(btol = abtol, maxit = amaxit)) -> model.sol
  if (amessage == T) {
    print(model.sol$message)
  }

  #*********************************************************#
  return(model.sol) #returns solutions and related details

}
