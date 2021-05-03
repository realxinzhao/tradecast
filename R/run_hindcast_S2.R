
#' run_hindcast_S2
#'
#' @param BASEYEAR A numeric value of base year
#' @param TARGETYEARS A vector of target years
#' @param MODEL.DATA list of all data, returned from MODEL.DATA()
#' @param BASEDATA.ALLYEARS The dataframe returned from dataproc.basedata
#' @param MODEL A function including model equations
#'
#' @return output database and print running time
#' @export

run_hindcast_S2 <- function(BASEYEAR,
                         TARGETYEARS,
                         MODEL.DATA,
                         BASEDATA.ALLYEARS,
                         MODEL = minicam_agtrade){


  #-------------------------------------------------------#
  #*Time the simulation
  start_time <- Sys.time()


  #*********************************************************#
  #*Check element inputs
  if (all(BASEYEAR %in% TARGETYEARS)) {
    warning("Target years includes base year as well!")
  } else
    if (!length(BASEYEAR) == 1 && BASEYEAR %in% BASEDATA.ALLYEARS$baseyear) {
      stop("Base year (length = 1) must be a subset of years in data")
    } else
      if (!all(TARGETYEARS %in% BASEDATA.ALLYEARS$baseyear)) {
        stop("TARGETYEARS must be a subset of years in data")
      }
  #* MODEL should be a function
  if (is.function(MODEL) == F) {
    stop("MODEL must be a function")
  }


  #*********************************************************#
  #apply for all target years
  lapply(TARGETYEARS, function(target.yr){


  #*********************************************************#
  #*config and run
    print(paste0("Running model period: ",
                 match(target.yr, TARGETYEARS), "/",
                 length(TARGETYEARS), ' (',
                 target.yr, ")"))
    model.shock.config <- model_shock_S2(BASEYEAR = BASEYEAR,
                                         TARGETYEAR = target.yr,
                                         MODEL.DATA = MODEL.DATA)

    model.out <- model_solve(CONFIG = model.shock.config,
                             FUN = MODEL)


  #*********************************************************#
  #*Translate model.out to output database and return
    model_proc_output(BASEYEAR = BASEYEAR,
                      TARGETYEAR = target.yr,
                      MODEL.DATA = MODEL.DATA,
                      DB.OUTPUT = model.out$outDB,
                      BASEDATA.ALLYEARS = BASEDATA.ALLYEARS) ->
      updated.db.equil.yr

    return(
      updated.db.equil.yr = updated.db.equil.yr)


  #*********************************************************#
  #*bind output database across years
  }) %>% bind_rows() -> updated.db.equil


  #*********************************************************#
  #*Return output database and print running time
  end_time <- Sys.time()
  print(paste0("Simulation completed; time:", format(round(end_time - start_time,2))))
  return(updated.db.equil)


  #*********************************************************#
}


