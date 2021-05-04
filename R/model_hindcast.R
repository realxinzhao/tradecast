

#' model_hindcast
#' @description Generate the FUN (returning GoF) to be used in hindcast optimization
#' @param PARAMETER A vector of parameters to optimize
#' @param BASEYEAR A numeric value of base year
#' @param TARGETYEARS A vector of target years
#' @param BASEDATA.ALLYEARS The dataframe returned from dataproc.basedata
#' @param LOG.WEIGHT if true using log(1+weight) as weight; otherwise weight^0.5 is used as weight in output_metric
#'
#' @return goodness-of-fit (GoF)
#' @export

model_hindcast <- function(PARAMETER,
                           PARAMETER.EXPONENT,
                           BASEYEAR,
                           TARGETYEARS,
                           BASEDATA.ALLYEARS,
                           LOG.WEIGHT = F){


  #*********************************************************#
  #*Update parameters to optimize
  PARAMETER.EXPONENT$logit.Armington.reg <- PARAMETER[1]
  PARAMETER.EXPONENT$logit.Armington.intl <- PARAMETER[2]


  #*********************************************************#
  #*Generate model.data per PARAMETER
  model_data <- model_data(HIST.YEARS = c(BASEYEAR, TARGETYEARS),
                           BASEDATA.ALLYEARS = BASEDATA.ALLYEARS,
                           PARAMETER.EXPONENT = PARAMETER.EXPONENT
  )


  #*********************************************************#
  #*Run model
  run_hindcast(BASEYEAR = BASEYEAR,
               TARGETYEARS = TARGETYEARS,
               MODEL.DATA = model_data,
               BASEDATA.ALLYEARS =  BASEDATA.ALLYEARS) ->
    DB.updated


  #*********************************************************#
  #*Evaluate results and return goodness-of-fit (GoF)
  output_metric(DF = DB.updated, LOG.WEIGHT = LOG.WEIGHT) -> GoF
  print(paste0("Parameters: ", paste(PARAMETER, collapse = ","), " SSE ", round(GoF,3)))
  return(GoF)

}
