

#' model_hindcast_S3
#' @description Generate the FUN (returning GoF) to be used in hindcast optimization for S3
#' @param PARAMETER A vector of parameters to optimize
#' @param BASEYEAR A numeric value of base year
#' @param TARGETYEARS A vector of target years
#' @param BASEDATA.ALLYEARS The dataframe returned from dataproc.basedata
#'
#' @return goodness-of-fit (GoF)
#' @export

model_hindcast_S3 <- function(PARAMETER,
                              PARAMETER.EXPONENT,
                              BASEYEAR,
                              TARGETYEARS,
                              BASEDATA.ALLYEARS){


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
  #*Update demand.regl.sw and using theta.regl [S3]
  theta.regl <- PARAMETER[3:6]
  regID <- BASEDATA.ALLYEARS$regID
  sectorID <- BASEDATA.ALLYEARS$sectorID

  for (target.yr in TARGETYEARS) {

    lapply(sectorID, function(crop){
      theta.regl[[match(target.yr, TARGETYEARS)]]
    }) ->
      model_data[[as.character(target.yr)]][["parameter.exponent"]][["logit.exponent.regl"]]

    lapply(regID, function(reg.imp){
      lapply(sectorID, function(crop){
        model_data[[as.character(BASEYEAR)]][["parameter.sw"]][["demand.regl.sw"]][[reg.imp]][[crop]]^(
          model_data[[as.character(BASEYEAR)]][["parameter.exponent"]][["logit.exponent.regl"]][[crop]] /
            theta.regl[[match(target.yr, TARGETYEARS)]])
      })
    }) ->
      model_data[[as.character(target.yr)]][["parameter.sw"]][["demand.regl.sw"]]
  }


  #*********************************************************#
  #*Run model
  run_hindcast_S3(BASEYEAR = BASEYEAR,
                TARGETYEARS = TARGETYEARS,
                MODEL.DATA = model_data,
                BASEDATA.ALLYEARS =  BASEDATA.ALLYEARS) ->
    DB.updated


  #*********************************************************#
  #*Evaluate results and return goodness-of-fit (GoF)
  output_metric(DB.updated) -> GoF
  print(paste0("Parameters: ", paste(PARAMETER, collapse = ","), " SSE ", round(GoF,3)))
  return(GoF)

}


