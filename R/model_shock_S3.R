
#' model_shock_S3
#'
#' @description Translate MODEL.DATA to model language of configuration info,
#'              e.g., parameters and shocks. Sourcing fn.demand.regl.sw from target
#' @param BASEYEAR A numeric value of base year
#' @param TARGETYEAR A numeric value of target year to calculate shocks or targeted exogenous variables
#' @param MODEL.DATA list of all data, returned from MODEL.DATA()
#'
#' @return A list of exogenous variable and parameters to be used in model solving
#' @export

model_shock_S3 <- function(BASEYEAR = 1995,
                        TARGETYEAR = 2015,
                        MODEL.DATA){

  #*********************************************************#
  #*Parsing data based on MODEL.DATA
  MODEL.DATA[[as.character(BASEYEAR)]] -> base
  MODEL.DATA[[as.character(TARGETYEAR)]] -> target


  model.shock.config <- list(
  #*********************************************************#
  #*Setting exogenous variables to target data (diffs are the shocks on the drivers)
    fn.margin.reg.shock = target$variable.exogenous$margin.reg.shock,
    fn.cropland.supply = target$variable.exogenous$cropland.supply,
    fn.expense = target$variable.exogenous$expense,
    fn.yield = target$variable.exogenous$yield,
    fn.biofuelfeedstock.mandate = target$variable.exogenous$biofuelfeedstock.mandate,
    fn.nlc.share = target$variable.exogenous$nlc.share,


  #*********************************************************#
  #*Setting parameters to those calibrated to base year data

    #Armington parameters: regional (micro)
    fn.logit.exponent.regl = target$parameter.exponent$logit.exponent.regl, #targeted here
    fn.demand.regl.sw = target$parameter.sw$demand.regl.sw,                 #targeted here

    #Armignton parameters: international (macro)
    fn.logit.exponent.intl = base$parameter.exponent$logit.exponent.intl,
    fn.demand.intl.sw = base$parameter.sw$demand.intl.sw,

    #CES utility function parameters
    fn.ces.exponent.demand = base$parameter.exponent$ces.exponent.demand,
    fn.demand.sw = base$parameter.sw$demand.sw,

    #Land allocation parameters
    fn.logit.exponent.land = base$parameter.exponent$logit.exponent.land,
    fn.land.sw = base$parameter.sw$land.sw,


  #*********************************************************#
  #*including Sets and target.year
    sets = base$sets,
    Year = TARGETYEAR
  )

  #*********************************************************#
  return(model.shock.config)

  #*********************************************************#
  #*Overwrite those parameters when needed
  #*
  #*
  #*
  #*  fn.demand.regl.sw = lapply(regID, function(reg.imp){
  # lapply(sectorID, function(crop){
  #   demand.regl.sw[[reg.imp]][[crop]] = demand.regl.sw[[reg.imp]][[crop]]^(1/theta.regl[[match(target.yr, target.yr.all)]])
  # }) })

}


