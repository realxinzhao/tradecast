


#' HISTDATA
#' @description Making use of the processed data to balance base year equilibrium and return essential data needed for initializing and verifying the model.
#'
#' @param basedata.allyear results from dataproc.basedata()
#' @param hist.year A numeric value of a historical year; CALIBRATION_YEAR is used by default
#'
#' @import dplyr
#' @return A list of data including parameters and data
#' @export

HISTDATA <- function(hist.year = CALIBRATION_YEAR, abasedata.allyear){


  #*********************************************************#
  #base year data for calibration and/or results comparison
  basedata.year(hist.year, abasedata.allyear) -> BD

  #Note that margin.reg.data.name need to be defined for data sensitivity analysis
  #"margin.reg.pim_pexp.mtax.shock" is used now

  #*********************************************************#
  #*Data

  (1:length(unique(BD$basedata.regmkt$reg))) -> regID
  names(regID) <- unique(BD$basedata.regmkt$reg)
  (1:length(unique(BD$basedata.regmkt$crop))) -> sectorID
  names(sectorID) <- unique(BD$basedata.regmkt$crop)
  as.character(unique(BD$basedata.regmkt$variable)) -> vars

  lapply(vars, function(var){
    assign(var,
           pull_list(BD$basedata.regmkt %>% filter(variable == var)),
           envir = parent.env(environment())
    )
  } )

  #nlc.share was updated now and differentiated by region and crop
  cropland.supply = lapply(regID, function(reg){sum(area[[reg]]) } )
  prod <- lapply(regID, function(reg){area[[reg]] * yield[[reg]] } )

  rental <- lapply(regID, function(reg){(pp[[reg]] * nlc.share[[reg]]) * yield[[reg]] } )

  consume.dom <-
    pull_list(BD$basedata.trade %>% filter(variable == "consume.dom"), group = "reg.imp", val.rm = c("variable", "crop"))
  consume.imp <-
    pull_list(BD$basedata.trade %>% filter(variable == "export"), group = "reg.imp", val.rm = c("variable", "crop"))

  consume <- lapply(regID, function(reg){consume.dom[[reg]] +  consume.imp[[reg]]})

  biofuelfeedstock.mandate <- lapply(regID, function(reg){consume[[reg]] * bioshare[[reg]] / 100  } )

  consum.nobiof <- lapply(regID, function(reg){consume[[reg]] * (1- bioshare[[reg]] / 100)  } )

  #international import demand
  lapply(unique(BD$basedata.trade$reg.imp), function(reg){
    pull_list(BD$basedata.trade %>% filter(variable == "export", reg.exp == reg), group = "reg.imp", val.rm = c("variable", "crop"))
  }) %>% as.list -> consume.imp.reg

  prod.exp.reg = consume.imp.reg  #market clearing

  export <- lapply(regID, function(reg.exp){
    lapply(sectorID, function(crop){
      lapply(regID, function(reg.imp){prod.exp.reg[[reg.exp]][[reg.imp]][crop]  } )%>% unlist() %>% sum()
    }) %>% unlist()
  }) %>% unlist()


  #balance check
  if (any(abs((consume %>% unlist()  - consume.imp %>% unlist()) -
              (prod %>% unlist() - export)) > 0.001) == T) {
    stop( "Check data balance!" ) }

  #read margin shock as price wedge between pp and pim
  lapply(unique(BD$basedata.pricelink$reg.exp), function(region.exp){
    pull_list(BD$basedata.pricelink %>% filter(variable == "margin.reg.pim_pexp.mtax.shock", reg.exp == region.exp),
              group = "reg.imp", val.rm = c("variable", "crop"))
  }) -> margin.reg.shock

  #margins (and tariffs) are added here
  pimp.reg <- lapply(regID, function(reg.exp){
    lapply(regID, function(reg.imp){
      pp[[reg.exp]] * margin.reg.shock[[reg.exp]][[reg.imp]]
    })
  })

  #import prices
  pimp <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      lapply(regID, function(reg.exp){consume.imp.reg[[reg.exp]][[reg.imp]][[crop]]/consume.imp[[reg.imp]][[crop]] * pimp.reg[[reg.exp]][[reg.imp]][[crop]] } ) %>%
        unlist() %>% sum()
    }) %>% unlist()
  })

  #aggregated consumer prices
  pc <- lapply(regID, function(reg){consume.imp[[reg]]/consume[[reg]] * pimp[[reg]] + consume.dom[[reg]]/consume[[reg]] * pp [[reg]]})

  expense <- lapply(regID, function(reg){sum(pc[[reg]] * consume[[reg]] ) } )


  #*********************************************************#
  #* PARAMETERS & CALIBRATION

  #read and parse exponents
  ces.exponent.demand <- ces.demand
  logit.exponent.land <- logit.landsupply
  logit.exponent.regl <- lapply(sectorID, function(crop){logit.Armington.reg})
  logit.exponent.intl <- lapply(sectorID, function(crop){logit.Armington.intl})

  #Calibrating share-weights for area allocation
  land.sw <- lapply(regID, function(reg){logit.sw.cali(area[[reg]], rental[[reg]], logit.exponent.land)})

  #Calibrating share-weights for CES utility

  demand.sw <- lapply(regID, function(reg){logit.sw.cali(consum.nobiof[[reg]], pc[[reg]], ces.exponent.demand)})
  #Calibrating share-weights for regional Armington

  demand.regl.sw <- lapply(regID, function(reg){
    lapply(sectorID, function(crop){
      logit.sw.cali(c(consume.dom[[reg]][[crop]], consume.imp[[reg]][[crop]]),
                    c(pp[[reg]][[crop]], pimp[[reg]][[crop]]), logit.exponent.regl[[crop]])
    })
  })

  #Calibrating share-weights for international Armington
  demand.intl.sw <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      logit.sw.cali(
        lapply(regID, function(reg.exp){consume.imp.reg[[reg.exp]][[reg.imp]][[crop]]}) %>% unlist(),
        lapply(regID, function(reg.exp){pimp.reg[[reg.exp]][[reg.imp]][[crop]]}) %>% unlist(),
        logit.exponent.intl[[crop]])
    })
  })


  #*********************************************************#
  #*Return a list of variables, parameters, and sets

  return(list(
    variable.exogenous = list(
      expense = expense,
      biofuelfeedstock.mandate = biofuelfeedstock.mandate,
      cropland.supply = cropland.supply,
      yield = yield,
      nlc.share = nlc.share,
      margin.reg.shock = margin.reg.shock
      ),

    variable.endogenous = list(
      consume.dom = consume.dom,
      consume.imp = consume.imp,
      consume.imp.reg = consume.imp.reg,
      area = area,
      prod = prod,
      pimp.reg = pimp.reg,
      pp = pp
      ),

    parameter.exponent = list(
      ces.exponent.demand = ces.exponent.demand,
      logit.exponent.land = logit.exponent.land,
      logit.exponent.regl = logit.exponent.regl,
      logit.exponent.intl =logit.exponent.intl
      ),

    parameter.sw = list(
      land.sw = land.sw,
      demand.sw = demand.sw,
      demand.regl.sw = demand.regl.sw,
      demand.intl.sw = demand.intl.sw
      ),

    sets = list(
      regID = regID,
      sectorID = sectorID),

    year = hist.year

  ))

  #*********************************************************#

}





#' HISTDATA_ALL
#'
#' @param allyear A vector of all study years
#'
#' @return all_data, which was a list of HISTDATA() for each year
#' @export

HISTDATA_ALL <- function(allyear){


  #*********************************************************#
  basedata.allyear <- dataproc.basedata()

  lapply(allyear, function(year){
    HISTDATA(hist.year = year, abasedata.allyear = basedata.allyear)
  } ) -> all_data

  names(all_data) <- allyear

  #*********************************************************#
  return(all_data) #return data

}



