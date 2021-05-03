#Model script
#
#' Partial equilibrium agricultural economic modeling (minicam_agtrade)
#' @description The model includes system of equations representing global agricultural markets.
#'              There are currently 144 variable and 144 equations to solve.
#' @param X price variables
#' @param OUTPUT A logical variable: with default "T", other results are output
#' @param CONFIG The configuration generated from model.shock.config
#' @import dplyr
#' @return The slack variables (y) in system of equations
#' @export
minicam_agtrade <- function(X, CONFIG, OUTPUT = F) {

  #*********************************************************#
  #*Define variables and sets
  y <- numeric(144)
  fn.pc <- list(X[1:6], X[7:12], X[13:18], X[19:24], X[25:30], X[31:36])
  fn.pimp <- list(X[37:42],	X[43:48],	X[49:54],	X[55:60],	X[61:66],	X[67:72])
  fn.pp <- list(X[73:78],	X[79:84],	X[85:90],	X[91:96],	X[97:102],	X[103:108])
  fn.r <- list(X[109:114],	X[115:120],	X[121:126],	X[127:132],	X[133:138],	X[139:144])

  regID = CONFIG$sets$regID
  sectorID = CONFIG$sets$sectorID

  #*********************************************************#
  #*System of equations (in sequence)

  #import price across sources; added margins
  #define pimp.reg
  fn.pimp.reg = lapply(regID, function(reg.exp){
    lapply(regID, function(reg.imp){
      fn.pp[[reg.exp]] * CONFIG$fn.margin.reg.shock[[reg.exp]][[reg.imp]]
    })
  })


  #-----------------------------------------#
  #*Consumer demand (Household and biofuels)

  #nonbiofuel expense
  fn.expense.nobiof = lapply(regID, function(reg){CONFIG$fn.expense[[reg]] - sum(CONFIG$fn.biofuelfeedstock.mandate[[reg]] %>% unlist() * fn.pc[[reg]]) })

  #CES regional crop demand: f(pc)
  fn.consume.nobiof <- lapply(regID, function(reg){ces.share(fn.pc[[reg]], CONFIG$fn.ces.exponent.demand, CONFIG$fn.demand.sw[[reg]]) * fn.expense.nobiof[[reg]] / fn.pc[[reg]] })

  #total consumption
  fn.consume <- lapply(regID, function(reg){fn.consume.nobiof[[reg]] +  CONFIG$fn.biofuelfeedstock.mandate[[reg]]})


  #-----------------------------------------#
  #*Armington trade (domestic and imorted demand)

  #demand.regl: f(pp, pimp, demand.regl.sw, consume)
  fn.consume.dom <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      (logit.share(c(fn.pp[[reg.imp]][[crop]], fn.pimp[[reg.imp]][[crop]]),
                   CONFIG$fn.logit.exponent.regl[[crop]],
                   CONFIG$fn.demand.regl.sw[[reg.imp]][[crop]]) * fn.consume[[reg.imp]][[crop]])[1] }) %>% unlist()
  })

  fn.consume.imp <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      (logit.share(c(fn.pp[[reg.imp]][[crop]], fn.pimp[[reg.imp]][[crop]]),
                   CONFIG$fn.logit.exponent.regl[[crop]],
                   CONFIG$fn.demand.regl.sw[[reg.imp]][[crop]]) * fn.consume[[reg.imp]][[crop]])[2] }) %>% unlist()
  })

  #zero-profit condition
  #aggregated consumer prices: f(pimp, pp, consume.imp, consume.dom, consume)
  y[1:36] <- lapply(regID, function(reg){
    fn.consume.imp[[reg]]/fn.consume[[reg]] * fn.pimp[[reg]] + fn.consume.dom[[reg]]/fn.consume[[reg]] * fn.pp [[reg]]}) %>%
    unlist() -
    fn.pc %>% unlist()

  #demand.intl: f(pimp.reg, demand.intl.sw, consume.imp)
  fn.consume.imp.reg0 <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      logit.share(lapply(regID, function(reg.exp){fn.pimp.reg[[reg.exp]][[reg.imp]][[crop]]}) %>% unlist(),
                  CONFIG$fn.logit.exponent.intl[[crop]],
                  CONFIG$fn.demand.intl.sw[[reg.imp]][[crop]]) * fn.consume.imp[[reg.imp]][[crop]] })
  })

  fn.consume.imp.reg <- lapply(regID, function(reg.exp){
    lapply(regID, function(reg.imp){
      lapply(sectorID, function(crop){
        fn.consume.imp.reg0[[reg.imp]][[crop]][[reg.exp]]
      }) %>% unlist()
    })
  })

  #zero-profit condition
  #import prices: f(consume, consume.imp, consume.imp.reg, pimp.reg)
  y[37:72] <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      lapply(regID, function(reg.exp){
        fn.consume.imp.reg[[reg.exp]][[reg.imp]][[crop]]/fn.consume.imp[[reg.imp]][[crop]] * fn.pimp.reg[[reg.exp]][[reg.imp]][[crop]] } ) %>%
        unlist() %>% sum()
    }) %>% unlist()
  }) %>% unlist() -
    fn.pimp %>% unlist()

  #-----------------------------------------#
  #*International price and trade volume links
  #import price across sources; added margins
  #see fn.pimp.reg in the beginning

  #implied export; mkt clearing
  fn.prod.exp.reg = fn.consume.imp.reg

  #export f(fn.prod.exp.reg)
  fn.export <- lapply(regID, function(reg.exp){
    lapply(sectorID, function(crop){
      lapply(regID, function(reg.imp){fn.prod.exp.reg[[reg.exp]][[reg.imp]][crop]  } )%>% unlist() %>% sum()
    }) %>% unlist()
  }) %>% unlist()


  #-----------------------------------------#
  #*Area allocation and Ag production

  #area supply: f(r, land.sw, cropland.supply)
  fn.area <- lapply(regID, function(reg){logit.share(fn.r[[reg]], CONFIG$fn.logit.exponent.land, CONFIG$fn.land.sw[[reg]]) * CONFIG$fn.cropland.supply[[reg]] })

  #production: f(area, yield)
  fn.prod <- lapply(regID, function(reg){fn.area[[reg]] * CONFIG$fn.yield[[reg]] }) %>% unlist()


  #-----------------------------------------#
  #Market clearing and zero-profit in production

  #market clearing
  y[73:108] <- (fn.consume %>% unlist() - fn.consume.imp %>% unlist()) - (fn.prod - fn.export)

  #zero-profit condition
  y[109:144] = (fn.pp %>% unlist() * CONFIG$fn.nlc.share %>% unlist()) * CONFIG$fn.yield %>% unlist() - fn.r %>% unlist()


  #*********************************************************#
  #return slack variables(y) for solving or
  #*return output database including key variables
  if (OUTPUT == F) {
    return(y)
    } else
      if (OUTPUT == T) {
        return(
          output = list(
            consume.dom = fn.consume.dom,
            consume.imp = fn.consume.imp,
            consume.imp.reg = fn.consume.imp.reg,
            area = fn.area,
            prod = fn.prod,
            pimp.reg = fn.pimp.reg,
            pp = fn.pp
            )
          )
        }
}
