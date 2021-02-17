#Model script
#
#' Partial equilibrium agricultural economic modeling (dslnex)
#' @description The model includes system of equations representing global agricultural markets.
#'              There are currently 144 variable and 144 equations to solve.
#' @param x price variables
#' @import dplyr
#' @return The slack variables (y) in system of equations
#' @export
dslnex <- function(x) {

  #*********************************************************#
  #*Define variables and sets
  y <- numeric(144)
  fn.pc <- list(x[1:6], x[7:12], x[13:18], x[19:24], x[25:30], x[31:36])
  fn.pimp <- list(x[37:42],	x[43:48],	x[49:54],	x[55:60],	x[61:66],	x[67:72])
  fn.pp <- list(x[73:78],	x[79:84],	x[85:90],	x[91:96],	x[97:102],	x[103:108])
  fn.r <- list(x[109:114],	x[115:120],	x[121:126],	x[127:132],	x[133:138],	x[139:144])

  regID = dslnex.config$sets$regID
  sectorID = dslnex.config$sets$sectorID

  #*********************************************************#
  #*System of equations (in sequence)

  #import price across sources; added margins
  #define pimp.reg
  fn.pimp.reg = lapply(regID, function(reg.exp){
    lapply(regID, function(reg.imp){
      fn.pp[[reg.exp]] * dslnex.config$fn.margin.reg.shock[[reg.exp]][[reg.imp]]
    })
  })


  #-----------------------------------------#
  #*Consumer demand (Household and biofuels)

  #nonbiofuel expense
  fn.expense.nobiof = lapply(regID, function(reg){dslnex.config$fn.expense[[reg]] - sum(dslnex.config$fn.biofuelfeedstock.mandate[[reg]] %>% unlist() * fn.pc[[reg]]) })

  #CES regional crop demand: f(pc)
  fn.consume.nobiof <- lapply(regID, function(reg){ces.share(fn.pc[[reg]], dslnex.config$fn.ces.exponent.demand, dslnex.config$fn.demand.sw[[reg]]) * fn.expense.nobiof[[reg]] / fn.pc[[reg]] })

  #total consumption
  fn.consume <- lapply(regID, function(reg){fn.consume.nobiof[[reg]] +  dslnex.config$fn.biofuelfeedstock.mandate[[reg]]})


  #-----------------------------------------#
  #*Armington trade (domestic and imorted demand)

  #demand.regl: f(pp, pimp, demand.regl.sw, consume)
  fn.consume.dom <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      (logit.share(c(fn.pp[[reg.imp]][[crop]], fn.pimp[[reg.imp]][[crop]]),
                   dslnex.config$fn.logit.exponent.regl[[crop]],
                   dslnex.config$fn.demand.regl.sw[[reg.imp]][[crop]]) * fn.consume[[reg.imp]][[crop]])[1] }) %>% unlist()
  })

  fn.consume.imp <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      (logit.share(c(fn.pp[[reg.imp]][[crop]], fn.pimp[[reg.imp]][[crop]]),
                   dslnex.config$fn.logit.exponent.regl[[crop]],
                   dslnex.config$fn.demand.regl.sw[[reg.imp]][[crop]]) * fn.consume[[reg.imp]][[crop]])[2] }) %>% unlist()
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
                  dslnex.config$fn.logit.exponent.intl[[crop]],
                  dslnex.config$fn.demand.intl.sw[[reg.imp]][[crop]]) * fn.consume.imp[[reg.imp]][[crop]] })
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
  fn.area <- lapply(regID, function(reg){logit.share(fn.r[[reg]], dslnex.config$fn.logit.exponent.land, dslnex.config$fn.land.sw[[reg]]) * dslnex.config$fn.cropland.supply[[reg]] })

  #production: f(area, yield)
  fn.prod <- lapply(regID, function(reg){fn.area[[reg]] * dslnex.config$fn.yield[[reg]] }) %>% unlist()


  #-----------------------------------------#
  #Market clearing and zero-profit in production

  #market clearing
  y[73:108] <- (fn.consume %>% unlist() - fn.consume.imp %>% unlist()) - (fn.prod - fn.export)

  #zero-profit condition
  y[109:144] = (fn.pp %>% unlist() * dslnex.config$fn.nlc.share %>% unlist()) * dslnex.config$fn.yield %>% unlist() - fn.r %>% unlist()


  #*********************************************************#
  y #return slack variables

}




#' dslnex_result
#' @description Using the same code in dslnex to process solved prices and get other results
#' @param output A logical variable: with default "T", other results are output
#' @param x price variables
#' @import dplyr
#' @return A list of output results
#' @export
#'
dslnex_result <- function(x = model.sol$x, output = T) {

  #*********************************************************#
  #*Define variables
  y <- numeric(144)
  fn.pc <- list(x[1:6], x[7:12], x[13:18], x[19:24], x[25:30], x[31:36])
  fn.pimp <- list(x[37:42],	x[43:48],	x[49:54],	x[55:60],	x[61:66],	x[67:72])
  fn.pp <- list(x[73:78],	x[79:84],	x[85:90],	x[91:96],	x[97:102],	x[103:108])
  fn.r <- list(x[109:114],	x[115:120],	x[121:126],	x[127:132],	x[133:138],	x[139:144])

  regID = dslnex.config$sets$regID
  sectorID = dslnex.config$sets$sectorID

  #*********************************************************#
  #*System of equations (in sequence)

  #import price across sources; added margins
  #define pimp.reg
  fn.pimp.reg = lapply(regID, function(reg.exp){
    lapply(regID, function(reg.imp){
      fn.pp[[reg.exp]] * dslnex.config$fn.margin.reg.shock[[reg.exp]][[reg.imp]]
    })
  })


  #-----------------------------------------#
  #*Consumer demand (Household and biofuels)

  #nonbiofuel expense
  fn.expense.nobiof = lapply(regID, function(reg){dslnex.config$fn.expense[[reg]] - sum(dslnex.config$fn.biofuelfeedstock.mandate[[reg]] %>% unlist() * fn.pc[[reg]]) })

  #CES regional crop demand: f(pc)
  fn.consume.nobiof <- lapply(regID, function(reg){ces.share(fn.pc[[reg]], dslnex.config$fn.ces.exponent.demand, dslnex.config$fn.demand.sw[[reg]]) * fn.expense.nobiof[[reg]] / fn.pc[[reg]] })

  #total consumption
  fn.consume <- lapply(regID, function(reg){fn.consume.nobiof[[reg]] +  dslnex.config$fn.biofuelfeedstock.mandate[[reg]]})


  #-----------------------------------------#
  #*Armington trade (domestic and imorted demand)

  #demand.regl: f(pp, pimp, demand.regl.sw, consume)
  fn.consume.dom <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      (logit.share(c(fn.pp[[reg.imp]][[crop]], fn.pimp[[reg.imp]][[crop]]),
                   dslnex.config$fn.logit.exponent.regl[[crop]],
                   dslnex.config$fn.demand.regl.sw[[reg.imp]][[crop]]) * fn.consume[[reg.imp]][[crop]])[1] }) %>% unlist()
  })

  fn.consume.imp <- lapply(regID, function(reg.imp){
    lapply(sectorID, function(crop){
      (logit.share(c(fn.pp[[reg.imp]][[crop]], fn.pimp[[reg.imp]][[crop]]),
                   dslnex.config$fn.logit.exponent.regl[[crop]],
                   dslnex.config$fn.demand.regl.sw[[reg.imp]][[crop]]) * fn.consume[[reg.imp]][[crop]])[2] }) %>% unlist()
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
                  dslnex.config$fn.logit.exponent.intl[[crop]],
                  dslnex.config$fn.demand.intl.sw[[reg.imp]][[crop]]) * fn.consume.imp[[reg.imp]][[crop]] })
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
  fn.area <- lapply(regID, function(reg){logit.share(fn.r[[reg]], dslnex.config$fn.logit.exponent.land, dslnex.config$fn.land.sw[[reg]]) * dslnex.config$fn.cropland.supply[[reg]] })

  #production: f(area, yield)
  fn.prod <- lapply(regID, function(reg){fn.area[[reg]] * dslnex.config$fn.yield[[reg]] }) %>% unlist()


  #-----------------------------------------#
  #Market clearing and zero-profit in production

  #market clearing
  y[73:108] <- (fn.consume %>% unlist() - fn.consume.imp %>% unlist()) - (fn.prod - fn.export)

  #zero-profit condition
  y[109:144] = (fn.pp %>% unlist() * dslnex.config$fn.nlc.share %>% unlist()) * dslnex.config$fn.yield %>% unlist() - fn.r %>% unlist()


  #*********************************************************#
  #*return output database including key variables
  if (output == T) {
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
