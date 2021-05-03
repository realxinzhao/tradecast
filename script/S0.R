#S0

library(dplyr,lib.loc = "C:/Users/XZhaoPC/Documents/R/win-library/newer")
library(tradecast)

scenname = "S0"

parameter.exponent <- list(
  ces.demand = 1,
  logit.landsupply = -1,
  logit.Armington.reg = 3,
  logit.Armington.intl = 6
)

basedata.allyear <- dataproc_basedata()

model.data <- model_data(seq(1995, 2015,5),
                         basedata.allyear,
                         parameter.exponent
)

output_metric(
  run_hindcast(BASEYEAR = 1995,
               TARGETYEARS = seq(2000, 2015,5),
               MODEL.DATA = model.data,
               BASEDATA.ALLYEARS = basedata.allyear,
               MODEL = minicam_agtrade) )->
  sol.out




sol.out$scenname = scenname

save(sol.out, file = paste0("output/hindcast/", scenname, ".Rdata"))



