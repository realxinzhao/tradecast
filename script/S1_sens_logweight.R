#S1

library(dplyr,lib.loc = "C:/Users/XZhaoPC/Documents/R/win-library/newer")
library(tradecast)

scenname = "S1"

parameter.exponent <- list(
  ces.demand = 1,
  logit.landsupply = -1,
  logit.Armington.reg = 3,
  logit.Armington.intl = 6
)

basedata.allyear <- dataproc_basedata()

# model_hindcast(PARAMETER = c(1.46,1.50),
#                PARAMETER.EXPONENT = parameter.exponent,
#                BASEYEAR = 1995,
#                TARGETYEARS = seq(2000, 2015,5),
#                BASEDATA.ALLYEARS = basedata.allyear,
#                LOG.WEIGHT = T)

run_hindcast_optim(PAR.START = c(1.46,1.50),
                   FUN = model_hindcast,
                   PARAMETER.EXPONENT = parameter.exponent,
                   BASEYEAR = 1995,
                   TARGETYEARS = seq(2000, 2015,5),
                   BASEDATA.ALLYEARS = basedata.allyear,
                   LOG.WEIGHT = T,
                   LOWER = c(0.1, 0.1),
                   UPPER = c(5, 10),
                   HESSIAN = T) -> sol.out


dof = (6^3 * 4  - length(sol.out[[1]]$par))
sol.out$se = sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))
sol.out$scenname = scenname

save(sol.out, file = paste0("output/hindcast/", scenname, ".Rdata"))

