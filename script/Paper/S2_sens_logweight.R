#S2

library(dplyr,lib.loc = "C:/Users/XZhaoPC/Documents/R/win-library/newer")
library(tradecast)

scenname = "S2_sens_weight"

parameter.exponent <- list(
  ces.demand = 1,
  logit.landsupply = -1,
  logit.Armington.reg = 3,
  logit.Armington.intl = 6
)

basedata.allyear <- dataproc_basedata()

run_hindcast_optim(PAR.START = c(0.8418275, 1.4485546, c(1.1492747, 1.2005832, 1.2150673, 1.3696531)),
                   FUN = model_hindcast_S2,
                   PARAMETER.EXPONENT = parameter.exponent,
                   BASEYEAR = 1995,
                   TARGETYEARS = seq(2000, 2015,5),
                   BASEDATA.ALLYEARS = basedata.allyear,
                   LOG.WEIGHT = T,
                   LOWER = c(0.1, 0.1, rep(0.4, 4)),
                   UPPER = c(3, 8, rep(4, 4)),
                   HESSIAN = T,
                   MAXIT = 500, PGTOL = 0.0005) -> sol.out

dof = (6^3 * 4  - length(sol.out[[1]]$par))
sol.out$se = sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))
sol.out$scenname = scenname

save(sol.out, file = paste0("output/hindcast/", scenname, ".Rdata"))
