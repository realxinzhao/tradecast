#S3

library(dplyr,lib.loc = "C:/Users/XZhaoPC/Documents/R/win-library/newer")
library(tradecast)

scenname = "S3"

parameter.exponent <- list(
  ces.demand = 1,
  logit.landsupply = -1,
  logit.Armington.reg = 3,
  logit.Armington.intl = 6
)

basedata.allyear <- dataproc_basedata()

# model_hindcast_S3(PARAMETER = c(1.1,1.50, c(1.11, 1.16, 1.23, 1.5)),
#                   PARAMETER.EXPONENT = parameter.exponent,
#                   BASEYEAR = 1995,
#                   TARGETYEARS = seq(2000, 2015, 5),
#                   BASEDATA.ALLYEARS = basedata.allyear)

run_hindcast_optim(PAR.START = c(1.58,1.1, c(1.11, 1.16, 1.23, 1.14)),
                   FUN = model_hindcast_S3,
                   PARAMETER.EXPONENT = parameter.exponent,
                   BASEYEAR = 1995,
                   TARGETYEARS = seq(2000, 2015, 5),
                   BASEDATA.ALLYEARS = basedata.allyear,
                   LOWER = c(0.1, 0.1, rep(0.4, 4)),
                   UPPER = c(3, 8, rep(4, 4)),
                   HESSIAN = T,
                   MAXIT = 500, PGTOL = 0.0005) -> sol.out

dof = (6^3 * 4  - length(sol.out[[1]]$par))
sol.out$se = sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))
sol.out$scenname = scenname

save(sol.out, file = paste0("output/hindcast/", scenname, ".Rdata"))

# Calibration tests
lapply(seq(1995, 2015,5), function(yr){

  model_hindcast_S3(PARAMETER = c(1.1,1.50, c(1, 1, 1, 1)),
                    PARAMETER.EXPONENT = parameter.exponent,
                    BASEYEAR = yr,
                    TARGETYEARS = yr,
                    BASEDATA.ALLYEARS = basedata.allyear) -> SSE
})

