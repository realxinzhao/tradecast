

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



model_hindcast(PARAMETER = c(3, 6),
               BASEYEAR = 1995,
               TARGETYEARS = seq(2000, 2015,5),
               BASEDATA.ALLYEARS = basedata.allyear)

run_hindcast_optim(PAR.START = c(1.46,1.50),
                   FUN = model_hindcast,
                   BASEYEAR = 1995,
                   TARGETYEARS = seq(2000, 2015,5),
                   HESSIAN = T,
                   BASEDATA.ALLYEARS = basedata.allyear) -> sol.out



load("output/hindcast/S1.Rdata")
sol.out -> S1
S1$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))

load("output/hindcast/S2.Rdata")
sol.out -> S2
S2$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))

load("output/hindcast/S3.Rdata")
sol.out -> S3
S3$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))



load("output/hindcast/S2_sens_BY2000.Rdata")
sol.out -> BY2000
BY2000$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))


load("output/hindcast/S2_sens_BY2005.Rdata")
sol.out -> BY2005
BY2005$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))

load("output/hindcast/S2_sens_BY2010.Rdata")
sol.out -> BY2010
BY2010$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))


load("output/hindcast/S2_sens_BY2015.Rdata")
sol.out -> BY2015
BY2015$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))



load("output/hindcast/S2_sens_demand_low.Rdata")
sol.out -> S2_sens_demand_low
S2_sens_demand_low$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))

load("output/hindcast/S2_sens_demand_high.Rdata")
sol.out -> S2_sens_demand_high
S2_sens_demand_high$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt( diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))


# optimHess(par = S2_sens_demand_high$sol$par,
#           fn = model_hindcast_S2,
#           PARAMETER.EXPONENT = parameter.exponent,
#           BASEYEAR = 1995,
#           TARGETYEARS = seq(2000, 2015,5),
#           BASEDATA.ALLYEARS = basedata.allyear,
#           control = list(pgtol = 0.00001, maxit = 1000)) -> ABC




load("output/hindcast/S2_sens_demand_high_3.Rdata")
sol.out


load("output/hindcast/S2_sens_land_high.Rdata")
sol.out -> S2_sens_land_high
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt( diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))


S2_sens_land_high$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))

load("output/hindcast/S2_sens_land_low.Rdata")
sol.out -> S2_sens_land_low
S2_sens_land_low$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))


load("output/hindcast/S2_sens_weight.Rdata")
sol.out -> S2_sens_weight
S2_sens_weight$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))

load("output/hindcast/S1_sens_weight.Rdata")
sol.out -> S1_sens_weight
S1_sens_weight$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))

load("output/hindcast/S3_sens_weight.Rdata")
sol.out -> S3_sens_weight
S3_sens_weight$sol
dof = (6^3 * 4  - length(sol.out[[1]]$par))
sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))




