

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
sol.out[[1]]$par + sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))*1.69  -> A
log(2) / log(sol.out[[1]]$par[6]^(1/20))
log(2) / log(A[6]^(1/20))

(sol.out[[1]]$par[6]^(1/20) )^5
(A[6]^(1/20) )^5
(A[5]^(1/15) )^5
(A[4]^(1/10) )^5
A[3]
log(2) / log(A[3]^(1/5))

sol.out[[1]]$par[6] / sol.out[[1]]$par[5]
sol.out[[1]]$par[5] / sol.out[[1]]$par[4]
sol.out[[1]]$par[4] / sol.out[[1]]$par[3]


log(2) / log((sol.out[[1]]$par[5] / sol.out[[1]]$par[4]) ^(1/5))
(sol.out[[1]]$par[5] / sol.out[[1]]$par[4])# ^(1/5)

sol.out[[1]]$par - sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))*1.69
sol.out[[1]]$par + sqrt(diag(2*sol.out[[1]]$value / dof * solve(sol.out[[1]]$hessian)))*1.69


model_hindcast_S2(PARAMETER = sol.out[[1]]$par,
               PARAMETER.EXPONENT = parameter.exponent,
               BASEYEAR = 1995,
               TARGETYEARS = seq(2000, 2015,5),
               BASEDATA.ALLYEARS = basedata.allyear, DB.OUTPUT = T) -> B

model_hindcast(PARAMETER = c(15, 30),
               PARAMETER.EXPONENT = parameter.exponent,
               BASEYEAR = 1995,
               TARGETYEARS = seq(2000, 2015,5),
               BASEDATA.ALLYEARS = basedata.allyear, DB.OUTPUT = T
              ) -> outDB_15_13

model_hindcast(PARAMETER = c(25, 25),
               PARAMETER.EXPONENT = parameter.exponent,
               BASEYEAR = 1995,
               TARGETYEARS = seq(2000, 2015,5),
               BASEDATA.ALLYEARS = basedata.allyear, DB.OUTPUT = T
) -> outDB_25_25


outDB_S0 %>% filter(scenario == "est") %>% mutate(scenario = "S0") %>%
  bind_rows(
    outDB_S1 %>% filter(scenario == "est") %>% mutate(scenario = "S1")
  ) %>% bind_rows(
    outDB_S2 %>% filter(scenario == "est") %>% mutate(scenario = "S2")
  ) %>% bind_rows(
    outDB_S3 %>% filter(scenario == "est") %>% mutate(scenario = "S3")
  ) %>% bind_rows(
    outDB_S0 %>% filter(scenario %in% c("obs", "ref"))
  ) %>% bind_rows(
    outDB_15_13$outDB %>% filter(scenario == "est") %>% mutate(scenario = "S1_15_30")
  ) %>% bind_rows(
    outDB_25_25$outDB %>% filter(scenario == "est") %>% mutate(scenario = "S1_25_25")
  ) %>%
  tidyr::gather(equil,"value", c(consumption, price)) %>%
  filter(!(reg.imp == reg.exp & variable == "export")) %>%
  mutate(reg.exp = if_else(reg.exp == "Domestic", reg.imp, reg.exp),
  ) %>%
  spread(scenario, value) %>% rename(year = target.yr) ->
  Tong

data = Tong %>% left_join(
  Tong %>%
    filter(equil == "consumption") %>%
    transmute(reg.imp, reg.exp, crop, variable, year, weight = obs) %>%
    group_by(year) %>%
    mutate(weight1 = weight ^ 0.5/ sum(weight^0.5) * sum(weight) )
) %>% ungroup()

scen= "S3"
scen= "ref"
scen = "S1_25_25"
scen = "S1_15_30"

data %>%
  transmute(reg.imp, reg.exp, crop, variable, target.yr = year, equil, weight,weight1,
            AE =  (abs(get(scen)/obs  -1)*100)) %>%
  filter(is.na(AE) == F & is.infinite(AE) == F) %>%
  group_by(equil) %>%
  summarise(AE0 = weighted.mean(AE, weight),
            AE25 = quantile(AE, probs = c(0.25)),
            AE50 = quantile(AE, probs = c(0.5)),
            AE75 = quantile(AE, probs = c(0.75)),
            weight = sum(weight)) %>%
  ungroup() %>%
  mutate(weight = weight / sum(weight) * 100) %>%
  mutate(scenario = scen)










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




