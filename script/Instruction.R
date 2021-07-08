#Brief instruction

library(dplyr)
library(tradecast)

#*********************************************

#Define parameters
parameter.exponent <- list(
  ces.demand = 1,
  logit.landsupply = -1,
  logit.Armington.reg = 3,
  logit.Armington.intl = 6
)

#Process historical data
basedata.allyear <- dataproc_basedata()

#Process data and parameters into structure that the model reads
#Base year of 1995 and shocking forward to 2015 with 5-year steps
model.data <- model_data(seq(1995, 2015,5),
                         basedata.allyear,
                         parameter.exponent
)

#Run model across historical periods and produce output data base
#Note minicam_agtrade is the model here, i.e., including all model equations
run_hindcast(BASEYEAR = 1995,
             TARGETYEARS = seq(2000, 2015,5),
             MODEL.DATA = model.data,
             BASEDATA.ALLYEARS = basedata.allyear,
             MODEL = minicam_agtrade) -> TestoutDB

#Calculate goodness of fit (GoF)
output_metric(TestoutDB, LOG.WEIGHT = F) ->  test.sol.out

#*********************************************

#Creat a wrapper function to do all above and calculate GoF given trade parameters
#logit.Armington.reg and logit.Armington.intl are 3, 6
#by changing those, we can minimize GoF
model_hindcast(PARAMETER = c(3,6),
               PARAMETER.EXPONENT = parameter.exponent,
               BASEYEAR = 1995,
               TARGETYEARS = seq(2000, 2015,5),
               BASEDATA.ALLYEARS = basedata.allyear)

#E.g., try other set of parameters 1.37 and 1.47; Gof will be smaller
model_hindcast(PARAMETER = c(1.38,1.47),
               PARAMETER.EXPONENT = parameter.exponent,
               BASEYEAR = 1995,
               TARGETYEARS = seq(2000, 2015,5),
               BASEDATA.ALLYEARS = basedata.allyear)

#*********************************************

#Another wrapper function to do the optimization
#E.g., initial value of 3, 6 are set for the two trade parameters
#HESSIAN matrix created can be used later to construct confidence intervals
#It takes ~8min to solve
run_hindcast_optim(PAR.START = c(3,6),
                   FUN = model_hindcast,
                   PARAMETER.EXPONENT = parameter.exponent,
                   BASEYEAR = 1995,
                   TARGETYEARS = seq(2000, 2015,5),
                   BASEDATA.ALLYEARS = basedata.allyear,
                   LOWER = c(0.1, 0.1),
                   UPPER = c(5, 10),
                   HESSIAN = T) -> sol.out

#*********************************************

#There are two alternative scenarios developed for the paper with generalized Armington modeling
#E.g., S2 and S3 in scripts
#But they are doing similar things, with more parameters (and more time-consuming) to optimize



