# constants.R

# **************************************************************************************************************
CALIBRATION_YEAR = 1995
TIMESTEP = 5
SET <- list(
    SET_YEAR = c(seq(1995, 2015, TIMESTEP)),
    SET_CROP = c("Corn", "Wheat", "Rice", "Soybeans", "Rapeseed", "Others"),
    SET_REG  = c("Africa", "Asia", "Europe", "N. America", "Oceania", "S. America") )


parameter.exponent <- list(
    ces.demand = 1,
    logit.landsupply = -1,
    logit.Armington.reg = 3,
    logit.Armington.intl = 6
)
