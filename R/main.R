

#Initialize



Run_model <- function(abase.yr, atarget.yr, aaHISTDATA, aabasedata.allyear){

  #*********************************************************#
  #*config and run

  model.shock.config <- model.shock.config(base.yr = abase.yr,
                                           target.yr = atarget.yr,
                                           aHISTDATA = aaHISTDATA)
  model.solve(config = model.shock.config) -> model.sol
  dslnex_result(x = model.sol$x) -> outDB

  #*********************************************************#
  model.proc.output(base.yr = abase.yr,
                    target.yr = atarget.yr,
                    aHISTDATA = aaHISTDATA,
                    output = outDB,
                    abasedata.allyear = aabasedata.allyear) -> updated.db.equil.yr


  #*********************************************************#
  return(list(
    model.sol = model.sol,
    outDB = outDB,
    updated.db.equil.yr = updated.db.equil.yr
  ))

}

#*********************************************************#
#*Parse data
all_data <- HISTDATA_ALL(allyear = SET$SET_YEAR)
basedata.allyear <- dataproc.basedata()

Run_model(CALIBRATION_YEAR, 2015, all_data, basedata.allyear) -> model.out


target.yr.all = setdiff(SET$SET_YEAR,CALIBRATION_YEAR)

Run_model_allyear <- function(abase.yr, target.yr.all, aHISTDATA, abasedata.allyear){

  #*********************************************************#
  #
  lapply(target.yr.all, function(aatarget.yr){

    Run_model(abase.yr = abase.yr, atarget.yr = aatarget.yr,
              aaHISTDATA = aHISTDATA, aabasedata.allyear = abasedata.allyear) -> model.out
    return(model.out$updated.db.equil.yr)

  }) %>% bind_rows() -> updated.db.equil

  return(updated.db.equil)

}



Run_model_allyear(CALIBRATION_YEAR, setdiff(SET$SET_YEAR,CALIBRATION_YEAR),
                  all_data, basedata.allyear) -> updated.db.equil

output.metric(updated.db.equil)


