
#' model.proc.output
#'
#' @param BASEYEAR A numeric value of base year
#' @param TARGETYEAR A numeric value of target year
#' @param MODEL.DATA list of all data, returned from MODEL.DATA()
#' @param DB.OUTPUT Output database from model_solve
#' @param BASEDATA.ALLYEARS BASEDATA.ALLYEARS returned from dataproc.basedata
#'
#' @return Processed output database for a target year (updated.db.equil.yr)
#' @export

model_proc_output <- function(BASEYEAR,
                              TARGETYEAR,
                              MODEL.DATA,
                              DB.OUTPUT,
                              BASEDATA.ALLYEARS){

  # Silence package checks
  consumption <- crop <- est <- fn.value <- margin.reg.pim_pexp.mtax.shock <- obs <- pp <-
  price <- ref <- ref.verify <- reg <- reg.exp <- reg.imp <- scenario <- target.value <-
    value <- variable <- verify <- NULL

  #*********************************************************#
  #*Parsing data based on aHISTDATA
  MODEL.DATA[[as.character(BASEYEAR)]] -> DB.base
  MODEL.DATA[[as.character(TARGETYEAR)]] -> DB.target
  basedata_year(BASEYEAR, BASEDATA.ALLYEARS) -> DB.basedata


  #*********************************************************#
  #*Joining datasets
  DB.basedata$basedata.trade %>% filter(variable == "consume.dom") %>%
    dplyr::arrange(reg.exp, crop) %>%
    dplyr::bind_cols(data.frame(ref.verify = DB.base$variable.endogenous$consume.dom %>% unlist(),
                                fn.value = DB.OUTPUT$consume.dom %>% unlist(),
                                target.value = DB.target$variable.endogenous$consume.dom %>% unlist(),
                                row.names = NULL)) %>%
    dplyr::bind_rows(DB.basedata$basedata.trade %>% filter(variable == "export") %>%
                       dplyr::arrange(reg.exp, reg.imp, crop) %>%
                       dplyr::bind_cols(data.frame(ref.verify = DB.base$variable.endogenous$consume.imp.reg %>% unlist(),
                                                   fn.value = DB.OUTPUT$consume.imp.reg %>% unlist(),
                                                   target.value = DB.target$variable.endogenous$consume.imp.reg %>% unlist(),
                                                   row.names = NULL))
    ) %>%
    dplyr::transmute(reg.imp, reg.exp, crop, variable,
                     ref = value, verify = ref - ref.verify,  est = fn.value, obs = target.value)  %>%
    tidyr::gather("scenario", "consumption", c(ref, verify, est, obs)) ->
    updated.db.trade



  DB.basedata$basedata.regmkt %>% filter(variable == "pp") %>%
    dplyr::arrange(reg, crop) %>%
    dplyr::bind_cols(data.frame(ref.verify = DB.base$variable.endogenous$pp %>% unlist(),
                                fn.value = DB.OUTPUT$pp %>% unlist(),
                                target.value = DB.target$variable.endogenous$pp %>% unlist(),
                                row.names = NULL)) %>%
    dplyr:: rename(reg.imp = reg) %>%
    dplyr::mutate(reg.exp = reg.imp, variable = "consume.dom") %>%
    dplyr::bind_rows(
      DB.basedata$basedata.pricelink %>%
        dplyr::filter(variable %in% c("pp", "margin.reg.pim_pexp.mtax.shock")) %>%
        tidyr::spread(variable, value) %>%
        dplyr::mutate(variable = "export",
                      value = pp * margin.reg.pim_pexp.mtax.shock ) %>%
        dplyr::arrange(reg.exp, reg.imp, crop) %>%
        within(rm(pp, margin.reg.pim_pexp.mtax.shock)) %>%
        dplyr::bind_cols(data.frame(ref.verify = DB.base$variable.endogenous$pimp.reg %>% unlist(),
                                    fn.value = DB.OUTPUT$pimp.reg %>% unlist(),
                                    target.value = DB.target$variable.endogenous$pimp.reg %>% unlist(), row.names = NULL))
    ) %>%
    dplyr::transmute(reg.imp, reg.exp, crop, variable,
                     ref = value, verify = ref - ref.verify,  est = fn.value, obs = target.value) %>%
    tidyr::gather("scenario", "price", c(ref, verify, est, obs)) ->
    updated.db.price

  #*********************************************************#
  #*Check here to verify joining
  if (any(c(updated.db.trade %>% filter(scenario == "verify") %>% distinct(consumption) %>% unlist(),
            updated.db.price %>% filter(scenario == "verify") %>% distinct(price) %>% unlist())
          != 0) ) {
    stop("Inconsistent joining issues when converting results lists to data frame")
  }


  updated.db.trade %>%
    dplyr::left_join(updated.db.price, by = c("reg.imp", "reg.exp", "crop", "variable", "scenario")) %>%
    dplyr::mutate(target.yr = TARGETYEAR,
                  reg.exp = if_else(as.factor(variable) == "consume.dom",
                                    "Domestic",
                                    as.character(reg.exp))) ->
    updated.db.equil.yr

  #*********************************************************#
  return(updated.db.equil.yr)


  #*********************************************************#
  #*End
}
