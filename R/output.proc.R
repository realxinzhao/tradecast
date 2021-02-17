



#' model.proc.output
#'
#' @param base.yr A numeric value of base year
#' @param target.yr A numeric value of target year
#' @param aHISTDATA list of all data, returned from HISTDATA()
#' @param output updated.db.equil.yr
#' @param abasedata.allyear basedata.allyear returned from dataproc.basedata
#'
#' @return
#' @export
#'
#' @examples
model.proc.output <- function(base.yr = 1995,
                        target.yr = 1995,
                        aHISTDATA,
                        output,
                        abasedata.allyear){

  #*********************************************************#
  #*Parsing data based on aHISTDATA
  aHISTDATA[[as.character(base.yr)]] -> base
  aHISTDATA[[as.character(target.yr)]] -> target
  basedata.year(base.yr, abasedata.allyear) -> DB


  #*********************************************************#
  #*Joining datasets
  DB$basedata.trade %>% filter(variable == "consume.dom") %>%
    dplyr::arrange(reg.exp, crop) %>%
    dplyr::bind_cols(data.frame(ref.verify = base$variable.endogenous$consume.dom %>% unlist(),
                                fn.value = output$consume.dom %>% unlist(),
                                target.value = target$variable.endogenous$consume.dom %>% unlist(),
                                row.names = NULL)) %>%
    dplyr::bind_rows(DB$basedata.trade %>% filter(variable == "export") %>%
                       dplyr::arrange(reg.exp, reg.imp, crop) %>%
                       dplyr::bind_cols(data.frame(ref.verify = base$variable.endogenous$consume.imp.reg %>% unlist(),
                                                   fn.value = output$consume.imp.reg %>% unlist(),
                                                   target.value = target$variable.endogenous$consume.imp.reg %>% unlist(),
                                                   row.names = NULL))
    ) %>%
    dplyr::transmute(reg.imp, reg.exp, crop, variable,
                     ref = value, verify = ref - ref.verify,  est = fn.value, obs = target.value)  %>%
    tidyr::gather("scenario", "consumption", c(ref, verify, est, obs)) ->
    updated.db.trade



  DB$basedata.regmkt %>% filter(variable == "pp") %>%
    dplyr::arrange(reg, crop) %>%
    dplyr::bind_cols(data.frame(ref.verify = base$variable.endogenous$pp %>% unlist(),
                                fn.value = output$pp %>% unlist(),
                                target.value = target$variable.endogenous$pp %>% unlist(),
                                row.names = NULL)) %>%
    dplyr:: rename(reg.imp = reg) %>%
    dplyr::mutate(reg.exp = reg.imp, variable = "consume.dom") %>%
    dplyr::bind_rows(
      DB$basedata.pricelink %>%
        dplyr::filter(variable %in% c("pp", "margin.reg.pim_pexp.mtax.shock")) %>%
        tidyr::spread(variable, value) %>%
        dplyr::mutate(variable = "export",
                      value = pp * margin.reg.pim_pexp.mtax.shock ) %>%
        dplyr::arrange(reg.exp, reg.imp, crop) %>%
        within(rm(pp, margin.reg.pim_pexp.mtax.shock)) %>%
        dplyr::bind_cols(data.frame(ref.verify = base$variable.endogenous$pimp.reg %>% unlist(),
                                    fn.value = output$pimp.reg %>% unlist(),
                                    target.value = target$variable.endogenous$pimp.reg %>% unlist(), row.names = NULL))
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
    dplyr::mutate(target.yr = target.yr,
                  reg.exp = if_else(as.factor(variable) == "consume.dom",
                                    "Domestic",
                                    as.character(reg.exp))) ->
    updated.db.equil.yr

  #*********************************************************#
  return(updated.db.equil.yr)

}
