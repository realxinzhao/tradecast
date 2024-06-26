

#' output_metric
#' @description Computes goodness-of-fit based on the output database


#' @param DF processed output database from hindcast experiment
#' @param LOG.WEIGHT if true using log(1+weight) as weight; otherwise weight^0.5 is used as weight
#'
#' @return The value of targeted error metric
#' @import dplyr tidyr
#' @importFrom assertthat assert_that
#' @export

output_metric <- function(DF, LOG.WEIGHT = F){

  # Silence package checks
  Err <- consumption <- crop <- equil <- est <- logdiff <- logdist <- obs <- price <- reg.exp <-
  reg.imp <- scenario <- target.yr <- value <- variable <- weight <- wmean.logdist.logw <- NULL

  assertthat::assert_that(is.logical(LOG.WEIGHT))

  if (LOG.WEIGHT == F) {
    DF %>% gather(equil,"value", c(consumption, price)) %>%
      filter(!(reg.imp == reg.exp & variable == "export")) %>%
      spread(scenario, value) %>%
      transmute(reg.imp, reg.exp, crop, variable, target.yr, equil, logdiff = (log(est / obs))^2) %>%
      spread(equil, logdiff) %>%
      filter(is.na(consumption) == F & is.infinite(consumption) == F
             #, is.finite(price)
      ) %>%
      mutate(logdist = (consumption + price)) %>%
      left_join(DF %>% filter(scenario == "obs") %>%
                  within(rm(scenario, price)) %>% rename(weight = consumption),
                by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) %>%
      group_by(target.yr) %>%
      summarise(wmean.logdist.logw = weighted.mean(logdist, weight^0.5),
                weight = sum(weight), .groups = "drop") %>%  ungroup() %>%
      summarise(Err = weighted.mean(wmean.logdist.logw, weight)) %>%
      pull(Err)
  } else if (LOG.WEIGHT == T) {
    DF %>% gather(equil,"value", c(consumption, price)) %>%
      filter(!(reg.imp == reg.exp & variable == "export")) %>%
      spread(scenario, value) %>%
      transmute(reg.imp, reg.exp, crop, variable, target.yr, equil, logdiff = (log(est / obs))^2) %>%
      spread(equil, logdiff) %>%
      filter(is.na(consumption) == F & is.infinite(consumption) == F
             #, is.finite(price)
      ) %>%
      mutate(logdist = (consumption + price)) %>%
      left_join(DF %>% filter(scenario == "obs") %>%
                  within(rm(scenario, price)) %>% rename(weight = consumption),
                by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) %>%
      group_by(target.yr) %>%
      summarise(wmean.logdist.logw = weighted.mean(logdist, log(1+weight)),
                weight = sum(weight), .groups = "drop") %>%  ungroup() %>%
      summarise(Err = weighted.mean(wmean.logdist.logw, weight)) %>%
      pull(Err)
  }

}

