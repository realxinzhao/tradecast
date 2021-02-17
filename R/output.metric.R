


#' output.metric
#'
#' @param aupdated.db.equil processed output database from output.prod (updated.db.equil)
#'
#' @return
#' @export

output.metric <- function(aupdated.db.equil){

  aupdated.db.equil %>% gather(equil,"value", c(consumption, price)) %>%
    filter(!(reg.imp == reg.exp & variable == "export")) %>%
    spread(scenario, value) %>%
    transmute(reg.imp, reg.exp, crop, variable, target.yr, equil, logdiff = (log(est / obs))^2) %>%
    spread(equil, logdiff) %>%
    filter(is.na(consumption) == F & is.infinite(consumption) == F
           #, is.finite(price)
    ) %>%
    mutate(logdist = (consumption + price)) %>%
    left_join(aupdated.db.equil %>% filter(scenario == "obs") %>%
                within(rm(scenario, price)) %>% rename(weight = consumption),
              by = c("reg.imp", "reg.exp", "crop", "variable", "target.yr")) %>%
    group_by(target.yr) %>%
    summarise(wmean.logdist.logw = weighted.mean(logdist, weight^0.5), #wmean.logdist.logw = weighted.mean(logdist, log(1+weight)),
              weight = sum(weight), .groups = "drop") %>%  ungroup() %>%
    summarise(Err = weighted.mean(wmean.logdist.logw, weight)) %>%
    pull(Err)
}

