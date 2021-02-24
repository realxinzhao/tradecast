
#' BASEDATA
#' @description  processes source data to generate basedata
#' @import dplyr tidyr
#' @return a list of basedata.regmkt.allyear, basedata.trade.allyear, and basedata.pricelink.allyear
#' @export
#' @author Xin Zhao 2021


dataproc.basedata <- function(){

  TradeGCAM <- readRDS(paste0(system.file("extdata", package = "tradecast", mustWork = T),
                              "/RDS/TradeGCAMmini.rds"))
  prodGCAM <- readRDS(paste0(system.file("extdata", package = "tradecast", mustWork = T),
                             "/RDS/prodGCAMmini.rds"))
  basedata.nlc <- dataproc.gtap()[[1]]
  basedata.pricelink.margin.mtax <- dataproc.gtap()[[2]] #source basedata.nlc & basedata.pricelink.margin.mtax
  basedata.cropbioshare <- cropbioshare()

  #----------
  #Production equilibrium
  basedata.regmkt.allyear <-
    prodGCAM %>%
    dplyr::transmute(reg = region, crop = Item, year, prod, revenue, area = H.area/1000) %>%
    dplyr::group_by(reg, crop) %>%
    dplyr::mutate_at(vars(prod, revenue, area), ~MA.n(., periods = TIMESTEP)) %>%
    dplyr::mutate(yield = prod/1000/area, pp = revenue / prod) %>%
    dplyr::ungroup() %>%
    tidyr::gather("variable", "value", -c(reg, crop, year)) %>%
    dplyr::filter(year %in% SET$SET_YEAR) %>%
    dplyr::bind_rows(basedata.nlc) %>%
    dplyr::bind_rows(basedata.cropbioshare) %>%
    dplyr::mutate(value = if_else(
      crop == "Soybeans" & reg == "Oceania" &
        variable == "pp" & year == 2015, 500, value)) #Oceania soya price was missing $500 was used based on export prices

  unique(basedata.regmkt.allyear$variable)


  #----------
  #Trade flows and domestic supply; imp.Q from FAO data were used
  #Note that in TradeGCAM, reg.imp was reporting country for imp. data while reg.exp was reporting country for exp. data
  #Using TIMESTEP average data
  TradeGCAM.MA <- TradeGCAM %>% dplyr::select(reg.imp, reg.exp, crop=Item, year, imp.Q, imp.V) %>%
    dplyr::left_join(TradeGCAM %>%  #change reporting countires to join
                       dplyr::transmute(reg.imp0 = reg.exp, reg.exp = reg.imp,
                                        crop = Item, year,exp.Q, exp.V),
                     by = c("reg.imp" = "reg.imp0","reg.exp", "crop", "year")) %>%
    dplyr::group_by(reg.imp, reg.exp, crop) %>%
    dplyr::mutate_at(vars(imp.Q,imp.V,exp.Q,exp.V), ~MA.n(., periods = TIMESTEP)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year %in% SET$SET_YEAR)

  #Requiring balancing data to make sure consume.dom is non-negative
  #Only region of adjustment was Europe soybean 2005 data
  #Only imp.Q is used in this study!

  basedata.regmkt.allyear %>% filter(variable == "prod") %>%
    dplyr::transmute(reg.exp = reg, crop, year, prod = value) %>%
    dplyr::left_join(TradeGCAM.MA %>%
                dplyr::mutate(imp.Q = if_else(
                  crop == "Soybeans" & reg.imp == "Europe" &
                    reg.exp == "Europe" & year == 2005,
                  exp.Q, imp.Q)) %>%
                dplyr::group_by(reg.exp, crop , year) %>%
                dplyr::summarise_at(vars(imp.Q), sum) %>% ungroup(),
              by = c("reg.exp", "crop", "year")) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(consume.dom = prod - imp.Q) %>%
    dplyr::transmute(reg.imp = reg.exp, reg.exp, crop, year,
                     variable = "consume.dom", value = consume.dom / 1000) %>%
    dplyr::bind_rows(
      TradeGCAM.MA %>%
        dplyr::mutate(imp.Q = if_else(
          crop == "Soybeans" & reg.imp == "Europe" &
            reg.exp == "Europe" & year == 2005,
          exp.Q, imp.Q)) %>%
        dplyr::mutate(variable = "export") %>%
        dplyr::transmute(reg.exp, reg.imp, crop, year, variable, value = imp.Q / 1000)
    ) ->
    basedata.trade.allyear0

  #move intraregional trade to domestic consumption
  basedata.trade.allyear0 %>%
    dplyr::filter(reg.imp == reg.exp) %>%
    dplyr::mutate(variable = "consume.dom") %>%
    dplyr::group_by(reg.imp, reg.exp, crop, year, variable) %>%
    dplyr::summarise(value = sum(value), .groups = "drop") %>%
    dplyr::bind_rows(basedata.trade.allyear0 %>%
                       dplyr::filter(variable == "export") %>%
                       dplyr::mutate(value = if_else(reg.imp == reg.exp, 0, value))
    ) -> basedata.trade.allyear

  unique(basedata.trade.allyear$variable)
  #----------
  #Price links

  TradeGCAM.MA %>%
    dplyr::transmute(reg.exp, reg.imp, crop, year, imp.Q, exp.Q,
                     pimp.reg = if_else(is.na(imp.V/imp.Q), 0, imp.V/imp.Q*1000),
                     pexp.reg = if_else(is.na(exp.V/exp.Q), 0, exp.V/exp.Q*1000)) %>%
    dplyr::left_join(basedata.regmkt.allyear %>% filter(variable == "pp") %>%
                       spread(variable, value), by = c("reg.exp" = "reg", "crop", "year") ) %>%
    dplyr::mutate(pimp.reg = if_else(pimp.reg == 0 & pexp.reg == 0, pp * 1.12, pimp.reg),
                  pexp.reg = if_else(pexp.reg <= pimp.reg, pexp.reg, pp),
                  pexp.reg = if_else(pexp.reg < pimp.reg, pexp.reg, pimp.reg * 1.05),
                  pexp.reg = if_else(3 * pexp.reg > pimp.reg, pexp.reg, pimp.reg * 3)) %>%
    #filter(is.na(pimp.reg)) %>%  Note that the two margins are set to a lower bound of 1.05
    dplyr::mutate(margin.reg.pim_pexp = pimp.reg / pexp.reg ) %>%
    dplyr::mutate(margin.reg.pim_pexp = if_else(is.finite(margin.reg.pim_pexp) &
                                                  margin.reg.pim_pexp > 1, margin.reg.pim_pexp, 1.05)) %>%
    dplyr::mutate(pexp.reg = pimp.reg/margin.reg.pim_pexp) %>%
    dplyr::mutate(margin.reg.pim_pp = pimp.reg / pp) %>%
    dplyr::mutate(margin.reg.pim_pp = if_else(is.finite(margin.reg.pim_pp) &
                                                margin.reg.pim_pp > 1, margin.reg.pim_pp, 1.05)) %>%
    dplyr::left_join(basedata.pricelink.margin.mtax %>% spread(variable, value),
                     by = c("reg.exp", "reg.imp", "crop", "year")) %>%
    dplyr::mutate(margin.reg.pim_pexp.mtax.shock = margin.reg.pim_pexp * margin.mtax,
                  margin.reg.pim_pp.mtax.shock = margin.reg.pim_pp * margin.mtax) %>%
    tidyr::gather("variable","value", -c(reg.exp, reg.imp, crop, year)) ->
    basedata.pricelink.allyear

  (1:length(unique(basedata.regmkt.allyear$reg))) -> regID
  names(regID) <- unique(basedata.regmkt.allyear$reg)
  (1:length(unique(basedata.regmkt.allyear$crop))) -> sectorID
  names(sectorID) <- unique(basedata.regmkt.allyear$crop)
  as.character(unique(basedata.regmkt.allyear$variable)) -> vars

  baseyear <- unique(basedata.regmkt.allyear$year)

  return(list(
    basedata.regmkt.allyear = basedata.regmkt.allyear,
    basedata.trade.allyear = basedata.trade.allyear,
    basedata.pricelink.allyear = basedata.pricelink.allyear,
    baseyear = baseyear,
    regID = regID,
    sectorID = sectorID))

}










#' basedata.year
#'
#' @param ayear data of a numeric year
#' @param abasedata.allyear results from dataproc.basedata()
#'
#' @return A list of base data in a year: basedata.regmkt, basedata.trade, basedata.pricelink
#' @export

basedata.year <- function(ayear, abasedata.allyear){

  #*********************************************************#
  #Filter out data for a single year
  lapply(names(abasedata.allyear), function(dataset){
    if (grepl("^basedata.*allyear$", dataset)) {
      abasedata.allyear[[dataset]] %>%
      filter(year %in% c(ayear)) %>%
      within(rm(year))
    } else{
      abasedata.allyear[[dataset]]
    }
  }) -> abasedata.year

  names(abasedata.year) <- gsub(".allyear", "" , names(abasedata.allyear))
  abasedata.year[["baseyear"]] <- ayear

  return(abasedata.year)

}






