
#' dataproc.gtap
#'
#' @param gtapdatadir path to gtap data dir
#'
#' @description  processes gtap raw data to get margin and tariff data for tradecast
#' @return Returns a dataframe (basedata.pricelink.margin.mtax) including margin and
#'         tariff, and also nonland cost  share.
#' @import dplyr tidyr
#' @export
#' @author Xin Zhao 2021
#'
dataproc.gtap <- function(gtapdatadir = paste0(system.file("extdata", package = "tradecast", mustWork = T), "/gtap/")
                          ){

files <- list.files(path = gtapdatadir, pattern = ".csv$", ignore.case = T)
gtap_bind <- function(gtapheader = "BI02"){
  lapply(files,function(file){
    utils::read.csv(paste0(gtapdatadir, file),header = F,
             fill = T, col.names = c(1:6), na.strings = NA) %>%
      tibble::rowid_to_column("ID")  %>%
      dplyr::filter(grepl(pattern = "!Header:", X1)) %>%
      dplyr::transmute(ID, header = gsub("!Header:","", X1)) %>%
      dplyr::right_join(
        utils::read.csv(paste0(gtapdatadir, file),header = F,
                 fill = T, col.names = c(1:6), na.strings = NA) %>%
          tibble::rowid_to_column("ID"), by = "ID"
      ) %>% arrange(ID) %>%
      tidyr::fill(header) %>%
      dplyr::filter(grepl(pattern = gtapheader, header),
                    grepl(pattern = "!Header:", X1) == F) %>%
      dplyr::mutate_all(funs(na_if(., ""))) %>%
      dplyr::select_if(function(col) !all(is.na(col))) %>%
      within(rm(ID)) %>%
      dplyr::mutate(year = gsub("Gtp6x6_Y|.csv", "", file, ignore.case = T))
  } ) %>% dplyr::bind_rows()
}

#-----------------------------
#nls share
gtap_bind("SF01") %>%
  dplyr::mutate_if(is.factor, as.character)%>%
  stats::setNames(c("header", as.character(.[1, ])[2:(ncol(.)-1)], "year")) %>%
  dplyr::filter(Value != "Value") -> gtap.SF01

#unique(gtap.SF01$year)
study.year <- c(1995, 2000, 2005, 2010, 2015)
gtap.year  <- c(1995, 2001, 2004, 2011, 2011)

#unique(gtap.SF01$PROD_COMM)
crop_agg   <- c("Corn", "Wheat", "Rice", "Soybeans", "Rapeseed", "Others")
gtap.crop  <- c("gro", "wht", "pdr", "osd", "osd", "others")

region <- c("Africa",    "Asia",      "Europe",
            "N. America", "Oceania",   "S. America")
gtap.reg <- setdiff(unique(gtap.SF01$REG), "ROW")


gtap.SF01 %>%
  dplyr::filter(PROD_COMM %in% gtap.crop,
         REG != "ROW",
         year %in% gtap.year) %>%
  dplyr::group_by_at(vars(names(.)[2:4], year)) %>%
  dplyr::summarise(value = sum(as.numeric(Value)),.groups = 'drop') %>%
  dplyr::ungroup() %>%
  dplyr::group_by_at(vars(REG, PROD_COMM, year)) %>%
  dplyr::mutate(value = value / sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(DEMD_COMM == "Land") %>%
  dplyr::transmute(reg = REG, gtap.crop = PROD_COMM, gtap.year = as.numeric(year),
            variable = "nlc.share", value = 1 - value) ->
  gtap.nlc.share

expand.grid(region = region,
            crop_agg = crop_agg, study.year = study.year) %>%
  dplyr::left_join(data.frame(study.year, gtap.year), by = "study.year") %>%
  dplyr::left_join(data.frame(crop_agg, gtap.crop), by = "crop_agg") %>%
  dplyr::left_join(data.frame(region, gtap.reg), by = "region") %>%
  dplyr::left_join(gtap.nlc.share, by = c("gtap.year", "gtap.crop")) %>%
  dplyr::transmute(reg = region, crop = crop_agg, year = study.year,
                   variable, value) ->
  basedata.nlc

#-----------------------------
#tariffs

gtap_bind("BI02") %>%
  stats::setNames(c("header", "TRAD_COMM", "reg.exp", "reg.imp", "IMPVALUE", "Value","year")) %>%
  dplyr::filter(Value != "Value") -> gtap.BI02

gtap.BI02 %>%
  dplyr::filter(TRAD_COMM %in% gtap.crop,
         reg.exp != "ROW",
         reg.imp != "ROW",
         year %in% gtap.year) %>%
  dplyr::transmute(gtap.crop = TRAD_COMM,
            gtap.reg.exp = reg.exp,
            gtap.reg.imp = reg.imp,
            gtap.year = as.numeric(year),
            IMPVALUE, value = as.numeric(Value)) %>%
  tidyr::spread(IMPVALUE, value) %>%
  dplyr::mutate(value = if_else(is.finite((impcost + mtax) / impcost),
                               (impcost + mtax) / impcost, 1) ) ->
  gtap.margin.mtax


expand.grid(reg.exp = region, reg.imp = region,
            crop_agg = crop_agg, study.year = study.year) %>%
  dplyr::left_join(data.frame(study.year, gtap.year), by = "study.year") %>%
  dplyr::left_join(data.frame(crop_agg, gtap.crop), by = "crop_agg") %>%
  dplyr::left_join(data.frame(reg.exp = region, gtap.reg.exp = gtap.reg), by = "reg.exp") %>%
  dplyr::left_join(data.frame(reg.imp = region, gtap.reg.imp = gtap.reg), by = "reg.imp") %>%
  dplyr::left_join(gtap.margin.mtax, by = c("gtap.year", "gtap.crop", "gtap.reg.exp", "gtap.reg.imp")) %>%
  dplyr::transmute(reg.exp, reg.imp, crop = crop_agg, year = study.year,
            variable = "margin.mtax",
            value = if_else(is.finite(value), value, 1)) ->
  basedata.pricelink.margin.mtax

rm(list = ls(pattern = "gtap*"))
#-----------------------------

return(list(basedata.nlc, basedata.pricelink.margin.mtax))
}







