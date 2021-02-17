#This file processes OECD_FAO data to get biofuel feedstock info


#' dataproc.OECD_FAO
#' @description  processes OECD_FAO raw data to get historical biofuel feedstock data for tradecast
#' @param oecddatadir path to data folder (default = "./inst/extdata/OECD_FAO/")
#' @import dplyr tidyr
#' @importFrom utils read.csv
#' @importFrom stats weighted.mean
#' @return the share of crops used for biofuels
#' @export
#' @author Xin Zhao 2021
#'
cropbioshare <- function(oecddatadir = paste0(system.file("extdata", package = "tradecast", mustWork = T), "/OECD_FAO/")
                         ){


files <- list.files(path = oecddatadir, pattern = ".csv$", ignore.case = T)


read.csv(paste0(oecddatadir, files), header = T, skip = 1) -> OECDdata
unique(OECDdata$Country)
unique(OECDdata$Commodity)
unique(OECDdata$Variable)

reg0 <- c("Mexico", "NORTH AMERICA", "LATIN AMERICA", "AFRICA", "EUROPE","OCEANIA", "ASIA")
reg1 <- c("N. America","N. America","S. America", "Africa","Europe","Oceania","Asia")

OECDdata.needed <- OECDdata %>%
  transmute(year = Time, reg = Country, crop = Commodity, variable = Variable, value = Value) %>%
  filter(reg %in% reg0,
         crop %in% c("Vegetable oils", "Other oilseeds", "Soybean", "Wheat",  "Maize", "Rice", "Other coarse grains",
                     "Sugar beet", "Sugar cane", "ROOTS AND TUBERS", "PULSES", "COTTON"),
         variable %in% c("Consumption", "Production", "Exports", "Imports", "Biofuel use", "Other use", "Crush"))
#note that consumption does not include intermediate consumption e.g., suger crop consumption is 0
#so consumption will be calculated as the balance

#clean region
OECDdata.needed %>% filter(reg != "LATIN AMERICA") %>%
  left_join(data.frame(reg = reg0,
                       region = reg1), by = "reg") %>%
  group_by(reg = region, crop, year, variable) %>%
  summarise(value = sum(value), .groups = "drop") %>% bind_rows(
    OECDdata.needed %>% filter(reg %in% c("LATIN AMERICA", "Mexico")) %>%
      arrange(reg) %>%
      group_by(year, crop, variable) %>%
      mutate(value = value - last(value)) %>%
      filter(reg == "LATIN AMERICA") %>%
      mutate(reg = "S. America")
  ) -> OECDdata.reg

starch0 <- OECDdata.reg %>%
  filter(!crop %in% c("Other oilseeds", "Soybean")) %>%
  mutate(crop1 = if_else(crop %in% c("Wheat", "Maize", "Rice", "Vegetable oils"), crop,"others")) %>%
  group_by(reg, crop = crop1, variable, year) %>%
  summarise(value = sum(value), .groups = "drop")

starch <- starch0 %>% filter(crop != "Vegetable oils") %>%
  spread(variable, value) %>%
  mutate(crop.consumption = Production + Imports - Exports,
    bioshare = `Biofuel use`/crop.consumption *100)

oil0 <- OECDdata.reg %>%
  filter(crop %in% c("Other oilseeds", "Soybean")) %>%
  group_by(reg, variable, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  spread(variable, value) %>%
  transmute(reg, year, Crush, crop.consumption = Production + Imports - Exports) %>%
  left_join(
      starch0 %>% filter(crop == "Vegetable oils") %>%
        spread(variable, value), by = c("reg", "year") ) %>%
  mutate(crush.rate = Production / Crush,
         bioshare.oil = `Biofuel use`/Production *100)

oil <- OECDdata.reg %>%
  filter(crop %in% c("Other oilseeds", "Soybean")) %>%
  spread(variable, value) %>%
  transmute(reg, crop, year, Crush, crop.consumption = Production + Imports - Exports) %>%
  left_join(oil0 %>% dplyr::select(reg, year,bioshare.oil), by = c("reg", "year")) %>%
  mutate(bioshare.seed = bioshare.oil *  Crush /crop.consumption)


crop_agg   <- c("Corn", "Wheat", "Rice", "Soybeans", "Rapeseed", "Others")
crop <- c("Maize", "Wheat", "Rice", "Soybeans", "Other oilseeds", "others")

basedata.cropbioshare <- starch %>% dplyr::select(reg, crop, year, crop.consumption, bioshare) %>%
  bind_rows(oil %>% dplyr::select(reg, crop, year, crop.consumption, bioshare = bioshare.seed)) %>%
  left_join(data.frame(crop = c("Maize", "Wheat", "Rice", "Soybean", "Other oilseeds", "others"),
                       crop1 = c("Corn", "Wheat", "Rice", "Soybeans", "Others", "Others") ), by = "crop") %>%
  group_by(reg, crop = crop1, year) %>%
  summarise(bioshare = weighted.mean(bioshare, crop.consumption),
            crop.consumption = sum(crop.consumption), .groups = "drop") %>%
  ungroup() %>%
  bind_rows(oil %>% filter(crop == "Other oilseeds") %>%
              dplyr::select(reg, crop, year, crop.consumption, bioshare = bioshare.seed)) %>%
  mutate(biocrop = crop.consumption * bioshare) %>%
  mutate_at(vars(crop.consumption, biocrop), ~MA.n(., periods = TIMESTEP)) %>%
  mutate(bioshare = biocrop / crop.consumption) %>%
  gather(variable, value, -c(reg, crop, year)) %>%
  mutate(crop = replace(crop, crop == "Other oilseeds", "Rapeseed")) %>%
  filter(year %in% SET$SET_YEAR) %>% filter(variable == "bioshare")

  return(basedata.cropbioshare)

}


#ggplot(basedata.cropbioshare) +
#  geom_line(aes(x = year, y = value, color = reg)) + facet_wrap(~crop)








