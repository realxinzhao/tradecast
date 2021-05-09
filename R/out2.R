
parameter.exponent <- list(
  ces.demand = 1,
  logit.landsupply = -1,
  logit.Armington.reg = 3,
  logit.Armington.intl = 6
)

basedata.allyear <- dataproc_basedata()

######################################

model_hindcast(PARAMETER = c(3, 6),
               PARAMETER.EXPONENT = parameter.exponent,
               BASEYEAR = 1995,
               TARGETYEARS = seq(2000, 2015,5),
               BASEDATA.ALLYEARS = basedata.allyear,
               LOG.WEIGHT = F,
               DB.OUTPUT = T) -> sol_out_S0_A

outDB_S0 <- sol_out_S0_A$outDB


load("output/hindcast/S1.Rdata")
sol.out -> sol_out_S1

model_hindcast(PARAMETER = sol_out_S1$sol$par,
               PARAMETER.EXPONENT = parameter.exponent,
               BASEYEAR = 1995,
               TARGETYEARS = seq(2000, 2015,5),
               BASEDATA.ALLYEARS = basedata.allyear,
               LOG.WEIGHT = F,
               DB.OUTPUT = T) -> sol_out_S1_A

outDB_S1 <- sol_out_S1_A$outDB


load("output/hindcast/S2.Rdata")
sol.out -> sol_out_S2
model_hindcast_S2(PARAMETER = sol_out_S2$sol$par,
                   PARAMETER.EXPONENT = parameter.exponent,
                   BASEYEAR = 1995,
                   TARGETYEARS = seq(2000, 2015,5),
                   BASEDATA.ALLYEARS = basedata.allyear,
                   LOG.WEIGHT = F,
                   DB.OUTPUT = T) -> sol_out_S2_A

outDB_S2 <- sol_out_S2_A$outDB


load("output/hindcast/S3.Rdata")
sol.out -> sol_out_S3
model_hindcast_S3(PARAMETER = sol_out_S3$sol$par,
                  PARAMETER.EXPONENT = parameter.exponent,
                  BASEYEAR = 1995,
                  TARGETYEARS = seq(2000, 2015,5),
                  BASEDATA.ALLYEARS = basedata.allyear,
                  LOG.WEIGHT = F,
                  DB.OUTPUT = T) -> sol_out_S3_A

outDB_S3 <- sol_out_S3_A$outDB

######################################

unique(outDB_S0$scenario)

outDB_S0 %>% filter(scenario == "est") %>% mutate(scenario = "S0") %>%
  bind_rows(
    outDB_S1 %>% filter(scenario == "est") %>% mutate(scenario = "S1")
  ) %>% bind_rows(
    outDB_S2 %>% filter(scenario == "est") %>% mutate(scenario = "S2")
  ) %>% bind_rows(
    outDB_S3 %>% filter(scenario == "est") %>% mutate(scenario = "S3")
  ) %>% bind_rows(
    outDB_S0 %>% filter(scenario %in% c("obs", "ref"))
  ) %>%
  mutate(scenario = if_else(scenario == "obs", "Obs.", scenario),
         scenario = if_else(scenario == "ref", "1995", scenario)) %>%
  mutate(expense = price * consumption) %>%
  tidyr::gather(equil,"value", c(consumption, price, expense)) %>%
  filter(!(reg.imp == reg.exp & variable == "export")) %>%
  mutate(reg.exp = if_else(reg.exp == "Domestic", reg.imp, reg.exp),
         ) ->
  data2

reg_agg <- c("Africa", "Asia", "Europe", "N. America", "S. America", "Oceania")
crop_agg <- c("Corn", "Wheat", "Rice", "Soybeans", "Rapeseed", "Others")
data2$reg.exp <-  factor(data2$reg.exp, levels = reg_agg)
data2$reg.imp <-  factor(data2$reg.imp, levels = reg_agg)
data2$crop <-  factor(data2$crop, levels = crop_agg)
data2$scenario <-  factor(data2$scenario , levels = c("1995", "Obs.", "S0",
                                                      "S1", "S2", "S3"))
outdir <- ("./output/paper/")
library(ggplot2)
Write_png <- function(.plot, name, w = 9000, h = 4500, r = 600){
  png(paste0(outdir,name,".png"), width = w, height = h, res = r)
  print(.plot)
  dev.off()
}
##########################################
#per year results

for (yr in seq(2000, 2015, 5)) {

  ggplot(data2 %>% filter(!scenario %in% c("1995", "S3"), equil == "consumption",
                          target.yr == yr) %>%
           mutate(value = value / 1000)) +
    geom_bar(aes(x = scenario, y = value, fill = reg.exp),
             color = "black", alpha = 0.85, stat="identity",position= "stack") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume (Million tons)") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    theme_bw() + theme0 + theme_leg -> scen.compare1

  Write_png(scen.compare1, paste0("good/result.stack.y", yr), h = 6000, w = 11000)
}


for (yr in seq(2000, 2015, 5)) {

  ggplot(data2 %>% filter(!scenario %in% c("1995", "S3"),
                          equil == "consumption", target.yr == yr) %>%
           mutate(value = value / 1000)) +
    geom_bar(aes(x = scenario, y = value, fill = reg.exp),
             color = "black", alpha = 0.85, stat="identity",position= "fill") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume share") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    theme_bw() + theme0 + theme_leg -> scen.compare1

  Write_png(scen.compare1, paste0("good/result.fill.y", yr), h = 6000, w = 11000)
}
#################################################


#per year results

for (yr in seq(2000, 2015, 5)) {

  ggplot(data2 %>% filter(equil == "consumption",
                          target.yr == yr) %>%
           mutate(value = value / 1000)) +
    geom_bar(aes(x = scenario, y = value, fill = reg.exp),
             color = "black", alpha = 0.85, stat="identity",position= "stack") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume (Million tons)") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    theme_bw() + theme0 + theme_leg -> scen.compare1

  Write_png(scen.compare1, paste0("good/SI.result.stack.y", yr), h = 7000, w = 13000)
}


for (yr in seq(2000, 2015, 5)) {

  ggplot(data2 %>% filter(equil == "consumption", target.yr == yr) %>%
           mutate(value = value / 1000)) +
    geom_bar(aes(x = scenario, y = value, fill = reg.exp),
             color = "black", alpha = 0.85, stat="identity",position= "fill") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume share") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    theme_bw() + theme0 + theme_leg -> scen.compare1

  Write_png(scen.compare1, paste0("good/SI.result.fill.y", yr), h = 7000, w = 13000)
}


##########################################
#per scenario results
for (ss in c("S0", "S1", "S2", "S3")) {

  ggplot(data2 %>% filter(scenario == ss, equil == "consumption") %>%
           mutate(value = value / 1000)) +
    geom_bar(aes(x = target.yr, y = value, fill = reg.exp),
             color = "black", alpha = 0.85, stat="identity",position= "stack") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume (Million tons)") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    theme_bw() + theme0 + theme_leg -> scen.stack

  Write_png(scen.stack, paste0("good/result.stack.", ss), h = 6000, w = 11000)
}


for (ss in c("S0", "S1", "S2", "S3")) {

  ggplot(data2 %>% filter(scenario == ss, equil == "consumption") %>%
           mutate(value = value / 1000)) +
    geom_bar(aes(x = target.yr, y = value, fill = reg.exp),
             color = "black", alpha = 0.85, stat="identity",position= "fill") +
    ggsci::scale_fill_npg(name = "Source") +
    labs(x = "Scenario", y = "Volume") +
    facet_grid(rows = vars(crop),
               cols = vars(reg.imp), scales = "free") +
    scale_y_continuous(breaks = c(0.25, 0.5, 0.75)) +
    theme_bw() + theme0 + theme_leg -> scen.fill

  Write_png(scen.fill, paste0("good/result.fill.", ss), h = 6000, w = 11000)
}


####################################################################

outDB_S0 %>% filter(scenario == "est") %>% mutate(scenario = "S0") %>%
  bind_rows(
    outDB_S1 %>% filter(scenario == "est") %>% mutate(scenario = "S1")
  ) %>% bind_rows(
    outDB_S2 %>% filter(scenario == "est") %>% mutate(scenario = "S2")
  ) %>% bind_rows(
    outDB_S3 %>% filter(scenario == "est") %>% mutate(scenario = "S3")
  ) %>% bind_rows(
    outDB_S0 %>% filter(scenario %in% c("obs", "ref"))
  ) %>%
  tidyr::gather(equil,"value", c(consumption, price)) %>%
  filter(!(reg.imp == reg.exp & variable == "export")) %>%
  mutate(reg.exp = if_else(reg.exp == "Domestic", reg.imp, reg.exp),
  ) %>%
  spread(scenario, value) %>% rename(year = target.yr) ->
  Tong

library(broom)

#### ANOVA

data = Tong %>% left_join(
  Tong %>%
    filter(equil == "consumption") %>%
    transmute(reg.imp, reg.exp, crop, variable, year, weight = obs) %>%
    group_by(year) %>%
    mutate(weight1 = weight ^ 0.5/ sum(weight^0.5) * sum(weight) )
) %>% ungroup()

mod = aov(value ~ crop + year + reg.imp + reg.exp,
          data = data %>% filter(equil =="consumption") %>%
            mutate(value = log(S1),
                   year = as.character(year)) %>%
            filter(is.finite(value) ==T, is.finite(log(obs)) == T),
          weight = weight^0.5
)
print(summary(mod))

vv = "price"
lapply(c("consumption", "price"), function(vv){

  aov(log(obs) ~  crop + year + interaction(reg.imp, reg.exp),
      data = data %>% gather(scenario, value, -names(.)[1:6]) %>%
        mutate(year = as.character(year)) %>%
        spread(equil, value) %>%
        filter(scenario %in% c("obs", "S2", "weight", "weight1")) %>%
        mutate(price = if_else(consumption == 0, 0, price)) %>%
        gather(equil, value, "consumption", "price") %>%
        spread(scenario, value) %>% filter(equil == vv) %>%
        filter(is.finite(log(obs)) == T,
               is.finite(log(obs/get("S2"))) == T), weight = weight1 ) %>%
    tidy() %>% mutate(weight = "sqroot", scenario = "none") %>%
    bind_rows(

      lapply(c("S0", "S1", "S2", "S3"), function(scen){
        aov(log(obs/get(scen)) ~  crop + year + interaction(reg.imp, reg.exp) ,
            data = data %>% gather(scenario, value, -names(.)[1:6]) %>%
              mutate(year = as.character(year)) %>%
              spread(equil, value) %>%
              mutate(price = if_else(consumption == 0, 0, price)) %>%
              gather(equil, value, "consumption", "price") %>%
              spread(scenario, value) %>% filter(equil == vv) %>%
              filter(is.finite(log(obs)) == T,
                     is.finite(log(obs/get("S2"))) == T), weight = weight1
        ) %>% tidy() %>% mutate(weight = "sqroot", scenario = scen)

      } ) %>% bind_rows()
    ) %>% mutate( variable = vv)
} ) %>% bind_rows() %>%
  bind_rows(

    aov(log(obs) ~  crop + year + interaction(reg.imp, reg.exp) + equil,
        data = data %>% gather(scenario, value, -names(.)[1:6]) %>%
          mutate(year = as.character(year)) %>%
          spread(equil, value) %>%
          filter(scenario %in% c("obs", "S2", "weight1")) %>%
          mutate(price = if_else(consumption == 0, 0, price)) %>%
          gather(equil, value, "consumption", "price") %>%
          spread(scenario, value) %>%
          filter(is.finite(log(obs)) == T,
                 is.finite(log(obs/get("S2"))) == T), weight = weight1 ) %>%
      tidy() %>% mutate(weight = "sqroot", scenario = "none") %>%
      bind_rows(

        lapply(c("S0", "S1", "S2", "S3"), function(scen){
          aov(log(obs/get(scen)) ~  crop + year + interaction(reg.imp, reg.exp) + equil,
              data = data %>% gather(scenario, value, -names(.)[1:6]) %>%
                mutate(year = as.character(year)) %>%
                spread(equil, value) %>%
                mutate(price = if_else(consumption == 0, 0, price)) %>%
                gather(equil, value, "consumption", "price") %>%
                spread(scenario, value) %>%
                filter(is.finite(log(obs)) == T,
                       is.finite(log(obs/get("S2"))) == T), weight = weight1
          ) %>% tidy() %>% mutate(weight = "sqroot", scenario = scen)

        } ) %>% bind_rows()
      ) %>% mutate( variable = "both")
  ) -> ANOVA

write.csv(ANOVA, paste0("output/paper/good/ANOVA.root.csv"))


####################################################################
#AE

data = Tong %>% left_join(
  Tong %>%
    filter(equil == "consumption") %>%
    transmute(reg.imp, reg.exp, crop, variable, year, weight = obs) %>%
    group_by(year) %>%
    mutate(weight1 = weight ^ 0.5/ sum(weight^0.5) * sum(weight) )
) %>% ungroup()

data %>%
  transmute(reg.imp, reg.exp, crop, variable, target.yr = year, equil, weight, AE = abs(S0 / obs -1 )) %>%
  filter(is.na(AE) == F & is.infinite(AE) == F) %>%
  group_by(target.yr) %>%
  summarise(AE = weighted.mean(AE, weight^.5),
            weight = sum(weight), .groups = "drop") %>%  ungroup() %>%
  summarise(Err = weighted.mean(AE, weight)) %>%
  pull(Err)

##AE calculaiton
Tong%>%
  gather(equil,"value", c(consumption, price)) %>%
  filter(!(reg.imp == reg.exp & variable == "export")) %>%
  mutate(reg.exp = if_else(reg.exp == "Domestic", reg.imp, reg.exp)) %>%
  rename(year = target.yr) %>%
  spread(scenario, value)%>% left_join(
    updated.db.equil%>%
      gather(equil,"value", c(consumption, price)) %>%
      filter(!(reg.imp == reg.exp & variable == "export")) %>%
      mutate(reg.exp = if_else(reg.exp == "Domestic", reg.imp, reg.exp)) %>%
      rename(year = target.yr) %>%
      spread(scenario, value) %>%
      filter(equil == "consumption") %>%
      transmute(reg.imp, reg.exp, crop, variable, year, weight = obs) %>%
      group_by(year) %>%
      mutate(weight1 = weight ^ 0.5/ sum(weight^0.5) * sum(weight) )
  ) %>% ungroup() %>%
  transmute(reg.imp, reg.exp, crop, variable, target.yr = year, equil, weight,weight1, AE = abs(est/obs  -1 )) %>%
  filter(is.na(AE) == F & is.infinite(AE) == F) %>%
  #group_by(equil) %>%
  summarise(AE = weighted.mean(AE, weight))



scen= "S2"
data %>%
  transmute(reg.imp, reg.exp, crop, variable, target.yr = year, equil, weight,weight1,
            AE =  (abs(get(scen)/obs  -1)*100)) %>%
  filter(is.na(AE) == F & is.infinite(AE) == F) %>%
  group_by(equil, crop, reg.imp) %>%
  summarise(AE0 = weighted.mean(AE, weight),
            AE25 = quantile(AE, probs = c(0.25)),
            AE50 = quantile(AE, probs = c(0.5)),
            AE75 = quantile(AE, probs = c(0.75)),
            weight = sum(weight)) %>%
  ungroup() %>%
  mutate(weight = weight / sum(weight) * 100) %>%
  mutate(scenario = scen) -> A
write.csv(A, "output/Paper/good/S2.dist.csv")

####################################################################


#------------------------------------------
fontfamily = "Arial"
windowsFonts(Arial=windowsFont("TT Arial"))
theme0 <- theme(
  #panel.grid.minor = element_line(size = 0.1, linetype = 2,colour = "grey30"),
  #panel.grid.major = element_line(size = 0.1, linetype = 2,colour = "grey30"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  #axis.ticks = element_line(linetype = 1,size = 0.5),
  #axis.ticks.length = unit(-0.1, "cm"),
  axis.text.y.right =  element_blank(),  axis.title.y.right = element_blank(),
  axis.text.x.top =  element_blank(),  axis.title.x.top = element_blank(),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
  plot.margin = margin(t = 10, r = 20, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)

theme_leg <- theme(legend.position="right", legend.justification = "center",
                   #legend.position=c(.1,0.7),
                   #legend.title = element_blank(),
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank())
