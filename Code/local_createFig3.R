
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compose figure 3
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I. Upper panel
#####################

### load & preprocess data
data_lines <- readRDS("Data/results/results_lk_20220720.rds") %>% filter(var %in% c("coal_exit", "onshore")) %>%
  select(ags, year, var, value, wirtsch_abs_gesamt,) %>% filter(!(var=="coal_exit" & year==2021))

startting_vals <- collap(data_lines[data_lines$year==2017,], value ~ ags + var) %>% dplyr::rename(start_val=value)
data_lines <- left_join(data_lines, startting_vals) %>% mutate(value_d = value - start_val)
data_lines$id <- paste0(as.character(data_lines$ags), as.character(data_lines$var))

# https://stackoverflow.com/questions/28984775/r-barplot-with-german-encoding
# \344 ä
# \366 ö
# \374 ü
# \337 ß

data_lines_2 <- readRDS("Data/results/results_lk_20220720.rds") %>% filter(var %in% c("solarplant","ice")) %>%
  select(ags, year, var, value, wirtsch_abs_gesamt,)

startting_vals_2 <- collap(data_lines_2[data_lines_2$year==2018,], value ~ ags + var) %>% dplyr::rename(start_val=value)
data_lines_2 <- left_join(data_lines_2, startting_vals_2) %>% mutate(value_d = value - start_val)
data_lines_2$id <- paste0(as.character(data_lines_2$ags), as.character(data_lines_2$var))

#data_lines[data_lines == "onshore"] <- "Expansion of onshore windplants"
#data_lines[data_lines == "coal_exit"] <- "Coal Exit"
#data_lines_2[data_lines_2 == "solarplant"] <- "Expansion of solar plants"
#data_lines_2[data_lines_2 == "ice"] <- "Phase out of fossil cars"

data_lines[data_lines == "onshore"] <- "Ausbau von Windenergieanlagen"
data_lines[data_lines == "coal_exit"] <- "Kohleausstieg"
data_lines_2[data_lines_2 == "solarplant"] <- "Ausbau von Solarfreifl\344chenanlagen"
data_lines_2[data_lines_2 == "ice"] <- "Zulassungsstop Verbrenner-PKW"


data_lines <- full_join(data_lines, data_lines_2)



# plot
scatterplot <- ggplot(data_lines)+ 
  geom_point(aes(x=year, y=value_d, color=var))+
  scale_color_manual(values=c('#003f5c','#ffa600', '#bc5090', '#73ad70'))+
  geom_line(aes(x=year, y=value_d, group=id, color=var), alpha=0.1)+
 # labs(title = "Temporal change in support for climate change mitigation policies")+xlab("Year")+ylab("change in support compared to 2017")+
  labs(title = "Zeitliche Entwicklung lokaler Unterst\374tzung für Klimaschutzmaßnahmen")+xlab("Jahr")+ylab("Relative Ver\344nderung gegen\374ber 2017")+
  theme_clean()+theme(legend.position="bottom", axis.title = element_text(size = 10))

#theme_clean()

# II. Lower panel
#####################

### load & preprocess data
data <- readRDS("Data/results/results_lk_20220720.rds") %>% filter(var %in% c("coal_exit", "solarplant", "onshore", "ice")) %>%
  select(ags, year, var, value) %>%
  pivot_wider(id_cols = ags, names_from = c(var, year), values_from = value) %>%
  mutate(coal_exit_delta = coal_exit_2019-coal_exit_2017) %>% # # calculate delta support
  mutate(solarplant_delta = solarplant_2021-solarplant_2018) %>%
  mutate(onshore_delta = onshore_2021-onshore_2017) %>%
  mutate(ice_delta = ice_2021-ice_2018)



# Prepare data for spatial join
data <- data %>% dplyr::rename(CC_2=ags)
data$CC_2 <- as.character(data$CC_2)
data[data$CC_2=="03159", "CC_2"]<-"03152" # correct lk_kz for Göttingen
data <- rbind(data, data %>% filter(CC_2=="03152") %>%mutate(CC_2="03156")) # Add record for Osterrode which was formerly merged with Göttigngen

# Load map and conduct spatial join
map_root <- st_read("Data/shapefiles/gadm40_DEU_2.shp") # get spatial data for Germany at the county level
map_root <- st_transform(map_root, crs = 'ESRI:31494')
map <- left_join(map_root, data)

### display in maps figure
map_d_coal <- ggplot(data = map) +
      geom_sf(aes(fill = coal_exit_delta), lwd=0) +   #labs(subtitle = paste0(ls_subtitles[i], ", in ", ls_years[i])) +
      scale_fill_gradientn(colors =c("#cbe8f5", "#3d7791", "#012536"),#colors = c("#de425b","#e6a37a", "#f1ebda", "#9db78f", "#488f31"), #values = scales::rescale(c(-0.05, 0, 0.2, 0.4)),
                            na.value = "grey80",# limits=c(-0.4,0.4),
                           labels = scales::percent_format(accuracy = 1), name = "")+
       theme_void()+ theme(    #legend.justification = c("left", "top"),
    legend.position ="bottom",plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))
    

#text = element_text(family = "Tahoma",  size = 8), plot.subtitle = element_text(family = "Tahoma", size = 8),legend.title = element_text(family = "Tahoma", size = 12),
    #legend.text = element_text(family = "Tahoma", size = 3),
    #legend.key.size = unit(0.5, 'cm'))



map_d_sol <- ggplot(data = map) +
      geom_sf(aes(fill = solarplant_delta), lwd=0) +   #labs(subtitle = paste0(ls_subtitles[i], ", in ", ls_years[i])) +
      scale_fill_gradientn(colors = c( "#f2dce9","#bc5090", "#750447"), na.value = "grey80", 
                           labels = scales::percent_format(accuracy = 1), name = "")   + theme_void() + theme(    #legend.justification = c("left", "top"),
          legend.position ="bottom", plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))
  

map_d_ice <- ggplot(data = map) +
  geom_sf(aes(fill = ice_delta), lwd=0) +   #labs(subtitle = paste0(ls_subtitles[i], ", in ", ls_years[i])) +
  scale_fill_gradientn(colors = c( "#d9e8d8" ,"#73ad70", "#2b4829"), na.value = "grey80", 
                       labels = scales::percent_format(accuracy = 1), name = "")   + theme_void() + theme(    #legend.justification = c("left", "top"),
                         legend.position ="bottom", plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))


map_d_onshore <- ggplot(data = map) +
  geom_sf(aes(fill = onshore_delta), lwd=0) +   #labs(subtitle = paste0(ls_subtitles[i], ", in ", ls_years[i])) +
  scale_fill_gradientn(colors = c("#fae3b9", "#ffa600", "#664201"  ), #colors = c("#e6a37a", "#f1ebda", "#9db78f", "#488f31"),
                       na.value = "grey80",             # limits=c(-0.4,0.4), values = scales::rescale(c(-0.05, 0, 0.2, 0.4)),
                       labels = scales::percent_format(accuracy = 1), name = "") +
  theme_void() + theme(    #legend.justification = c("left", "top"),
    legend.position ="bottom", plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))
#text = element_text(family = "Tahoma",  size = 8), plot.subtitle = element_text(family = "Tahoma", size = 8),legend.title = element_text(family = "Tahoma", size = 12),
#legend.text = element_text(family = "Tahoma", size = 3),
#legend.key.size = unit(0.5, 'cm'))


change_maps <- ggarrange( map_d_coal, map_d_onshore , map_d_sol, map_d_ice,
                   labels = c("b", "c","d", "e" ),#"d", "", "", ""),
                   ncol = 4, nrow = 1, common.legend = FALSE, legend = "bottom")

### display in scatterplot figure

plots <- ggarrange(scatterplot, change_maps, nrow = 2, ncol=1, heights = c(2,1),  labels = c("a", ""))
plots
ggsave( "Figures/Fig 3 - change support_new2_2_DE.png", width = 2300, height = 2000 , units = "px")


#############
# Descriptive

mean(pull(data, "ee_expansion_2017"), na.rm=TRUE)
mean(pull(data, "ee_expansion_2021"), na.rm=TRUE)
sd(pull(data, "ee_expansion_2017"), na.rm=TRUE)
sd(pull(data, "ee_expansion_2021"), na.rm=TRUE)

mean(pull(data, "onshore_2017"), na.rm=TRUE)
mean(pull(data, "onshore_2021"), na.rm=TRUE)
sd(pull(data, "onshore_2017"), na.rm=TRUE)
sd(pull(data, "onshore_2021"), na.rm=TRUE)

mean(pull(data, "solarplant_2018"), na.rm=TRUE)
mean(pull(data, "solarplant_2021"), na.rm=TRUE)
sd(pull(data, "solarplant_2018"), na.rm=TRUE)
sd(pull(data, "solarplant_2021"), na.rm=TRUE)

mean(pull(data, "ice_2018"), na.rm=TRUE)
mean(pull(data, "ice_2019"), na.rm=TRUE)
mean(pull(data, "ice_2021"), na.rm=TRUE)
sd(pull(data, "ice_2018"), na.rm=TRUE)
sd(pull(data, "ice_2021"), na.rm=TRUE)

max(pull(data_lines[data_lines$year==2018 & data_lines$var=="onshore", ], "value_d"))
max(pull(data_lines[data_lines$year==2019 & data_lines$var=="onshore", ], "value_d"))
max(pull(data_lines[data_lines$year==2021 & data_lines$var=="onshore", ], "value_d"))
View(data_lines[data_lines$year==2018 & data_lines$var=="onshore", ])
