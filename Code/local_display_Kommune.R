
# Kommune Daten einlesen
load("Data/results/output/fittedModel_y.coal_exit- kommune.rds")


# Shapefile einlesen
map_root <- st_read("Data/shapefiles/VG250_GEM.shp") # get spatial data for Germany at the county level
map_root <- st_transform(map_root, crs = 'ESRI:31494')
map_root <- map_root%>% dplyr::rename(gkz_f=AGS,)

predictions <- ml_model$ebma %>% dplyr::rename(gkz_f=CC_2) # -- join map with data
#predictions <- rename(predictions, gkz_f=CC_2)

map <- left_join(map_root, predictions, by="gkz_f")
map <- na.omit(map)

# Darstellen

ggplot(data=map) + geom_sf(aes(fill = ebma), lwd=0) + scale_fill_gradientn(colors = c("#00006c","#8e7ea0", "#e3cb42", "#c36c17", "#8b0000"), na.value = "grey80",
                                                                              limits=c(0,1), labels = scales::percent, name = "Support")+
  theme_void()+ theme(    #legend.justification = c("left", "top"),
    legend.position ="bottom",plot.margin = unit(c(0, 0, 0, 0), "cm"),
    text = element_text(family = "Tahoma",  size = 10), plot.subtitle = element_text(family = "Tahoma", size = 10),legend.title = element_text(family = "Tahoma", size = 10),
    legend.text = element_text(family = "Tahoma", size = 10),
    legend.key.size = unit(1, 'cm'))

