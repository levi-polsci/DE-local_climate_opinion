###### Create figure with four variables over four points of time

# -- obtain list of all files in the folder
filenames_in <- list.files("Data/results/output")
# -- select variables
filenames_in <- filenames_in[c(5:8, # coal exit
                               10:13, # builduing levy 2021
                               36:39)]#, # coal exit 2019
                               #52:55)] # solar

# -- obtain list of variables, years, & German and English subtitles legend titles
ls_vars <- sub("*fittedModel_y.","", sub("-.*", "",filenames_in))
ls_years <- sub(".*-","", sub(".rds.*", "",filenames_in))
ttl_map <- read.xlsx("Data/title_data.xlsx")

ls_subtitles_EN <- unlist(lapply(ls_vars, function (x) ttl_map[ttl_map$var==x,"subtitle_EN"]))
ls_legendtlt_EN <- unlist(lapply(ls_vars, function (x) ttl_map[ttl_map$var==x,"legend_EN"]))
ls_subtitles_EN <- gsub("xxx", "\n", ls_subtitles_EN)


# -- prepare map object
map_root <- st_read("Data/shapefiles/gadm40_DEU_2.shp") # get spatial data for Germany at the county level
map_root <- st_transform(map_root, crs = 'ESRI:31494')
obj_lst <- list()

plot_data <- function (i){

  load(paste0("Data/results/output/",filenames_in[i]))
  #data <- readRDS(paste0("Data/results/",filenames_in[i])) # -- load data
  data <- ml_model

  predictions <- data$ebma %>% dplyr::rename(CC_2=lk_kz, var=ebma) # -- join map with data
  predictions$CC_2 <- as.character(predictions$CC_2)
  predictions[predictions$CC_2=="03159", "CC_2"]<-"03152" # correct lk_kz for Göttingen
  predictions <- rbind(predictions, predictions %>% filter(CC_2=="03152") %>%mutate(CC_2="03156")) # Add record for Osterrode which was formerly merged with Göttigngen
  map <- left_join(map_root, predictions)

  print(filenames_in[i])

  ggplot(data = map) +
      geom_sf(aes(fill = var), lwd=0) +   #labs(subtitle = paste0(ls_subtitles[i], ", in ", ls_years[i])) +
      scale_fill_gradientn(colors = c("#00006c","#8e7ea0", "#e3cb42", "#c36c17", "#8b0000"), na.value = "grey80",
                           limits=c(0,1), labels = scales::percent, name = "Support")+
      theme_void()+ theme(    #legend.justification = c("left", "top"),
    legend.position ="bottom",plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"),
    text = element_text(family = "Tahoma",  size = 8), plot.subtitle = element_text(family = "Tahoma", size = 8),legend.title = element_text(family = "Tahoma", size = 12),
    legend.text = element_text(family = "Tahoma", size = 8),
    legend.key.size = unit(1, 'cm'))
}
myplots <- lapply(seq_along(filenames_in), plot_data)

plots <- ggarrange(myplots[[1]], myplots[[2]],myplots[[3]], myplots[[4]],
                   myplots[[5]], myplots[[6]],myplots[[7]], myplots[[8]],
                   myplots[[9]], myplots[[10]],myplots[[11]], myplots[[12]],
                   #myplots[[13]], myplots[[14]],myplots[[15]], myplots[[16]],
          labels = c("a", "", "", "", "b", "", "", "","c", "", "", ""),#"d", "", "", ""),
          ncol = 4, nrow = 3, common.legend = TRUE, legend = "bottom")
plots
ggsave("Figures/Fig 2 - change over time.png")
