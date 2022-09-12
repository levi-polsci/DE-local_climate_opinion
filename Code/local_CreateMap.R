#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Loop create maps
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# -- obtain list of all files in the folder

filenames_in <- list.files("Data/results/output")
filenames_in <- filenames_in[c(32, 33, 34, 63,64, 65)]

  # -- obtain list of variables, years, & German and English subtitles legend titles
  ls_vars <- sub("*fittedModel_y.","", sub("-.*", "",filenames_in))
  ls_years <- sub(".*-","", sub(".rds.*", "",filenames_in))

  ttl_map <- read.xlsx("Data/title_data.xlsx")

  ls_subtitles_EN <- unlist(lapply(ls_vars, function (x) ttl_map[ttl_map$var==x,"subtitle_EN"]))
  ls_subtitles_DE <- unlist(lapply(ls_vars, function (x) ttl_map[ttl_map$var==x,"subtitle_DE"]))
  ls_legendtlt_EN <- unlist(lapply(ls_vars, function (x) ttl_map[ttl_map$var==x,"legend_EN"]))
  ls_legendtlt_DE <- unlist(lapply(ls_vars, function (x) ttl_map[ttl_map$var==x,"legend_DE"]))
  ls_subtitles_DE <- gsub("xxx", "\n", ls_subtitles_DE)
  ls_subtitles_EN <- gsub("xxx", "\n", ls_subtitles_EN)

  # -- prepare map object

  map_root <- st_read("Data/shapefiles/gadm40_DEU_2.shp") # get spatial data for Germany at the county level
  map_root <- st_transform(map_root, crs = 'ESRI:31494')

  for(i in seq_along(filenames_in)){

    load(paste0("Data/results/output/",filenames_in[i]))
    #data <- readRDS(paste0("Data/results/",filenames_in[i])) # -- load data
    data <- ml_model

    # -- join map with data

    predictions <- data$ebma %>% dplyr::rename(CC_2=lk_kz, var=ebma)
    predictions$CC_2 <- as.character(predictions$CC_2)
    predictions[predictions$CC_2=="03159", "CC_2"]<-"03152" # correct lk_kz for Göttingen
    predictions <- rbind(predictions, predictions %>% filter(CC_2=="03152") %>%mutate(CC_2="03156")) # Add record for Osterrode which was formerly merged with Göttigngen
    map <- left_join(map_root, predictions)

    for (language in c("DE", "EN")){

      # -- prepare titles and legend

       if(language=="DE"){
         title <- "Lokale Klimaschutzeinstellungen in Deutschland"
         source <- "Quelle: Levi et al. (unveröffentlicht)"
         ls_subtitles <- ls_subtitles_DE
         ls_legendtlt <- ls_legendtlt_DE
         location <- "German"
       }
      if(language=="EN"){
        title <- "Local climate opinion in Germany"
        source <- "Source: Levi et al. (forthcoming)"
        ls_subtitles <- ls_subtitles_EN
        ls_legendtlt <- ls_legendtlt_EN
        location <- "English"
      }
      subtitle <- paste0(ls_subtitles[i], ", in ", ls_years[i])
      legend_title <- paste0(ls_legendtlt[i], " (", ls_years[i],")")

      # -- plot object
      filename_out <- paste0("Figures/",location,"/map_", ls_vars[i],"_",ls_years[i],"_",language,".png")

      ggplot(data = map) +
        geom_sf(aes(fill = var), lwd=0, colour="white") +
        labs( title = title, subtitle = subtitle, caption = source) +
        scale_fill_gradientn(colors = c("#00006c","#8e7ea0", "#e3cb42", "#c36c17", "#8b0000"), na.value = "grey80",
                             limits=c(0,1),
                             labels = scales::percent, name = paste0("\n", legend_title,"\n"))+
        theme_void()+
        theme(
      legend.justification = c("left", "top"),
      legend.position = c(0, 0.98),     # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      plot.margin = unit(c(0.1, 0.1, 0.05, 1), "cm"),
      text = element_text(family = "Tahoma",  size = 20),
      plot.title = element_text(family = "Tahoma", size = 36),
      plot.subtitle = element_text(family = "Tahoma", size = 22),
      plot.caption = element_text(family = "Tahoma", size = 22),
      legend.title = element_text(family = "Tahoma", size = 22),
      legend.text = element_text(family = "Tahoma", size = 20),
      legend.key.size = unit(1, 'cm')
      )

      # -- export
      ggsave(filename = filename_out, scale=2, width = 2000, height = 2500 , units = "px")

    }

  }