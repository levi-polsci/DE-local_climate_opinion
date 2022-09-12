#~~~~~~~~~~~~~~~~~~~~~~~~~
# Model diffusion of wind attitudes
#~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~
# Prepare data

context <- readRDS("Data/contexts/other - landkreise/Context 2017-2019+2021/Kontextdaten2016-2019+2021.rds") %>% clean_names() %>%
  dplyr::rename(ags ="gkz") %>% dplyr::rename(solar ="solare_strahlungsenergie") %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))  %>%  select(ags, year, bev, arbeitslose_insgesamt,
  braunkohle, wind, steinkohle, solar,af_d, grune,cdu_csu,fdp) %>% mutate(across(bev:fdp, as.numeric)) %>% missRanger()

data <- readRDS("Data/results/results_lk_20220720.rds") %>% filter(var %in% c("onshore", "coal_exit", "ee_expansion")) %>%
  select(ags, year, value, var)  %>% filter(!(var=="coal_exit" & year==2021))%>%
  pivot_wider(names_from = var, values_from = value) %>% left_join(context)

pd <- pdata.frame(data, index=c("ags", "year"))
ind <- sapply(pd, is.numeric)
pd[ind] <- lapply(pd[ind], scale)

# Model diffusion of wind attitudes
margEff_Rostock <- tibble(ags = unique(data$ags), treatment =0)
margEff_Bayreuth <- tibble(ags = unique(data$ags), treatment =0)
#margEff[margEff$ags=="13071", "dSupport"] <- 0.2 # Strongest increase in 2018 in 13071
margEff_Rostock[margEff_Rostock$ags=="13003", "treatment"] <- 0.1 # 13003 Rostock - small connection
margEff_Bayreuth[margEff_Bayreuth$ags=="09472", "treatment"] <- 0.1 # # 09472 Bayreuth LK - large connection


#~~~~~~~~~~~~~~
# Estimate coefficients

# get proximity matrix
m <- GetProximityMatrix(data, type = "nb",print = FALSE, style = "C") # get proximity matrix
#m <- GetProximityMatrix(data_wind, type = "dist",print = FALSE)

fm.wind <- onshore ~  bev + arbeitslose_insgesamt +
  wind + af_d + grune+cdu_csu+fdp

model.fe <- suppressWarnings(spml(formula = fm.wind, data = pd, listw=m,
              spatial.error="b", effect = "twoways", model = "within"))
summary(model.fe)

W <- listw2mat(m)
rownames(W) <- seq_len(nrow(W))
#View(nb2mat(m$neighbours, glist = m$weights, style = ))
rho <- model.fe$coefficients["rho"][[1]]
I <- diag(nrow(W))

#~~~~~~~~~~~~~~
# Calculate impact

#direct <- I * beta
#margSpatialImpat$direct <- rowSums(direct)
margEff_Rostock$direct <- colSums(I* margEff_Rostock$treatment)
margEff_Bayreuth$direct <- colSums(I* margEff_Bayreuth$treatment)


margEff_Rostock$indirect <- colSums(rho* W * margEff_Rostock$treatment)
margEff_Bayreuth$indirect <- colSums(rho* W * margEff_Bayreuth$treatment)

margEff_Rostock$shortterm <- colSums((I*margEff_Rostock$treatment) + rho * W * margEff_Rostock$treatment)# colsums describe the accumulated effect that country i recevies from changes of all other countries
margEff_Bayreuth$shortterm <- colSums((I*margEff_Bayreuth$treatment) + rho * W * margEff_Bayreuth$treatment)

# average total effects
margEff_Rostock$TE <- colSums(solve(diag(nrow(W)) - rho * W) * margEff_Rostock$treatment)
margEff_Bayreuth$TE <- colSums(solve(diag(nrow(W)) - rho * W) * margEff_Bayreuth$treatment)



#~~~~~~~~~~~~~
# Print Impact

margEff_Rostock <- margEff_Rostock %>% dplyr::rename(CC_2 = ags)
margEff_Bayreuth <- margEff_Bayreuth %>% dplyr::rename(CC_2 = ags)


map_root <- st_read("Data/shapefiles/gadm40_DEU_2.shp") %>% # get spatial data for Germany at the county level
  st_transform(crs = 'ESRI:31494')
map_Rostock <- left_join(map_root, margEff_Rostock)
map_Bayreuth <- left_join(map_root, margEff_Bayreuth)


direct_Rostock <- ggplot(data = map_Rostock) + geom_sf(aes(fill = direct), lwd=0) + labs(title = "Externer Schock")+#labs(title = "Exogenous shock")+
      scale_fill_gradientn(colors =c("#ecebf0", "#7d61dc","#794CB2","#6B3887","#53245B",  "#2E122C"), na.value = "grey80",limits=c(0,0.2), labels = scales::percent_format(accuracy = 1), name = "")+
  scale_y_continuous(trans='log10')+  theme_void()+ theme(legend.position ="bottom",plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))

indirect_Rostock <- ggplot(data = map_Rostock) + geom_sf(aes(fill = indirect), lwd=0) +labs(title = "Indirect spatial impact")+
      scale_fill_gradientn(colors =c("#ecebf0", "#7d61dc","#794CB2","#6B3887","#53245B",  "#2E122C"), na.value = "grey80",limits=c(0,0.2), labels = scales::percent_format(accuracy = 1), name = "")+
  scale_y_continuous(trans='log10')+  theme_void()+ theme(legend.position ="bottom",plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))

shortterm_Rostock <- ggplot(data = map_Rostock) +geom_sf(aes(fill = shortterm), lwd=0) +labs(title = "Kurzfrister Diffusionseffekt")+#labs(title = "Shortterm spatial impact")+
      scale_fill_gradientn(colors =c("#ecebf0", "#7d61dc","#794CB2","#6B3887","#53245B",  "#2E122C"), na.value = "grey80",limits=c(0,0.2), labels = scales::percent_format(accuracy = 1), name = "")+
  scale_y_continuous(trans='log10')+  theme_void()+ theme(legend.position ="bottom",plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))

longterm_Rostock <- ggplot(data = map_Rostock) +  geom_sf(aes(fill = TE), lwd=0) +labs(title = "Langfristiger Diffusionseffekt")+#+labs(title = "Longterm spatial impact")+
      scale_fill_gradientn(colors =c("#ecebf0", "#7d61dc","#794CB2","#6B3887","#53245B",  "#2E122C"), na.value = "grey80",limits=c(0,0.2), labels = scales::percent_format(accuracy = 1), name = "")+
  scale_y_continuous(trans='log10')+  theme_void()+ theme(legend.position ="bottom",plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))

#2E122C
#53245B
#6B3887
#794CB2
#7d61dc
#937BE2
#A894E7
#BEAEED
#D3C7F3
#E7E1F8
#FBFAFE

direct_Bayreuth <- ggplot(data = map_Bayreuth) + geom_sf(aes(fill = direct), lwd=0) +
      scale_fill_gradientn(colors =c("#ecebf0", "#7d61dc","#794CB2","#6B3887","#53245B",  "#2E122C"), na.value = "grey80",limits=c(0,0.2), labels = scales::percent_format(accuracy = 1), name = "")+
  scale_y_continuous(trans='log10')+  theme_void()+ theme(legend.position ="bottom",plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))

indirect_Bayreuth <- ggplot(data = map_Bayreuth) + geom_sf(aes(fill = indirect), lwd=0) +
      scale_fill_gradientn(colors =c("#ecebf0", "#7d61dc","#794CB2","#6B3887","#53245B",  "#2E122C"), na.value = "grey80",limits=c(0,0.2), labels = scales::percent_format(accuracy = 1), name = "")+
  scale_y_continuous(trans='log10')+  theme_void()+ theme(legend.position ="bottom",plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))

shortterm_Bayreuth <- ggplot(data = map_Bayreuth) +geom_sf(aes(fill = shortterm), lwd=0) +
      scale_fill_gradientn(colors =c("#ecebf0", "#7d61dc","#794CB2","#6B3887","#53245B",  "#2E122C"), na.value = "grey80",limits=c(0,0.2), labels = scales::percent_format(accuracy = 1), name = "")+
  scale_y_continuous(trans='log10')+  theme_void()+ theme(legend.position ="bottom",plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))

longterm_Bayreuth <- ggplot(data = map_Bayreuth) + geom_sf(aes(fill = TE), lwd=0) +
      scale_fill_gradientn(colors =c("#ecebf0", "#7d61dc","#794CB2","#6B3887","#53245B",  "#2E122C"), na.value = "grey80",limits=c(0,0.2), labels = scales::percent_format(accuracy = 1), name = "")+
  scale_y_continuous(trans='log10')+  theme_void()+ theme(legend.position ="bottom",plot.margin = unit(c(0.0, 0, 0.0, 0), "cm"), legend.text = element_text(family = "Tahoma", size = 7))



ggarrange(direct_Rostock, #indirect_Rostock,
          shortterm_Rostock, longterm_Rostock,
         direct_Bayreuth, #indirect_Bayreuth,
          shortterm_Bayreuth, longterm_Bayreuth, nrow = 2, ncol = 3,
labels = c("a", "", "", "b", "", "", ""), common.legend = TRUE, legend = "bottom")
ggsave("Figures/Fig 5 - Simulated spatial impact_DE.png", width = 3000, height = 2000 , units = "px" )
