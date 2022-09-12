#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run regression and print results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### subset & transform data

context <- readRDS("Data/contexts/other - landkreise/Context 2017-2019+2021/Kontextdaten2016-2019+2021.rds") %>% clean_names() %>%
  dplyr::rename(ags ="gkz") %>% dplyr::rename(solar ="solare_strahlungsenergie") %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))  %>%  select(ags, year, bev, arbeitslose_insgesamt,
  braunkohle, wind, steinkohle, solar,af_d, grune,cdu_csu,fdp) %>% mutate(across(bev:fdp, as.numeric)) %>% missRanger()

data <- readRDS("Data/results/results_lk.rds") %>% filter(var %in% c("onshore", "coal_exit", "ee_expansion")) %>%
  select(ags, year, value, var)  %>% filter(!(var=="coal_exit" & year==2021))%>%
  pivot_wider(names_from = var, values_from = value) %>% left_join(context)

m <- GetProximityMatrix(data, type = "dist",print = FALSE) # get proximity matrix

pd <- pdata.frame(data, index=c("ags", "year"))
ind <- sapply(pd, is.numeric)
pd[ind] <- lapply(pd[ind], scale)

pd_coal <- pdata.frame(data[data$year!=2021,], index=c("ags", "year"))
ind <- sapply(pd_coal, is.numeric)
pd_coal[ind] <- lapply(pd_coal[ind], scale)

# fit model
fm.wind <- onshore ~  bev + arbeitslose_insgesamt +
  wind + af_d + grune+cdu_csu+fdp
fm.ee <- ee_expansion ~ bev + arbeitslose_insgesamt +
  wind + af_d + grune+cdu_csu+fdp
fm.coal <- coal_exit ~  bev + arbeitslose_insgesamt +
  wind + af_d + grune+cdu_csu+fdp


coef_wind <- suppressWarnings(spml(formula = fm.wind, data = pd, listw= m,
              spatial.error="b",effect = "twoways", model = "within")) %>% summary() %>% tidy.splm(conf.int = TRUE)
coef_ee <- suppressWarnings(spml(formula = fm.ee, data = pd, listw= m,
              spatial.error="b",effect = "twoways", model = "within")) %>% summary() %>% tidy.splm(conf.int = TRUE)
coef_coal <- suppressWarnings(spml(formula = fm.coal, data = pd_coal, listw= m,
              effect = "twoways", model = "within")) %>% summary() %>% tidy.splm(conf.int = TRUE)

coef_wind <- coef_wind %>%
  mutate(term = case_when(row_number() == 1 ~ "spatial diffusion",
                          row_number() == 2 ~ "population",
                          row_number() == 3 ~ "share of recipients of long term \nunemployment benefits (ALG II)",
                          row_number() == 4 ~ "wind power capacity",
                          row_number() == 5 ~ "share of AfD voter",
                          row_number() == 6 ~ "share of Greens voter",
                          row_number() == 7 ~ "share of CDU voter",
                          row_number() == 8 ~ "share of FDP voter"))
coef_ee <- coef_ee %>%
  mutate(term = case_when(row_number() == 1 ~ "spatial diffusion",
                          row_number() == 2 ~ "population",
                          row_number() == 3 ~ "share of recipients of long term \nunemployment benefits (ALG II)",
                          row_number() == 4 ~ "wind power capacity",
                          row_number() == 5 ~ "share of AfD voter",
                          row_number() == 6 ~ "share of Greens voter",
                          row_number() == 7 ~ "share of CDU voter",
                          row_number() == 8 ~ "share of FDP voter"))
coef_coal <- coef_coal %>%
  mutate(term = case_when(row_number() == 1 ~ "spatial diffusion",
                          row_number() == 2 ~ "population",
                          row_number() == 3 ~ "share of recipients of long term \nunemployment benefits (ALG II)",
                          row_number() == 4 ~ "wind power capacity",
                          row_number() == 5 ~ "share of AfD voter",
                          row_number() == 6 ~ "share of Greens voter",
                          row_number() == 7 ~ "share of CDU voter",
                          row_number() == 8 ~ "share of FDP voter"))


#coef <- coef %>%
#  mutate(term = case_when(row_number() == 1 ~ "share of CDU voters",
#                          row_number() == 2 ~ "share of AfD voter",
#                          row_number() == 3 ~ "wind power capacity",
#                          row_number() == 4 ~ "eligible voters",
#                          row_number() == 5 ~ "share of recipients of long term \nunemployment benefits (ALG II)",
#                          row_number() == 6 ~ "share of FDP voters",
#                          row_number() == 7 ~ "living space p.c.",
#                          row_number() == 8 ~ "coal power capacity",
#                          row_number() == 9 ~ "share of green voters",
#                          row_number() == 10 ~ "land use",
#                          row_number() == 11 ~ "spacial diffusion"))


p1 <- ggplot(coef_wind, aes(term, estimate)) + geom_hline(yintercept = 0, color="lightgrey", size=2) + geom_point() +
  labs(title = "Std. effect on local support for wind power")+ ylab("Estimates") + xlab("") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  rotate() + theme_clean()
p2 <- ggplot(coef_ee, aes(term, estimate)) + geom_hline(yintercept = 0, color="lightgrey", size=2) + geom_point() +
  labs(title = "Std. effect on local support for renewable energy")+ ylab("Estimates") + xlab("") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  rotate() + theme_clean() + theme(axis.text.y=element_blank())
p3 <- ggplot(coef_coal, aes(term, estimate)) + geom_hline(yintercept = 0, color="lightgrey", size=2) + geom_point() +
  labs(title = "Std. effect on local support for coal exit")+ ylab("Estimates") + xlab("") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  rotate() + theme_clean()+theme(axis.text.y=element_blank())

ggarrange(p1,p2,p3, ncol = 3, nrow = 1, labels = c("a", "b", "c"))

ggsave("Figures/Fig 4 - Regression results.png", width = 6000, height = 2000 , units = "px" )