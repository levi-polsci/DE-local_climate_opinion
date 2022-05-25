
preProcessSurvey_heat <- function (survey_raw){

# variables that need (different) NA substition: recode RF/DK in likert-scale variables with middle value, recode DK/RF in other variables with NA
likert_5 <- c('y.oil_furnace', 'y.tax_promotion_owner', 'y.eff_heating', 'y.energy_audit', 'y.gas_furnace', 'y.tax_promotion_landlord',
              'y.mand_res', 'y.mand_eff', 'y.building_levy', 'y.co2_price')

survey_clean <- survey_raw %>%
  mutate_if(is.numeric, as.numeric) %>%
  select(key, gkz, age, gender,
         lk = nuts3, y.oil_furnace = oil_furnace, y.tax_promotion_owner = tax_promotion_owner, 
         y.eff_heating = eff_heating, y.energy_audit = energy_audit, y.gas_furnace = gas_furnace, 
         y.tax_promotion_landlord = tax_promotion_landlord, y.mand_res = mand_res, 
         y.mand_eff = mand_eff, y.building_levy = building_levy, y.co2_price = co2_price,
         educ1 = school, educ2 = further_edu) %>%
  mutate(across(all_of(likert_5), car::recode, '6=3')) %>% 
  mutate(across(educ2, car::recode, '1:3 ="1"; 4:7="2"; 8="3"; 9="4"; 10:11="5"; 12="6"; 13="7"; 14= "NA"'))

survey_clean$age <- cut(survey_clean$age, breaks = c(18, 25, 30, 40, 50, 65, 75, 100),
    include.lowest = TRUE, right = FALSE, labels = c(1:7))
  
survey_clean$educ2 <- suppressWarnings(as.numeric(as.character(survey_clean$educ2)))
  survey_clean$gkz <- suppressWarnings(as.numeric(as.character(survey_clean$gkz)))

  survey_clean <- survey_clean[!is.na(survey_clean$gkz),]

return(survey_clean)

  }
