preProcessSurvey <- function (survey_raw){

  print("Cleaning individual level data...")

  # 1. Subsetting, NA substitution, renamin
  ###########################################################

  # which predictors to include
  predictors <- c('s2', 'e6_1', 'e6_2', 'e6_3', 'e6_4', 'e6_6', 'e6_5', 'u7', 'u8', 'u9', 'u10', 'lc1',
                  'lc5_1', 'lc5_2', 'lc5_3', 'lc5_4', 'lc5_5', 'k6',
                  'ku3_1', 'ku3_2', 'ku3_3', 'ku3_4',
                  'so3', 'so6', 'so7', 'so8')

  # variables that need (different) NA substition: recode RF/DK in likert-scale variables with middle value, recode DK/RF in other variables with NA
  likert_2 <- c('lc5_1', 'lc5_2', 'lc5_3', 'lc5_4', 'lc5_5', 'y.ctax', 'so7')
  likert_5 <- c('y.fff', 'y.nuclear_exit', 'y.ee_expansion', 'y.coal_exit', 'y.transmissionline_expansion', 'y.efficiency_expansion',
                'y.energyconsumption_reduction', 'y.emobility_expansion', 'y.solar', 'y.onshore', 'y.offshore', 'y.hydro' , 'y.bioenergy' , 'y.geothermal', 'y.solarplant',
                'u8','ku3_1', 'ku3_2', 'ku3_3', 'ku3_4', 'y.ice', 'y.speedlimit','y.opnv','y.bike','y.cityban','y.evsubs')
  other_3 <- c('s2', 'u7', 'k6')
  other_4 <- c('lc1')
  other_5 <- c('y.invest_res', 'y.protest.wind')
  other_6 <- c('educ1')

  survey_clean <- survey_raw %>%
    mutate_if(is.numeric, as.numeric) %>%
    select(key, year, plz, gkz, ind_nr, ind_pers_nr, ostwest, ostwest_Berlin_cut,bl, ostwest,
           gender = ges, age = altNum, educ1= so1, educ2= so2,
           y.fff = a13, y.climate.belief = e3, y.climate.human = e4,
           y.nuclear_exit = e5_1, y.ee_expansion = e5_2, y.coal_exit = e5_3, y.transmissionline_expansion = e5_4,
           y.efficiency_expansion = e5_5, y.energyconsumption_reduction = e5_6, y.emobility_expansion = e5_7,
           y.solar = e10_1, y.onshore=e10_2, y.offshore=e10_3, y.hydro = e10_4, y.bioenergy = e10_5, y.geothermal=e10_6, y.solarplant=e10_7,
           y.invest_res = b2, y.invest.pv = b3_1, y.protest.wind = b8,
           y.ctax=c0,
           y.ice = u3d_1, y.speedlimit = u3d_2, y.opnv=u3d_4, y.bike=u3d_6, y.cityban=u3d_7, y.evsubs=u3d_8,
           all_of(predictors)) %>%
    mutate(across(all_of(other_3), car::recode, '4=NA')) %>%
    mutate(across(all_of(other_4), car::recode, '5=NA')) %>%
    mutate(across(all_of(other_5), car::recode, '6=NA')) %>%
    mutate(across(all_of(other_6), car::recode,  '7=NA')) %>%
    mutate(across(all_of(likert_2), car::recode, '3=1.5')) %>%
    mutate(across(all_of(likert_5), car::recode, '6=3')) %>%
    mutate(across(y.climate.belief, car::recode, '2=0; 3=1; 1=2')) %>%
    mutate(across(y.climate.human, car::recode, '1=3; 2=1; 3=2;4=2')) %>%
    mutate(across(educ2, car::recode,  '14=NA')) %>%
    mutate(across(so6, car::recode, '13=NA')) %>%
    mutate(across(u9, car::recode, '1="EW_CDUCSU"; 2="EW_SPD"; 3="EW_AfD"; 4="EW_FDP"; 5="EW_Linke"; 6="EW_Gruene"; 7="EW_Other"; 8:10="EW_OtherDK"')) %>%
    mutate(across(so3, car::recode, '1="Job_Erwerb"; 2="Job_Schueler"; 3="Job_Student"; 4="Job_Rentner"; 5="Job_Kapital"; 6="Unterhalt"; 7="Job_Haus"; 8:10="Job_ALG"; 11:12="Job_Other"')) #%>%
    #mutate(value = 1)  %>% spread(u9, value,  fill = 0 ) %>%
    #mutate(value = 1)  %>% spread(so3, value,  fill = 0 )



  # 2. Some further recoding
  ###########################################################

  # Recoding climate change belief
  survey_clean[!is.na(survey_clean$y.climate.belief) & survey_clean$y.climate.belief==0, "y.climate.human"] <- 0

  # Recoding y.invest_solar
  survey_clean[!is.na(survey_clean$y.invest_res) & is.na(survey_clean$y.invest.pv), "y.invest.pv"] <- 0

  # Composite indicators for KU3
  survey_clean$env_att <- rowMeans(survey_clean[,c("ku3_1", "ku3_2", "ku3_3", "ku3_4")], na.rm = TRUE)
  survey_clean$env_att[is.nan(survey_clean$env_att)] <- NA

  # Make party hot encoding
  #survey_clean[!is.na(survey_clean$so7) & survey_clean$so7==2, "so8"] <- 8
  #survey_clean <- survey_clean %>%
  #  mutate(across(so8, car::recode, '1="party_CDUCSU"; 2="party_SPD"; 3="party_AfD"; 4="party_FDP"; 5="party_Linke"; 6="party_Gruene"; 7="party_Other"; 8:9="party_none"')) %>%
  #  mutate(value = 1)  %>% spread(so8, value,  fill = 0 )

  #survey_clean <- survey_clean[,1:(ncol(survey_clean)-1)] %>% # remove the last column which was accidently created by the NA's in so8
  #  mutate(across(EW_AfD:Unterhalt, as.factor)) %>%
  #  mutate(across(party_AfD:party_SPD, as.factor))

  return(survey_clean)


}