addBinaryOutcomeVariables <- function (data, var, kommune=FALSE){

  if(any(is.na(data[, var])))
    stop("NA's in target varibale are not allowed.")

  if(kommune){
    likert_5 <- c("y.nuclear_exit", "y.ee_expansion", "y.coal_exit", "y.transmissionline_expansion", "y.efficiency_expansion",
                  "y.energyconsumption_reduction","y.emobility_expansion",  "y.solar", "y.onshore",  "y.offshore", "y.hydro",
                  "y.bioenergy" ,  "y.geothermal", 'y.ice','y.solarplant', 'y.speedlimit','y.opnv','y.bike','y.cityban','y.evsubs')
    other_5 <- c('y.protest.wind')

    data <- data %>%
      mutate(across(y.ctax, car::recode, '2=0; 1.5=0')) %>%
      mutate(across(all_of(likert_5), car::recode, '1=0; 2=0;3=0;4=1;5=1')) %>%
      mutate(across(all_of(other_5), car::recode, '2=1;3=1;4=0;5=0')) %>%
      mutate(across(y.climate.human, car::recode, '3=1;2=0;1=0'))

    return(data)

  }

    var_heat <- c('y.oil_furnace', 'y.tax_promotion_owner', 'y.eff_heating', 'y.energy_audit', 'y.gas_furnace', 'y.tax_promotion_landlord',
              'y.mand_res', 'y.mand_eff', 'y.building_levy', 'y.co2_price')

      likert_5 <- c('y.fff', 'y.nuclear_exit', 'y.ee_expansion', 'y.coal_exit', 'y.transmissionline_expansion', 'y.efficiency_expansion',
                'y.energyconsumption_reduction', 'y.emobility_expansion', 'y.solar', 'y.onshore', 'y.offshore', 'y.geothermal','y.hydro' , 'y.bioenergy' , 'y.geothermal', 'y.ice','y.solarplant', 'y.speedlimit','y.opnv','y.bike','y.cityban','y.evsubs')
  other_5 <- c('y.invest_res', 'y.protest.wind')

    if(!(var %in% c(likert_5, other_5, var_heat, "y.ctax", "y.climate.human", "y.invest.pv")))
    stop("Function not implemented for this specific policy")

    if(var %in% var_heat){
         data <- data %>%
      mutate(across(all_of(var_heat), car::recode, '1=0; 2=0;3=0;4=1;5=1'))
    }else{
         data <- data %>%
      mutate(across(y.ctax, car::recode, '2=0; 1.5=0')) %>% # Carbon tax - 1 - bereit; 1.5 and 2 nicht bereit bzw. DK
      mutate(across(all_of(likert_5), car::recode, '1=0; 2=0;3=0;4=1;5=1')) %>%
      mutate(across(all_of(other_5), car::recode, '2=1;3=1;4=0;5=0')) %>%
      mutate(across(y.climate.human, car::recode, '3=1;2=0;1=0'))
    }


# https://medium.com/codex/easy-implementation-of-dummy-coding-one-hot-coding-in-r-42c0486f8996

  return(data)
}