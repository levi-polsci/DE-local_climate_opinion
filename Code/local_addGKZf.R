addGKZ_f <- function (data){

  data$RGS_Land_f <- as.factor(ifelse(data$land < 10, paste0("0", as.character(data$land)),as.character(data$land)))
  data$RGS_RegBez_f <- as.factor(as.character(data$regierungsbezirk))
  data$RGS_Kreis_f <- as.factor(ifelse(data$kreis < 10, paste0("0", as.character(data$kreis)),as.character(data$kreis)))
  data$RGS_Gemeinde_f <- as.factor(ifelse(data$gemeinde < 10, paste0("00", as.character(data$gemeinde)),
                                            ifelse(data$gemeinde < 100, paste0("0", as.character(data$gemeinde)),as.character(data$gemeinde))))

  return(data)
}