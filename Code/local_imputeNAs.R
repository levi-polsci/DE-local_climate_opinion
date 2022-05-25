imputeNAs <- function (ind, cont,target, mode=1){

  if(!(target %in% c("individual", "context")))
    stop("Please specify whether to impute individual or context level data.")

  if(target=="individual"){
    print("Imputing individual level variables.")

    if(any(is.na(ind$key))){
      ind[is.na(ind$key), "key"] <- seq(from = max(individualData$key,na.rm = TRUE), length.out = nrow(ind[is.na(ind$key), ]))
    }

    #map(left_join(ind, cont, by=c("lk_kz", "year")), ~sum(is.na(.))) # check which variables contain missing values
    cont<- cont %>% select(-year) %>% collap(~ lk_kz) %>%filter(lk_kz %in% unique(ind$lk_kz))
    cont <- cont[ , colSums(is.na(cont)) == 0] # only using complete context level information
    joineddata <- ind %>% select(key, year, lk_kz,gender:educ2, educ_cat, age_cat)
    joineddata <-  left_join(joineddata, cont, by="lk_kz")
    ind_imp <- missRanger(data =joineddata, formula = . ~ .) # ca. 1.5h
    ind_imp$year <- ind$year
    ind <- left_join(ind %>% select(key:year, gkz_f:Bundesland,y.fff:y.evsubs),
                            ind_imp %>% select(key, year,gender,educ_cat,age_cat)) # create small ds in which you use technical info+outcome vars + imputed ind. predictors
    return(ind)

  }
  if(target=="context"){

    cont2 <- left_join(cont, collap(ind %>% select(lk_kz,year,gender:educ2,s2:env_att), ~ lk_kz + year), by=c("lk_kz", "year"))

    # The complication here is that you would like to impute variables that are missing for full years.
    if(mode==1){# Mode 1 solution: split the dataset yearwise, impute all variables where at least 1 variable is not missing and bind the datasets together

      for (y in unique(cont$year)){
        print(paste0("Impute context variables, year ", as.character(y)," ..."))
        cont_y <- cont2[cont2$year==y,]
        suppressWarnings( cont_y <- missRanger(data = cont_y,
                            formula = as.formula(paste0(paste0(colnames(cont_y[colSums(!is.na(cont_y)) > 0]), collapse = " + "), " ~ ",
              paste0(colnames(cont_y[colSums(!is.na(cont_y)) > 0]), collapse = " + "))))
        )
        if(y==min(unique(context_data$year)))
          cont_imp <- cont_y else
          cont_imp <- rbind(cont_imp, cont_y)
      }
    }else{# Mode 2: just impute anything anyway
       suppressWarnings(cont_imp <- missRanger(data = cont2, .~.))
    }

    cont_imp <- cont_imp %>% select(lk_kz:keine_heizung_im_gebaude)

    return(cont_imp)
  }
}