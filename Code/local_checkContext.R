checkContextComplete <- function (ind, cont, level="landkreis"){

  if(level=="landkreis"){

    if(nrow(as.data.frame(ind[!(ind$lk_kz %in% cont$lk_kz), "lk_kz"]))>0)
      print(paste0("Warning! These landkreise are missing in individual-level data: ", as.character(unique(ind[!(ind$lk_kz %in% cont$lk_kz), "lk_kz"])))) else
      print("All landkreise are present in context data")


  }else stop("Check only implementet at landkreis level!")

  #View(context_aggregated[context_aggregated$gkz %in% context_aggregated[!(context_aggregated$gkz %in% SNB_clean$lk_kz), "gkz"], ]) # Some cities have higher aggregation in context data than in SNB
  #election_data[!(election_data$kkz_f %in% SNB_clean$lk_kz), "kkz_f"]
#return(TRUE)
}