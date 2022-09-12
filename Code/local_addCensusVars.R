addCensusVars <- function (dfImp){


# Create new factor variables for age, gender and education.
# Age must be a factor with 7 levels.
# (census includes 11 levels, but the first four are persons under 18)
dfImp$age_cat <- cut(dfImp$age, breaks = c(18, 25, 30, 40, 50, 65, 75, 100),
  include.lowest = TRUE, right = FALSE, labels = c(1:7))

# Education must be a factor with 7 levels (encoding as in census)
dfImp$educ_cat <- as.factor(dfImp$educ2)
dfImp$educ_cat <- fct_collapse(dfImp$educ_cat,
  `1` = c("1", "2", "3"), # ohne Berufsbildung (no professional education)
  # Anlernausbildung, berufliches Praktikum von mindestens 12 Monaten, (vocational training or apprenticeship of at least 12 months)
  # Berufsvorbereitungsjahr (occupational preparation year)
  `2` = c("4", "5", "6"), # Lehre & Berufsausbildung im dualen System (vocational education in dual system)
  # Vorbereitungsdienst für den mittleren Dienst in öff. Dienst, (preparation service in the middle civil service)
  # Berufsqualifizierender Abschluss an Berufsfachschule/Kollegschule, (vocational school or college graduation)
  # Abschluss 1-j, 2- oder 3-jährige Schule Gesundheitswesen, (graduation after a 1-, 2- or 3-year health system school)
  `3` = c("7", "8"), # Fachschulabschluss, (professional school)
  # (Meister/-in, Techniker/-in oder gleichwertiger Abschluss) (master or technician)
  `4` = c("9"), # Berufsakademie, Fachakademie (professional or technical academy)
  `5` = c("10", "11"), # Abschluss einer Verwaltungsfachhochschule, (advanced administrative, professional or engineering college)
  # Fachhochschulabschluss, auch Ingenieurschulabschluss
  `6` = c("12"), # Hochschule (university)
  `7` = c("13") # Promotion (doctorate)
)

# Gender must be a binary variable with level 0 for women and 1 for men
dfImp$gender <- as.factor(ifelse(dfImp$gender == 2, 0, 1))

  return(dfImp)
}