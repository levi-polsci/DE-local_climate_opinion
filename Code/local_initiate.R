
# Load libraries
library(haven)
library(plyr)
library(janitor)
library(tidyr)
library(tidyverse)
library(devtools)
library(dplyr)
library(ggplot2)
library(magrittr)
#library(car)
library(openxlsx)
library(collapse)
library(missRanger)
library(autoMrP)
library(parallel)
library(sf)
library(caret)
library(tidyr)
library(extrafont)
library(ggpubr)
library(spdep)
library(splm)
library(rmapshaper)
library(showtext)
library(plm)
library(broom)
library(grid)
library(gridExtra)
library(ggthemes)
wdFig <- "C:/Users/s.levi/OneDrive - Hertie School/Original Articles/Unpublished/Local attitudes/Figures"

source("Code/local_preProcessSNB.R")
source("Code/local_addAreaCode.R")
source("Code/local_createContextData.R")
source("Code/local_createPowerData.R")
source("Code/local_getCensusData.R")
source("Code/local_addGKZf.R")
source("Code/local_getElectionData.R")
source("Code/local_addCensusVars.R")
source("Code/local_addSocioDemoData.R")
source("Code/local_createIndividualData.R")
source("Code/local_addBinaryOutcomeVariables.R")
source("Code/local_checkContext.R")
source("Code/local_imputeNAs.R")
source("Code/local_addPLZ.R")
source("Code/local_scaleBerlinCensus.R")
source("Code/local_preProcessSommer.R")
source("Code/local_createIndividualData_heat.R")
source("Code/local_getHeatData.R")
source("Code/local_getOtherKommuneData.R")
source("Code/local_scaleHamburgCensus.R")
source("Code/local_scaleMunichCensus.R")
source("Code/local_add2021data.R")
source("Code/local_GetProximityMatrix.R")

theme_clean <- function() {
  theme_minimal(base_family = "Barlow Semi Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(family = "Barlow Semi Condensed Medium"),
          strip.text = element_text(family = "Barlow Semi Condensed",
                                    face = "bold", size = rel(1), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          plot.caption = element_text(hjust = 0))
}

tidy.splm <- function (x, conf.int=TRUE, conf.level=0.95,...){

  result <- summary(x)$CoefTable %>%
  tibble::as_tibble(rownames = "term") %>%
  dplyr::rename(estimate = Estimate,
                std.error = `Std. Error`,
                statistic = `t-value`,
                p.value = `Pr(>|t|)`)

  #conf.int <- FALSE

  if(conf.int){
    res2 <- lmtest::coeftest(x)
     ci <- suppressMessages(stats::confint(res2))
    # confint called on models with a single predictor
    # often returns a named vector rather than a matrix :(

    if (is.null(dim(ci))) {
      ci <- matrix(ci, nrow = 1)
      rownames(ci) <- names(coef(x))[1]
    }

    ci <- as_tibble(ci, rownames = "term")
    names(ci) <- c("term", "conf.low", "conf.high")
    result <- dplyr::left_join(result,ci,by="term")
  }

  result
}

grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {

    plots <- list(...)
    position <- match.arg(position)
    g <-   ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)

    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )

    grid.newpage()
    grid.draw(combined)

    # return gtable invisibly
    invisible(combined)

  }