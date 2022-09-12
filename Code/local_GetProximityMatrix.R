GetProximityMatrix <- function (ds, type="nb", print=FALSE, style="S"){

  #ds <- dsSolar

  ind <- unique(ds$ags)
  n <- length(ind)

  # Get Polygons and Neighborhood Matrix
  if(FALSE){
    counties <- tigris::counties()
    counties$GEOID <- as.character(as.numeric(counties$GEOID))
    #missing <- ds[!(ds$GEOID %in% counties$GEOID),"GEOID"]
    counties <- counties[counties$GEOID %in% ind,]
    counties$GEOID <- as.numeric(counties$GEOID)
    counties <- counties[order(counties$GEOID),]
    counties$GEOID <- as.character(counties$GEOID)
    save(counties, file = "Data/counties.rdata")
  }

  counties <- st_read("Data/shapefiles/gadm40_DEU_2.shp") # get spatial data for Germany at the county level
counties <- st_transform(counties, crs = 'ESRI:31494')

  counties <- counties[counties$CC_2 %in% ind,]
  counties <- counties[order(counties$CC_2),]
  ds <- ds[order(ds$ags),]

  if(!(identical(unique(ds$ags), counties$CC_2)))
     stop("dataset does not equal counties data")



  #for(i in seq_along(w$neighbours)){
  #  if(length(w$neighbours[[i]])<2)
  #    print(i)
  #}
  #counties[2351,]

  if(type=="nb"){
      nb <- poly2nb(counties, row.names = counties$CC_2, snap = 0.0001,
                queen = TRUE)
      w <- nb2listw(nb, style = style)
  }

  if(type=="dist"){
    # https://rdrr.io/github/r-spatial/spdep/man/nb2listwdist.html
    # Transform map into CRS projection
    #cnts_trns <- st_transform(counties, crs = 'ESRI:102003')
    # Calculate points and coordinates of centroids
    pts <- st_centroid(counties) # calculates centre points
    coords <- st_centroid(st_geometry(counties), of_largest_polygon=FALSE)
    # Calculate maximum distance of nearest neighbour
    k1 <- knn2nb(knearneigh(pts))
    all.linked <- max(unlist(nbdists(k1, coords)))
    # Create distance weighted neighborhood matrix
    nb_cnt <- dnearneigh(pts, 0, all.linked)

    if(print){ # Diagnostics: Print centroids and distance weighted neighbours

      summary(nb_cnt)#, coords)
      opar <- par(no.readonly = TRUE)
      cnts_simpl <- ms_simplify(counties, keep = 0.001,
                                keep_shapes = FALSE)
      # https://r-spatial.github.io/spdep/reference/dnearneigh.html
      setwd(wdFig)
      pdf("Figure X - Distance-based neighbors.pdf", width=10, height=8)
      showtext_begin()
      par(family = "sans", xpd=FALSE)
      plot(st_geometry(cnts_simpl), border="grey", reset=FALSE,
           main= paste0("Distance based neighbours 0-", format(all.linked)))
      plot(nb_cnt, coords, add=TRUE)
      showtext_end()
      while (!is.null(dev.list()))  dev.off()
      setwd(wd)
      par(opar)
    }

    # Calculate Inverse Distance matrix
    w <- nb2listwdist(nb_cnt, as(pts, "Spatial"), type="idw", style = "W",
                      alpha = 2, zero.policy = TRUE)
  }

   return(w)

}
