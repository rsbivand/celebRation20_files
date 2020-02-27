## ---- echo = TRUE, mysize=TRUE, size='\\tiny', results='hide', warning=FALSE--------------------
suppressPackageStartupMessages(library(osmdata))
library(sf)

## ---- cache=TRUE, echo = TRUE, mysize=TRUE, size='\\tiny', results='hide', warning=FALSE--------
bbox <- opq(bbox = 'bergen norway')
byb0 <- osmdata_sf(add_osm_feature(bbox, key = 'railway',
  value = 'light_rail'))$osm_lines
tram <- osmdata_sf(add_osm_feature(bbox, key = 'railway',
  value = 'tram'))$osm_lines
byb1 <- tram[!is.na(tram$name),]
o <- intersect(names(byb0), names(byb1))
byb <- rbind(byb0[,o], byb1[,o])

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------------------------------
library(mapview)
mapview(byb)

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
library(sf)
new_eire <- st_read(system.file("shapes/eire.shp", package="spData")[1])

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
library(spdep)
eire.nb <- poly2nb(new_eire)
eireW <- nb2listw(eire.nb, style="W")
eireB <- nb2listw(eire.nb, style="B")

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------------------------------
plot(st_geometry(new_eire))
plot(eire.nb, st_coordinates(st_centroid(new_eire)), add=TRUE)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
moran.test(new_eire$OWNCONS, eireW)
geary.test(new_eire$OWNCONS, eireW)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
moran.test(new_eire$OWNCONS, eireB)
geary.test(new_eire$OWNCONS, eireB)


## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------------------------------
moran.plot(new_eire$OWNCONS, eireW)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
f <- OWNCONS ~ ROADACC
res0 <- lm(f, data=new_eire)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------------------------------
summary(res0)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
resLM <- lm.LMtests(res0, listw=eireW, test="all")
t(sapply(resLM, function(x) unlist(x[1:3])))


## ---- echo = TRUE, eval=TRUE, results='hide', warning=FALSE, message=FALSE, mysize=TRUE, size='\\tiny'----
library(spatialreg)

## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
res0D <- lmSLX(f, data=new_eire, listw=eireW)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------------------------------
summary(res0D)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
resLMa <- lm.LMtests(res0D, listw=eireW, test="all")
t(sapply(resLMa, function(x) unlist(x[1:3])))


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
summary(impacts(res0D))


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
res1 <- errorsarlm(f, data=new_eire, listw=eireW)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------------------------------
summary(res1, Hausman=TRUE)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
res1D <- errorsarlm(f, data=new_eire, listw=eireW, Durbin=TRUE)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------------------------------
summary(res1D, Hausman=TRUE)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
summary(impacts(res1D))


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
res2 <- lagsarlm(f, data=new_eire, listw=eireW)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------------------------------
summary(res2)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
tr <- trW(as(eireW, "CsparseMatrix"))
set.seed(1)
summary(impacts(res2, tr=tr, R=2000), short=TRUE, zstats=TRUE)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
res2D <- lagsarlm(f, data=new_eire, listw=eireW, Durbin=TRUE)

## ---- echo = TRUE, eval=FALSE, mysize=TRUE, size='\\tiny'---------------------------------------
summary(res2D)


## ---- echo = TRUE, eval=TRUE, mysize=TRUE, size='\\tiny'----------------------------------------
summary(impacts(res2D, tr=tr, R=2000), short=TRUE, zstats=TRUE)

########################
library(sf)
sf_extSoftVersion()
bp_file <- "b_pump.gpkg"
b_pump_sf <- st_read(bp_file)
library(mapview)
mapview(b_pump_sf)




