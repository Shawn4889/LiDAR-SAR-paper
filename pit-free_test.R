#Author: Xiaoxuan Li
library(lidR)
library(R.oo)
site <- "Justicia2010"
workspace <- "C:\\Users\\lxiao\\Desktop\\20200704"
path <- file.path("C:\\Users\\lxiao\\Desktop\\20200704",paste0(site,"_test.las"))


#method: pitfree
las <- readLAS(path,select = "i,c,r")
las <- lasnormalize(las, tin())
sub <- 1
chm <- grid_canopy(las, 1, pitfree(thresholds = c(0,2,5,10,15), max_edge = c(0, 1), subcircle = sub))
out <- file.path(workspace,paste0(site,"_pit_",sub,".tif"))
writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)


#method: p2r
las <- readLAS(path,select = "i,c,r")
las <- lasnormalize(las, tin())
sub <- 1
chm <- grid_canopy(las, 1, p2r(subcircle = sub))
out <- file.path(workspace,paste0(site,"_p2r_",sub,".tif"))
writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)


#method: chm = dtm-dsm
las <- readLAS(path,select = "i,c,r")
dsm <- grid_canopy(las, res = 1, dsmtin())
dtm <- grid_terrain(
    las,
    res = 1,
    tin(),
    keep_lowest = FALSE,
    full_raster = FALSE,
    use_class = c(2L, 9L)
)
chm <- dsm - dtm
out <- file.path(workspace,paste0(site,"_chm.tif"))
writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)


#method 
las <- readLAS(path,select = "i,c,r")
las <- lasnormalize(las, tin())
sub <- 0
chm <- grid_canopy(las, res = 1, dsmtin(max_edge = sub))
out <- file.path(workspace,paste0(site,"_dsmtin_",sub,".tif"))
writeRaster(chm,out,options=c('TFW=YES'),overwrite=TRUE)





dir <- "C:\\Users\\lxiao\\Desktop\\20200815\\tile_-65080_-3140680_subset.las"
las <- readLAS(dir,select = "i,c,n,r")
plot(las)

dsm <- grid_canopy(las, res = 1, p2r())


dtm <- grid_terrain(
    las,
    res = 1,
    tin(),
    keep_lowest = TRUE,
    full_raster = FALSE
)

chm <- dsm - dtm

out1 <- "C:\\Users\\lxiao\\Desktop\\20200815\\canopy.tif"
out2 <- "C:\\Users\\lxiao\\Desktop\\20200815\\ground.tif"
out3 <- "C:\\Users\\lxiao\\Desktop\\20200815\\chm.tif"
writeRaster(dsm,out1,options=c('TFW=YES'),overwrite=TRUE)
writeRaster(dtm,out2,options=c('TFW=YES'),overwrite=TRUE)
writeRaster(chm,out3,options=c('TFW=YES'),overwrite =TRUE)

ground = lasfilter(las, Classification == 2)
metrics_g = grid_metrics(ground, ~min(Z), 1)
plot(metrics_g)
canopy = lasfilter(las, Classification == 5)
plot(canopy)
metrics_c = try(grid_metrics(canopy, ~max(Z), 1), silent = TRUE)
plot(metrics_c)
writeRaster(metrics_c,out1,overwrite = T)
writeRaster(metrics_g,out2,overwrite = T)
try(writeRaster(metrics_c,outfiles2,overwrite = T), silent = TRUE)
writeRaster(metrics_g,out2,overwrite = T)
