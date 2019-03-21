library(raster)
library(sp)
library(rgdal)

dom <- brick("Z:/gnr/transfer/projects/Re-Direct/RS/rd_jw_processing_2018/G4/CUBERT/g4_20180510_cubert/DOM_DEM/g4_20180510_cubert_DOM_1cm.tif")
names(dom) <- c("PAN", paste("WV_", seq(450, 998, 4), sep=""))
dom

rasterOptions(tmptime = 1.1)
rasterOptions(maxmemory = 1e10)
rasterOptions()


tictoc::tic("Aggregation")
ag_dom_1m <- aggregate(dom, fact = 100, fun = mean)
tictoc::toc()
ag_dom_1m 

plot(ag_dom_1m[[25]], 
     breaks = seq(10, 1200, 50), 
     col = terrain.colors(20),
     main="Aggregate", 
     axes=FALSE)


writeRaster(ag_dom_1m, filename="Z:/gnr/transfer/projects/Re-Direct/RS/rd_jw_processing_2018/G4/CUBERT/g4_20180510_cubert/DOM_DEM/g4_20180510_cubert_DOM_100cm.tif", 
            format="GTiff", datatype='INT2U', 
            options=c("COMPRESS=NONE", "INTERLEAVE=BAND", "TFW=YES"), 
            overwrite=TRUE)
