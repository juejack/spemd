# library
library(raster)
library(spemd)
library(raster)
library(MBA)
library(spdep)
library(FNN)

rst <- "/home/jds-linux/Documents/Data/Altimetrie/SRTM1/srtm_4326/n45_w073_1arc_v3_z01_buffer_srtm_extract.tif"
rst_in <- basename(rst)
inPath <- dirname(rst)
rst_name <- unlist(strsplit(x = rst_in, split = "\\."))[1]
ext <- unlist(strsplit(x = rst_in, split = "\\."))[2]

if(!exists("outPath")){
  outPath <- inPath
}
rst_out <- paste0(outPath,'/',rst_name, '_imf1.', ext)

# read raster
r <- raster(rst)

# create sp object
rxy <- as.data.frame(xyFromCell(r, 1:ncell(r)))
rxy$z <- r[]
rxy_na <- na.omit(rxy)
coordinates(rxy_na) <- ~x+y
gridded(rxy_na) <- TRUE

# Empirical decomposition
tini1 <- proc.time()
res.ncp <- spEMD(rxy_na, zcol = "z",
                 method = "splines",
                 n.imf.max = 1,
                 thresh.extrema = 0.1,
                 verbose = TRUE,
                 nb.nn = 6,
                 save_neig = FALSE)
tfin1 <- proc.time()
tfin1 - tini1

# save raster
writeRaster(raster(res.ncp['imf1']),filename = rst_out)
cat('---- spEMD is done.')
