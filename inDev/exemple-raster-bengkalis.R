# library
library(raster)
library(spemd)
library(raster)
library(MBA)
library(spdep)
library(FNN)

rst <- '/home/jds-linux/Documents/Data/Altimetrie/bengkalis/srtm_bengkalis.tif'
rst_in <- basename(rst)
rst_name <- unlist(strsplit(x = rst_in, split = "\\."))[1]
inPath <- dirname(rst)
ext <- unlist(strsplit(x = rst_in, split = "\\."))[2]

if(!exists("outPath")){
  outPath <- inPath
}
rst_out <- paste0(outPath,'/',rst_name, '_imf2.', ext)

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
                 n.imf.max = 2,
                 thresh.extrema = 0.1,
                 verbose = TRUE,
                 nb.nn = 4,
                 save_neig = FALSE)
tfin1 <- proc.time()
tfin1 - tini1

# save raster
writeRaster(raster(res.ncp['imf2']),filename = rst_out)
cat('---- spEMD is done.')
