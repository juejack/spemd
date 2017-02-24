# library
library(raster)
library(spemd)
library(raster)
library(MBA)
library(spdep)
library(FNN)

rst <- "/home/jds/Temp/clip/n45_w074_clip2.tif"
wd <- "/home/jds/Documents/Data/Altimetrie/srtm"

files <- list.files(path = wd , pattern = "1arc_v3.tif",full.names = TRUE)
for ( i in 1: length(files)){
rst <- files[i]
rst_in <- basename(rst)
inPath <-  dirname(rst)
rst_name <- unlist(strsplit(x = rst_in, split = "\\."))[1]
ext <- unlist(strsplit(x = rst_in, split = "\\."))[2]

if(!exists("outPath")){
  outPath <- inPath
}
rst_out <- paste0(outPath,'/',rst_name, '_imf1.', ext)

# read raster
r <- raster(paste0(inPath,"/",rst_in))


# create sp object
rxy <- as.data.frame(xyFromCell(r, 1:ncell(r)))
rxy$z <- r[]

rxy_na <- rxy
rxy_na[is.na(rxy)] <- -99999
coordinates(rxy_na) <- ~x+y
gridded(rxy_na) <- TRUE


Rprof("profiling.out",line.profiling = TRUE)
### Your code ###


# Empirical decomposition
tini1 <- proc.time()
res.ncp <- spEMD(rxy_na, zcol = "z",
                 method = "splines",
                 n.imf.max = 1,
                 thresh.extrema = 0.1,
                 verbose = TRUE,
                 nb.nn = 4,
                 save_neig = FALSE)
tfin1 <- proc.time()
tfin1 - tini1
Rprof()
summaryRprof("profiling.out", lines="show")

# save raster
writeRaster(raster(res.ncp['imf1']),filename = rst_out)
cat('---- spEMD is done.')
}
