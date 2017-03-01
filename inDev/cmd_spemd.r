# library
library(raster)
library(spemd)


# set options and et args from the command line
options(warn = 0)
args <- commandArgs(trailingOnly = TRUE)



if(args[1] == '-help' || length(args) == 0){

  print("

        == Description ==

        Ce programme permet d'appliquer la méthode empirical decomposition mode
        à un raster fournit par l'utilisateur;

        Pour utiliser ce programme vous devez avoir le logiciel R prealablement installe
        et avoir intialise le programme comme variable environnement.


        ================


        == Parametres entree ==

        # -rst       : char. a position file obtained from RTKlib (ex. file.pos)


        ===================

        == return ==

        # A raster in outPath or inPath (if the previous is not set)
        ===================


        == Exemple ==

        # R_Script cmd_spemd.R  l7.tif

        ===================

        \n

        ")

}else if(length(args) == 1){
  cat('Default parameter used ...\n')
  rst_in <- basename(args[1])
  inPath <- dirname(args[1])
  rst_name <- unlist(strsplit(x = rst_in, split = "\\."))[1]
  ext <- unlist(strsplit(x = rst_in, split = "\\."))[2]

  if(!exists("outPath")){
    outPath <- inPath
  }
  rst_out <- paste0(outPath,'/',rst_name, '_imf1.', ext)

}


# read raster
r <- raster(rst_in)

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
                 nb.nn = 5,
                 save_neig = FALSE)
tfin1 <- proc.time()
tfin1 - tini1

# save raster
writeRaster(raster(res.ncp['imf1']),filename = rst_out)
cat('---- spEMD is done.')
