#' @title create.neig
#' @aliases  create.neig
#' @author Pierre Roudier, modified by Jean-Daniel Sylvain (2017)
#' @description Initiates the neighbourhood relationships between the points in the processed data set
#' @import sp spdep, FNN
create.neig <- function(
  data.set,
  # gridded.data,
  nb.nn = 4,
  duplicate='remove',
  verbose = FALSE
){
  coords <- as.data.frame(coordinates(data.set))
  names(coords) <- c("x","y")

  # if (TRUE) {
  #   if (gridded.data){


  # Finding nearest neighbours
  #  original code
  #### data.set.nn <- knearneigh(as.matrix(coords),k=nb.nn,longlat=FALSE)
  #### Converting to nb object
  #### data.set.nb <- knn2nb(data.set.nn)

  # Converting to nb object
  kn <-  knn.index(coords, k=nb.nn)
  knnObj <- list(nn=kn, np = nrow(coords), k = nb.nn, dimension = 2, x = coords[,1:2] )
  class(knnObj) <- "knn"

  # calculate neighboor
  data.set.nb <- knn2nb(knnObj)
  neig <- list(NULL)
  neig$x <- coords[,1]
  neig$y <- coords[,2]
  neig$n <- nrow(coords)
  neig$neig <- data.set.nb

  # }
  # else {
  #   require(tripack)
  #   if (verbose) cat("\t\tGenerating neighbourhood relationships...\n")
  #   # Creating a triangulation
  #   if (verbose) cat("\t\t\tTriangulation of the data set...\t")
  #   neig <- tri.mesh(coords,duplicate=duplicate)
  #
  #
  #   if (TEST <- TRUE) {
  #     source("pruned_neig.R")
  #     require(rgeos)
  #     require(rgdal)
  #     bound.shp <- "/home/pierre/Documents/Travail/ACPA/Data/Hassall/swamp/boundary/Swamp.shp"
  #     bound <- readOGR("/home/pierre/Documents/Travail/ACPA/Data/Hassall/swamp/boundary/Swamp.shp", layer="Swamp")
  #     if (verbose) cat("tri.asSplines\n")
  #     neig.sl <- tri.asSpLines(neig)
  #     if (verbose) cat("gCrosses\n")
  #     out <- gCrosses(neig.sl,bound,byid=TRUE)
  #     neig <- neig.sl[!out,]
  #     if (verbose) cat("sapply\n")
  #     id.neighbours <- t(sapply(row.names(neig), function(x){as.numeric(unlist(strsplit(x, " ")))},USE.NAMES=FALSE))
  #     if (verbose) cat("getNeig\n")
  #     my.neig <- getNeig(id.neighbours)
  #     if (verbose) cat("ok\n")
  #     neig <- list()
  #     neig$neig <- my.neig
  #     neig$x <- coords[,1]
  #     neig$y <- coords[,2]
  #     neig$n <- nrow(coords)
  #     class(neig) <- "neig"
  #   }
  #
  #   if (verbose) cat("Done.\n")
  #   # Adding neighbours informations
  #   if (verbose) cat("\t\t\tConverting to neighbourhood object...\t")
  #   if (!TEST) neig$neig <- neighbours(neig)
  #   if (verbose) cat("Done.\n")
  # }

  class(neig) <- c(class(neig),"neig")
  gc()
  return(neig)
}
