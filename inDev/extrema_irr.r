#' @title extrema.irr
#' @aliases  extrema.irr
#' @author Pierre Roudier
#' @description Finds regional extrema on a irregularly sampled data set
#' @include create_neig.r
# new implementation
 # user      system   elapsed
 # 38.4      0.004    38.409 # 300
 # 290.224   1.164    292.347 #1290
# original implementation
 # user     system  elapsed
 # 39.044   0.016   39.131
 # 301.188   0.576 302.909
 # 3434.312   31.084    3701.952

extrema.irr <- function(
  data.set,
  gridded.data,
  neig = NULL,
  zcol = 'z',
  duplicate = 'remove',
  nb.nn = 4, # Number of nearest neighbours to take into account if data is on a grid
  thresh.extrema = 1, # Significative threshold for the extrema
  verbose = FALSE
){
  if (is.null(neig)) {
    cat("\t\tWARNING : Old-style neig generation. This might be time-consuming.\n")
    neig <- create.neig(
      data.set,
      gridded.data = gridded.data,
      nb.nn = nb.nn,
      duplicate = 'remove',
      verbose = verbose
    )
  }
  else {
    if (!any(class(neig) != "neig")) stop("neig needs to be generated through the create.neig function.")
  }

  # Adding the value attribute
  neig$value <- data.set[[zcol]]
  neig <- data.table(neig)

  # Adding some extrema attributes
  neig$is.minima <- vector(mode='integer',length = neig$n)
  neig$is.maxima <- vector(mode='integer',length = neig$n)

  # Initialisation
  k <- 1

  # vectorize condition
  candidat <- vector(mode='integer',length = neig$n)
  amplitude.max <- vector(mode='integer',length = neig$n)

  # Optimisation possible
  # datat.table
  # parallelisation
  # library(doMC) # Use doMC package
  # registerDoMC() # Determine number of cores to use, default = #cores/2

  #tini1 <- proc.time()
  # For each point of the triangulation object
  for (i_point in (1:neig$n)) {
    #candidat <- 0
    #amplitude.max <- 0
    voisins <- neig$neig[[i_point]]
    #diff neig$value[i_point] - neig$value[voisins]
    candidat[i_point] <- list((neig$value[i_point] - neig$value[voisins])>0)
    amplitude.max[i_point] <- list(neig$value[i_point] - neig$value[voisins])
    #k <- k + length(voisins)
  }

  candidat <- unlist(lapply(candidat, sum))
  amplitude.max <- unlist(lapply(amplitude.max, function(x) max(abs(x))))

  n_neig <- unlist(lapply(neig$neig,length))
  abs_amplitude <- abs(amplitude.max)
  neig$is.minima[which((( abs_amplitude >= thresh.extrema) && (candidat == -1*n_neig)))] <- TRUE
  neig$is.maxima[which(((abs_amplitude  >= thresh.extrema) && (candidat == n_neig)))] <- TRUE



  if (verbose) {
    n.extrema <- length(which(neig$is.minima)) + length(which(neig$is.maxima))
    cat(paste('\t\t\tFound ',n.extrema,
              ' extrema points (',length(which(neig$is.minima)), ' minima, ',
              length(which(neig$is.maxima)),' maxima)\n',sep=''))
  }

  class(neig) <- c(class(neig),"extrema")

  return(neig)
}
