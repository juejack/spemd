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
      nb.nn = nb.nn,
      duplicate = 'remove',
      verbose = verbose
    )
  }
  else {
    if (!any(class(neig) != "neig")) stop("neig needs to be generated through the create.neig function.")
  }

  #require(data.table)
  # Adding some extrema attributes
  neig$is.minima <- vector(mode='logical', length = neig$n)
  neig$is.maxima <- vector(mode='logical', length = neig$n)

  # vectorizing plus or minus condition
  candidat_p <- vector(mode='list',length = neig$n)
  candidat_n <- vector(mode='list',length = neig$n)
  amplitude.max <- vector(mode='integer',length = neig$n)

  # Adding the value attribute
  neig$value <- data.set[[zcol]]

  # Initialisation
  k <- 1

  # Optimisation possible
  # datat.table
  # parallelisation
  # library(doMC) # Use doMC package
  # registerDoMC() # Determine number of cores to use, default = #cores/2

    # For each point of the triangulation object
  for (i_point in (1:neig$n)) {
    voisins <- neig$neig[[i_point]]
    #candidat_p[i_point] <- (neig$value[i_point] - neig$value[voisins])>0
    #candidat_n[i_point] <- (neig$value[i_point] - neig$value[voisins])<0
    amplitude.max[i_point] <- list(neig$value[i_point] - neig$value[voisins])
  }


  # not memory safe for bug dataset
  candidat_n <- lapply(amplitude.max, function(x) x<0)
  candidat_p <- lapply(amplitude.max, function(x) x>0)

  candidat_n <- -1*unlist(lapply(candidat_n, sum))
  candidat_p <- unlist(lapply(candidat_p, sum))
  amplitude.max <- unlist(lapply(amplitude.max, function(x) max(abs(x))))

  n_neig <- unlist(lapply(neig$neig,length))
  abs_amplitude <- abs(amplitude.max)
  neig$is.minima[which(( abs_amplitude >= thresh.extrema) & (candidat_n == -1*n_neig))] <- TRUE
  neig$is.maxima[which(((abs_amplitude  >= thresh.extrema) & (candidat_p == n_neig)))] <- TRUE


  n.extrema <- length(which(neig$is.minima)) + length(which(neig$is.maxima))
  cat(paste('\t\t\tFound ',n.extrema,
            ' extrema points (',length(which(neig$is.minima)), ' minima, ',
            length(which(neig$is.maxima)),' maxima)\n',sep=''))


  class(neig) <- c(class(neig),"extrema")

  return(neig)
}
