#' @title extrema.irr
#' @aliases  extrema.irr
#' @author Pierre Roudier
#' @description Finds regional extrema on a irregularly sampled data set
#' @include create_neig.r
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
  #neig$value <- data.set[[zcol]]

  # Adding some extrema attributes
  is.minima <- vector(mode='logical',length = neig$n)
  is.maxima <- vector(mode='logical',length = neig$n)

  # Initialisation
  k <- 1

  # For each point of the triangulation object
  for (i_point in (1:neig$n)) {

    candidat <- 0
    amplitude.max <- 0
    voisins <- neig$neig[[i_point]]

    for (i_voisin in (1:length(voisins))){

      i_voisin_curr <- voisins[i_voisin]
      amplitude <- data.set[[zcol]][i_point] - data.set[[zcol]][i_voisin_curr]
      if (amplitude < 0) candidat <- candidat - 1
      if (amplitude > 0) candidat <- candidat + 1
      if (abs(amplitude) > abs(amplitude.max)) amplitude.max <- amplitude

      k <- k+1
    }

    if ((abs(amplitude.max) >= thresh.extrema) && (candidat == -1*length(voisins))) {
      is.minima[i_point] <- TRUE
    }

    if ((abs(amplitude.max) >= thresh.extrema) && (candidat == length(voisins))) {
      is.maxima[i_point] <- TRUE
    }

  }

  if (verbose) {
    n.extrema <- length(which(neig$is.minima)) + length(which(is.maxima))
    cat(paste('\t\t\tFound ',n.extrema,' extrema points (',length(which(is.minima)),' minima, ',length(which(is.maxima)),' maxima)\n',sep=''))
  }

  neig$value <-  data.set[[zcol]]
  neig$is.minima <- is.minima
  neig$is.maxima <- is.minima
  class(neig) <- c(class(neig),"extrema")

  return(neig)
}
