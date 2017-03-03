#' @title return.mean.enveloppe
#' @aliases  return.mean.enveloppe
#' @author Pierre Roudier
#' @description Returns the mean enveloppe
#' @importFrom MBA mba.points
return.mean.enveloppe2 <- function(
  extrema,
  donnee,
  data,
  zcol = "z",
  method = "splines",
  n.pts.spline = 3,	# number of points to locally interpolate
  verbose = TRUE
){

  if (method == "splines") {

    min.data <- as.data.frame(list(x=extrema$min$x,y=extrema$min$y,z=extrema$min$value))
    max.data <- as.data.frame(list(x=extrema$max$x,y=extrema$max$y,z=extrema$max$value))
    names(min.data) <- names(max.data) <- c('x','y','z')

    interp.min <- mba.points(min.data, coordinates(donnee), verbose = FALSE)
    interp.max <- mba.points(max.data, coordinates(donnee), verbose = FALSE)

    extrema.min.surf <- as.data.frame(interp.min$xyz.est)
    extrema.max.surf <- as.data.frame(interp.max$xyz.est)
    names(extrema.min.surf) <- names(extrema.max.surf) <- c('x','y','z')

  }
  else {
    stop("No other interpolation method that multi-level B splines had been implemented for the moment. Please use the splines option.\n")
  }

  mean.enveloppe <- as.data.frame(coordinates(donnee))
  mean.enveloppe[[zcol]] <- rowMeans(cbind(extrema.max.surf$z,extrema.min.surf$z)) # (extrema.max.surf$z-extrema.min.surf$z)
  coordinates(mean.enveloppe) <- ~x+y

  return(mean.enveloppe)
}
