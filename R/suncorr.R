#' suncorr
#'
#' This function takes a correlation matrix and turns it into a ggplot suncorr object.
#'
#' In a suncorr graph, each variable is represented as a "sun" surrounded by "moons".
#'
#' The size (and, perhaps, color) of the moons represents the degree of correlation with the sun.
#'
#' The suns and the moons are arranged in matching radial order, so that you can easily tell which correlation you are looking at.
#'
#' @param C This is a correlation matrix. Matrix, data.frame, or tibble all work. The diagonal will be ignored. The order of the variables in C will be used to arrange them clockwise.
#' @param names A character vector with the names of the variables, which will be displayed on the graph. By default takes the column names of C. Or, set to FALSE to omit variable names (although good luck understanding the graph).
#' @param sunsize If you would like the "suns" to be scaled at different sizes, perhaps using the variance, set this to a numeric vector with that size. Keep in mind that a value of 1 will be a same-size sun as a perfect correlation moon.
#' @param suncolor This is the color that the suns will be.
#' @param moonpositive This is the color that the positively-valued moons will be.
#' @param moonnegative This is the color that the negatively-valued moons will be.
#' @param sunscale  Multiply the circle diameter by this number for the suns.
#' @param moonscale Multiply the circle diameter by this number for the moons.
#' @param sunradius The distance from the center of the universe to the center point of the suns.
#' @param moonradius The distance from the center of each sun to the center of each moon.
#' @param nameradius The distance from the center of the universe to the names of the suns.
#' @param printsizesun Prints the size of suns right on 'em.
#' @param printsizemoon Prints the size of moons right on 'em.
#' @param namesize The size argument for the geom_text names of the suns.
#' @param labelsize The size arguments for the size of the suns/moons as written on them.
#'
#' @examples
#'
#' # Load up the correlation matrix between majors
#' data(C)
#' # And the within-major variances
#' data(majorvariance)
#'
#' # Draw the graph, futzing with a lot of the parameters to make it look nicer
#' suncorr(C,
#'     sunsize = majorvariance,
#'     sunscale = 1/min(majorvariance),
#'     labelsize = 5,
#'     moonradius = 1.25,
#'     sunradius = 5,
#'     nameradius = 2.5)
#'
#' @export

suncorr <- function(C, names = NULL, sunsize = NULL, suncolor = 'green', moonpositive = 'blue', moonnegative = 'red', sunscale = 1, moonscale = 1, sunradius = 4, moonradius = 1, nameradius = 2, printsizesun = TRUE, printsizemoon = FALSE, namesize = 5, labelsize = 3) {

  # We work with a matrix!
  C <- as.matrix(C)

  # If this isn't square that's bad
  if (ncol(C) != nrow(C)) {
    stop('C must be a square matrix.')
  }

  # How many suns we got?
  nsun <- ncol(C)

  # Default
  if (is.null(names)) {
    names <- colnames(C)
  }
  if (isFALSE(names)) {
    names <- rep('',nsun)
  }

  # Radians between each sun
  deggap <- 2*pi/(nsun)

  # Radial position of the center of each sun
  rotate <- 0:(nsun-1)*deggap

  # First, turn the correlation matrix into a set of points
  # Begin with the suns
  suns <- dplyr::tibble(sunid = 1:nsun,
               sunname = names,
               r = rotate,
               d = sunradius) %>%
    dplyr::mutate(x = p2cx(r, d),
           y = p2cy(r, d),
           xtext = p2cx(r, nameradius),
           ytext = p2cy(r, nameradius))

  # Put each moon on its own line in a data set
  moons <- dplyr::tibble(sunid = sort(rep(1:nsun,nsun)),
                  moonid = rep(1:nsun, nsun),
                  moonsize = as.vector(C)) %>%
    dplyr::filter(sunid != moonid) %>%
    dplyr::left_join(suns, by = 'sunid') %>%
    dplyr::left_join(suns %>%
                       dplyr::transmute(moonid = sunid,
                       rmoon = r), by = 'moonid') %>%
    dplyr::mutate(moonlabel = round(moonsize, 2)) %>%
    dplyr::mutate(dmoon = moonradius,
           pos = moonsize >= 0,
           moonsize = moonscale*abs(moonsize)) %>%
    # Locate the moons
    dplyr::mutate(x = p2cx(rmoon, dmoon, x, y),
           y = p2cy(rmoon, dmoon, x, y))

  mooncols <- c(moonnegative, moonpositive)
  # If there are only positive correlations, then the above won't work
  if (min(moons$pos) == 1) {
    mooncols <- moonpositive
  }

  # Size the suns
  if (is.null(sunsize)) {
    suns <- suns %>%
      dplyr::mutate(sunsize = sunscale,
             sunlabel = 1)
  } else {
    suns <- suns %>%
      dplyr::mutate(sunlabel = round(sunsize, 2)) %>%
      dplyr::mutate(sunsize = sunscale*sunsize)
  }

  # Build that graph!
  # Put on the suns
  p <- ggplot(suns, aes(x0 = x, y0 = y, r = sunsize/2)) +
    ggforce::geom_circle(fill = suncolor, color = suncolor) +
    # And add names
    geom_text(mapping = aes(x = xtext, y = ytext, label = sunname), size = namesize) +
    # Theming
    theme_void() +
    theme(legend.position = 'none') +
    coord_fixed()

  # If requested
  if (printsizesun) {
    p <- p +
      geom_text(mapping = aes(x = x, y = y, label = sunlabel), size = labelsize)
  }

  # Add moons
  p <- p +
    ggforce::geom_circle(data = moons, mapping = aes(x0 = x, y0 = y, r = moonsize/2, color = pos, fill = pos))+
    scale_discrete_manual(values = mooncols, aesthetics = c("color", "fill"))

  # If requested
  if (printsizemoon) {
    p <- p +
      geom_text(data = moons, mapping = aes(x = x, y = y, label = moonlabel), size = labelsize)
  }

  return(p)
}

#https://www.r-bloggers.com/convert-polar-coordinates-to-cartesian/
p2c <-function(r, d, x=0,y=0){
  ## Translate Polar coordinates into Cartesian coordinates
  ## based on starting location, distance, and bearing

  newx<-x+d*sin(r)  ##X
  newy<-y+d*cos(r)  ##Y
  return(c(newx,newy))
}
p2cx <-function(r, d, x=0,y=0){
  ## Translate Polar coordinates into Cartesian coordinates
  ## based on starting location, distance, and bearing

  newx<-x+d*sin(r)  ##X
  newy<-y+d*cos(r)  ##Y
  return(newx)
}
p2cy <-function(r, d, x=0,y=0){
  ## Translate Polar coordinates into Cartesian coordinates
  ## based on starting location, distance, and bearing

  newx<-x+d*sin(r)  ##X
  newy<-y+d*cos(r)  ##Y
  return(newy)
}
