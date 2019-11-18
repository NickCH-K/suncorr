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
#' @param sunsize If you would like the "suns" to be scaled at different sizes, perhaps using the covariance, set this to a numeric vector with that size.
#' @param suncolor This is the color that the suns will be.
#' @param moonpositive This is the color that the positively-valued moons will be.
#' @param moonnegative This is the color that the negatively-valued moons will be.
#' @param sunscale  Multiply the ggplot size argument by this number for the suns.
#' @param moonscale Multiply the ggplot size argument by this number for the moons.
#' @param allscale Just sorta make everything bigger
#' @param sunradius The distance from the center of the universe to the center point of the suns.
#' @param moonradius The distance from the center of each sun to the center of each moon.
#' @param nameradius The distance from the center of the universe to the names of the suns.
#' @param printsizesun Prints the size of suns right on 'em.
#' @param printsizemoon Prints the size of suns right on 'em.
#'
#' @examples
#'
#' # Load up the correlation matrix between majors
#' data(C)
#' # And the within-major covariances
#' data(majorvariance)
#'
#' # Draw the graph
#' suncorr(C, sunsize = majorvariance)
#'
#' @export

suncorr <- function(C, names = NULL, sunsize = NULL, suncolor = 'green', moonpositive = 'blue', moonnegative = 'red', sunscale = 4, moonscale = 1, allscale = 20, sunradius = .85, moonradius = .15, nameradius = .5, printsizesun = TRUE, printsizemoon = FALSE) {

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
  suns <- tibble(sunid = 1:nsun,
               sunname = names,
               r = rotate,
               d = sunradius) %>%
    mutate(x = p2cx(r, d),
           y = p2cy(r, d),
           xtext = p2cx(r, nameradius),
           ytext = p2cy(r, nameradius))

  # Put each moon on its own line in a data set
  moons <- tibble(sunid = sort(rep(1:nsun,nsun)),
                  moonid = rep(1:nsun, nsun),
                  moonsize = as.vector(C)) %>%
    filter(sunid != moonid) %>%
    left_join(suns, by = 'sunid') %>%
    left_join(suns %>%
                transmute(moonid = sunid,
                       rmoon = r), by = 'moonid') %>%
    mutate(dmoon = moonradius,
           pos = moonsize >= 0,
           moonlabel = round(moonsize, 2),
           moonsize = moonscale*abs(moonsize)) %>%
    # Locate the moons
    mutate(x = p2cx(rmoon, dmoon, x, y),
           y = p2cy(rmoon, dmoon, x, y))

  # Size the suns
  if (is.null(sunsize)) {
    suns <- suns %>%
      mutate(sunsize = sunscale,
             sunlabel = 1)
  } else {
    suns <- suns %>%
      mutate(sunsize = sunscale*sunsize,
             sunlabel = round(sunsize, 2))
  }

  # Build that graph!
  # Put on the suns
  p <- ggplot(suns, aes(x = x, y = y, size = sunsize)) +
    geom_point(color = suncolor) +
    # And add names
    geom_text(mapping = aes(x = xtext, y = ytext, label = sunname), size = 5) +
    # Theming
    theme_void() +
    theme(legend.position = 'none') +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_size_continuous(range = c(5,allscale))

  # If requested
  if (printsizesun) {
    p <- p +
      geom_text(mapping = aes(x = x, y = y, label = sunlabel), size = sunscale)
  }

  # Add moons
  p <- p +
    geom_point(data = moons, mapping = aes(x = x, y = y, size = moonsize, color = pos))+
    scale_discrete_manual(values = c(moonpositive, moonnegative), aesthetics = "color")

  # If requested
  if (printsizemoon) {
    p <- p +
      geom_text(data = moons, mapping = aes(x = x, y = y, label = moonlabel), size = moonscale*.75)
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
