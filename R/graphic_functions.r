#' Plot raster using ggplot.
#'
#' \code{plot_raster} creates a ggplot of a raster.
#'
#'  @param data Raster. A single layer raster to be plotted.
#'  @return Returns a ggplot object.
plot_raster_points <- function(data=NULL){
  temp <- data.frame(raster::rasterToPoints(data))
  colnames(temp)[3] <- "value"
  p1 <- ggplot(temp, aes(x=x, y=y)) +
    geom_raster(aes(fill=value)) +
    coord_fixed()
  p1
}

#' Write a .PNG file from a ggplot2 object.
#'
#' \code{makePNG} writes a ggplot2 object into a .PNG file.
#'
#'  @param plt ggplot2 object.
#'  @param filename Character string. File name and path (.png extension
#'    automatically added).
#'  @param ht Integer. Height of image in inches.
#'  @param wt Integer. Width of image in inches.
#'  @param ppi Integer. Pixels per inch resolution.
#'  @return Writes a file with the name and location speficied.

makePNG <- function(plt, filename, ht=6, wt=6, ppi=300){
  png(filename, width=wt*ppi, height=ht*ppi, res=ppi)
  print(plt)
  dev.off()
}

#' Create raster.
#'
#' \code{create_raster} builds a rater object out of a 3 column data frame.
#'
#'  @param data Data frame. A 3 column data frame with first two columns giving
#'    point coordinates and labeled "x" and "y". Third column will be raster
#'    point values.
#'  @return Returns raster of data with extents to encompass all points in
#'    data frame.
create_raster <- function(data=NULL){
  sp::coordinates(data) <- ~ x + y
  sp::gridded(data) <- TRUE
  rast <- raster::raster(data)
  rast
}

stack_rasters <- function(data=NULL, spacing=NULL,
                          proj="+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs
                          +ellps=WGS84 +towgs84=0,0,0"){
  grid <- expand.grid(x=seq(min(data$x), max(data$x), spacing),
                      y=seq(min(data$y), max(data$y), spacing))
  gridded_data <- dplyr::left_join(grid, data, by=c("x", "y"))
  raster_stack <- raster::stack()
  pb <- txtProgressBar(min=0, max=ncol(gridded_data), style=3, width=80)
  for (i in 3:ncol(gridded_data)){
    slice <- dplyr::select(gridded_data, 1, 2, i)
    rast <- create_raster(slice)
    raster_stack <- raster::stack(raster_stack, rast)
    setTxtProgressBar(pb, i - 2)
  }
  close(pb)
  raster::projection(raster_stack) <- proj
  raster_stack
}

#' Plot raster using ggplot.
#'
#' \code{plot_raster} creates a ggplot of a raster.
#'
#'  @param data Raster. A single layer raster to be plotted.
#'  @return Returns a ggplot object.
plot_raster <- function(data=NULL){
  temp <- data.frame(raster::rasterToPoints(data))
  colnames(temp) <- c("x", "y", "value")
  p1 <- ggplot(temp, aes(x=x, y=y)) +
    geom_raster(aes(fill=value)) +
    scale_fill_gradient2(high="#1a9641", midpoint=0, mid="#f7f7f7", low="#d7191c", space="Lab") +
    coord_fixed()
  p1
}
