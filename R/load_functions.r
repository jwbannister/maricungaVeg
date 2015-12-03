#' Read geodatabase file.
#'
#' \code{load_gdb_points} loads point locations from a GDB (geodatabase) file
#'  into a dataframe object.
#'
#'  @param file Character string. GDB file to be read, including path.
#'  @param layer Character string. Layer within GDB file to be read.
#'  @return Dataframe with three columns: point ID, x coordinate,
#'    and y coordinate
load_gdb_points <- function(file=NULL, layer=NULL){
  data <- rgdal::readOGR(dsn=file,
                           layer=layer)
  data <- data.frame(data@data, data@coords)
  data
}

#' Read and concatenate multiple DBF files.
#'
#' \code{load_dbfs} loads a multiple DBF (dBase) files and joins them on a
#'  common ID variable into a dataframe object.
#'
#'  @param file Vector of character strings. DBF files to be read,
#'    including path.
#'  @return Dataframe with three columns: point ID, x coordinate,
#'    and y coordinate
load_dbfs <- function(files=NULL){
  for (i in files){
    temp <- foreign::read.dbf(file=i)
    if (i==files[1]){
      built_df <- temp
    } else{
      built_df <- dplyr::inner_join(built_df, temp)
    }
  }
  built_df
}

#' Transform a SpatialPolygonsDataFrame object into a data
#'
#' \code{build_polygon_df} takes a SpatialPolygonsDataFrame object and strips 
#' coordinates of points which define the polygons. Each point also carries 
#' it's associated polygon id.
#'
#' @param polygons SpatialPolygonsDataFrame object.
#' @return Dataframe with three columns: x coordinate, y coordinate,
#'    and polygon id.
build_polygon_df <- function(polygons=NULL){
  poly_df <- data.frame(x=numeric(), y=numeric(), poly.id=character())
  for (i in 1:length(polygons[[1]])){
    name <- as.character(polygons[[1]][i])
    boundary <- data.frame(x=polygons@polygons[[i]]@Polygons[[1]]@coords[ , 1],
                           y=polygons@polygons[[i]]@Polygons[[1]]@coords[ , 2],
                           area.id=name, stringsAsFactors=FALSE)
    poly_df <- rbind(poly_df, boundary)
  }
  poly_df
}

#' Assign points an area id bif they are inside defined polygons.
#'
#' \code{assign_points_to_polygons} takes a dataframe of of individual points, 
#' and a dataframe of points defining polygons (in the form returned by 
#' \code{build_polygon_df}) and assigns area ids to points contained in the
#' polygons.
#'
#' @param points dataframe.
#' @param polygons dataframe.
#' @return Dataframe identical to points dataframe, with area.id variable added.
#'  Points which do not fall within polygon boundaries will have area.id = NA.
assign_points_to_polygons <- function(points=NULL, polygons=NULL){
  points$area.id <- rep(NA, nrow(points))
  for (i in unique(polygons$area.id)){
    name <- i
    boundary <- dplyr::filter(polygons, area.id==i)
    chk <- as.logical(sp::point.in.polygon(points$x, points$y, boundary$x,
                                           boundary$y))
    points$area.id[which(chk)] <- name
  }
  points
}