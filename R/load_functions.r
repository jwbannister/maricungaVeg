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