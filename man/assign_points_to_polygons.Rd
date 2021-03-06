% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_functions.r
\name{assign_points_to_polygons}
\alias{assign_points_to_polygons}
\title{Assign points an area id bif they are inside defined polygons.}
\usage{
assign_points_to_polygons(points = NULL, polygons = NULL)
}
\arguments{
\item{points}{dataframe.}

\item{polygons}{dataframe.}
}
\value{
Dataframe identical to points dataframe, with area.id variable added.
 Points which do not fall within polygon boundaries will have area.id = NA.
}
\description{
\code{assign_points_to_polygons} takes a dataframe of of individual points, 
and a dataframe of points defining polygons (in the form returned by 
\code{build_polygon_df}) and assigns area ids to points contained in the
polygons.
}

