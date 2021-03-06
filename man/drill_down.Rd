% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_functions.r
\name{drill_down}
\alias{drill_down}
\title{Get values from all layers of raster stack.}
\usage{
drill_down(layers = NULL, stackname = NULL, point = NULL)
}
\description{
\code{drill_down} retrieves the value for a given point in all layers of a
 raster stack. The raster stack first layer should be point IDs, all
 subsequent layers should be values.
}
\details{
@param layers Vector. A vector to label the layers retrieved from the stack.
 @param stack RasterStack.
 @param point Integer. The point for which to retrieve values.
 @return Returns a data frame.
}

