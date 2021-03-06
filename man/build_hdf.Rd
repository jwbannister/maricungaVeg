% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/storage_functions.r
\name{build_hdf}
\alias{build_hdf}
\title{Build HDF5 file.}
\usage{
build_hdf(file = NULL, data = NULL)
}
\description{
\code{build_hdf} builds an HDF5 file to store LAI data for Owens Lake
 Seeps and Springs areas.
}
\details{
@param file Character string. HDF5 file to be created, including path. File
   must have ".h5" extension.
 @param data Data frame. Should include all applicable areas in a "zone"
   column.
 @return Creates HDF5 file at specified location.
}

