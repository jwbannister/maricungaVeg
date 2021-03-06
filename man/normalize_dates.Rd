% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lai_functions.r
\name{normalize_dates}
\alias{normalize_dates}
\title{Normalize all dates to a single year.}
\usage{
normalize_dates(data = NULL)
}
\description{
\code{normalize_dates} changes the year of all dates to 1980 so values on
 sames days can be compared between years.
}
\details{
@param data Data frame. Column of date data titled "date".
 @return Returns original data frame with a new column of normalized dates.
}

