% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recfin_areas.R
\name{recfin_areas}
\alias{recfin_areas}
\title{Create standarized field for RecFIN data}
\usage{
recfin_areas(data, area_grouping, area_names)
}
\arguments{
\item{data}{read in RecFIN data}

\item{area_grouping}{list of area names in data source}

\item{area_names}{user area names}
}
\value{
A data frame
}
\description{
Create standarized field for RecFIN data
}
\examples{
recfin_areas(data = recfin,
area_grouping = list(c("CHANNEL", "SOUTH"), c("BAY AREA", "WINE", "CENTRAL", "REDWOOD", "NOT KNOWN")),
area_names = c("south_pt_concep", "north_pt_concep")
)


}
\author{
Chantel Wetzel
}
