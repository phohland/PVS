% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pvs.R
\name{pvs}
\alias{pvs}
\title{PVS}
\usage{
pvs(
  pre = pre,
  bsl,
  a = 1,
  drop = NULL,
  turnvars = NULL,
  ambivars = NULL,
  zvars = NULL,
  pvsNA = NA
)
}
\arguments{
\item{pre}{normalized data set}

\item{bsl}{baseline}

\item{a}{unique patient id}

\item{drop}{variables which shall not used}

\item{turnvars}{variables with "turned" direction}

\item{ambivars}{ambivalent variables}

\item{zvars}{z variables}

\item{pvsNA}{code how to handle NaN values during calculations
(default is NA)}
}
\value{
\code{pvs} list with PVS results for specific patient
(differences and weights plus PVS score)
}
\description{
The \code{pvs} function calculates a root mean square as PVS value
using the prior calculated baseline for a specific patient
}
