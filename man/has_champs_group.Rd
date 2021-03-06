% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/has_condition.R
\name{has_champs_group}
\alias{has_champs_group}
\title{Check to see if a CHAMPS group is in the causal chain}
\usage{
has_champs_group(ads, group, cc = TRUE)
}
\arguments{
\item{ads}{CHAMPS analysis dataset}

\item{group}{string representing a CHAMPS group}

\item{cc}{if TRUE: searches across causal chain, if FALSE: searches in
underlying cause only}
}
\description{
Check to see if a CHAMPS group is in the causal chain
}
