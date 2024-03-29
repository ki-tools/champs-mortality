% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch.R
\name{batch_rates_and_fractions}
\alias{batch_rates_and_fractions}
\title{Calculate rates and fraction of an input list of values.}
\usage{
batch_rates_and_fractions(dat, inputs)
}
\arguments{
\item{dat}{Processed CHAMPS dataset.}

\item{inputs}{Path to a csv file specifying each scenario to run with
columns 'site','catchment', 'age', 'condition', 'icd10_regex',
'causal_chain'. The columns 'age', 'condition' ,'site', and 'catchment'
can have multiple values separated by a semicolon. These variables
specify parameters that will be passed to \code{get_rates_and_fractions()}
Alternately, this can be a list of named lists of parameters with the
same names of the csv file. See examples below.}
}
\description{
Allows user to provide an input table in CSV format
to iteratively calculate results using get_rates_and_fractions()
with the output returned in one row per set of inputs.
}
