% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iocalculator.R
\name{io_calculator}
\alias{io_calculator}
\title{Run a Fisheries Input/Output Model}
\usage{
io_calculator(
  catch,
  import,
  mult = multipliers,
  deflator = 0.8734298,
  imports_s = imports_states
)
}
\arguments{
\item{catch}{A data frame that details catch numbers at the state-species category level for a single year, including variables fips (FIPs number, 0 for US), spec_no (a numeric variable for species category), and base_catch (raw catch numbers in dollars).}

\item{import}{A data frame that includes imports numbers in dollars at the state level for a single year, including fips (FIPs number, 0 for US) and imports.}

\item{mult}{A data frame that includes 17 multipliers at the state-species category-economic category for a single year. Defaults to David Records numbers.}

\item{deflator}{A numeric value that adjusts jobs numbers from the current year to the year the multipliers were made; defaults to 0.8734298, which represents 2017 to 2014.}

\item{imports_s}{A data frame that includes multipliers governing the percentages of imports going to each economic category for a single year. Defaults to 2017 numbers. Set to False for no imports.}
}
\description{
This function takes in commercial fisheries catch numbers, IMPLAN multipliers, a GDP deflator, and imports numbers and outputs economic impacts.
}
