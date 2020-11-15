#' Sample labels of cluster groups
#'
#' A dataframe of alphabetical labels representing
#' the optimal solution of `traj` dataset based on `akClust`
#' function
#'
#' @format A dataframe containing one variable:
#' \itemize{
#'   \item label: alphabetical label by clusters
#'     }
"clustr"


#' Simulated population data.
#'
#' Sample simulated population data to be used as
#' the denominator variable against `traj` dataset. Contains
#' data for two consecutive census years
#'
#' @format A dataframe with the following variables:
#' \itemize{
#'   \item location_id: Character id of sample census unit
#'   at which the population is obtained.
#'   \item census_2003: Population estimates at the sample
#'   locations for the census year 2003.
#'   \item census_2007: Population estimates at the sample
#'   locations for the census year 2007.
#'     }
"popl"

#' Simulated longitudinal dataset
#'
#' Contains simulated trajectories belonging to one of the
#' three pre-defined groups, namely (a) decreasing, (b) stable
#' and (c) increasing groups.
#'
#' @format A dataframe with the following variables:
#' \itemize{
#'   \item X1: Values across locations at time step 1
#'   \item X2: Values across locations at time step 2
#'   \item X3: Values across locations at time step 3
#'   \item X4: Values across locations at time step 4
#'   \item X5: Values across locations at time step 5
#'   \item X6: Values across locations at time step 6
#'   \item X7: Values across locations at time step 7
#'   \item X8: Values across locations at time step 8
#'   \item X9: Values across locations at time step 9
#'   \item X10: Values across locations at time step 10
#'   \item X11: Values across locations at time step 11
#'   \item X12: Values across locations at time step 12
#'   \item X13: Values across locations at time step 13
#'   \item X14: Values across locations at time step 14
#'   \item X15: Values across locations at time step 15
#'   \item X16: Values across locations at time step 16
#'   \item X17: Values across locations at time step 17
#'   \item X18: Values across locations at time step 18
#'   \item X19: Values across locations at time step 19
#'   \item X20: Values across locations at time step 20
#'   \item X21: Values across locations at time step 21
#'     }
"simulated"


#' Time-at-risk for the Adjudicated Toronto
#' Youth Data (Sample 1)
#'
#' Real-life time-at-risk per year for 378 individuals
#' from the age of 8 to 38 in the Toronto, Ontario, Canada.
#' The data is obtained through the R package `crimCV`.
#' For further information, please see: Nielsen, J. (2018)
#' crimCV: Group-Based Modelling of Longitudinal Data.
#' R package version 0.9.6.
#' URL https://CRAN.R-project.org/package=crimCV.
#'
#' @format A dataframe with the following variables:
#'   \itemize{
#'   \item 8: Time-at-risk per year at age 8
#'   \item 9: Time-at-risk per year at age 9
#'   \item 10: Time-at-risk per year at age 10
#'   \item 11: Time-at-risk per year at age 11
#'   \item 12: Time-at-risk per year at age 12
#'   \item 13: Time-at-risk per year at age 13
#'   \item 14: Time-at-risk per year at age 14
#'   \item 15: Time-at-risk per year at age 15
#'   \item 16: Time-at-risk per year at age 16
#'   \item 17: Time-at-risk per year at age 17
#'   \item 18: Time-at-risk per year at age 18
#'   \item 19: Time-at-risk per year at age 19
#'   \item 20: Time-at-risk per year at age 20
#'   \item 21: Time-at-risk per year at age 21
#'   \item 22: Time-at-risk per year at age 22
#'   \item 23: Time-at-risk per year at age 23
#'   \item 24: Time-at-risk per year at age 24
#'   \item 25: Time-at-risk per year at age 25
#'   \item 26: Time-at-risk per year at age 26
#'   \item 27: Time-at-risk per year at age 27
#'   \item 28: Time-at-risk per year at age 28
#'   \item 29: Time-at-risk per year at age 29
#'   \item 30: Time-at-risk per year at age 30
#'   \item 31: Time-at-risk per year at age 31
#'   \item 32: Time-at-risk per year at age 32
#'   \item 33: Time-at-risk per year at age 33
#'   \item 34: Time-at-risk per year at age 34
#'   \item 35: Time-at-risk per year at age 35
#'   \item 36: Time-at-risk per year at age 36
#'   \item 37: Time-at-risk per year at age 37
#'   \item 38: Time-at-risk per year at age 38
#'     }
"TO1Risk"

#' Sample longitudinal dataset
#'
#' Simulated longitudinal datasets containing
#' trajectories with missing values
#' (\code{NA}, \code{Inf}, \code{null})
#'
#' @format A dataframe with the following variables:
#'   \itemize{
#'   \item location_ids: Character id of sample locations
#'   at which values are obtained.
#'   \item X2001: Values at time step 1 (i.e. year 2001)
#'   \item X2002: Values at time step 2 (i.e. year 2002)
#'   \item X2003: Values at time step 3 (i.e. year 2003)
#'   \item X2004: Values at time step 4 (i.e. year 2004)
#'   \item X2005: Values at time step 5 (i.e. year 2005)
#'   \item X2006: Values at time step 6 (i.e. year 2006)
#'   \item X2007: Values at time step 7 (i.e. year 2007)
#'   \item X2008: Values at time step 8 (i.e. year 2008)
#'   \item X2009: Values at time step 9 (i.e. year 2009)
#'     }
"traj"

#' Sample longitudinal dataset containing whitespaces
#'
#' Longitudinal dataset with both trailing and leading
#' whitespaces. For example, there is a trailing whitespace
#' at cell [3, 6], while there is a leading whitespace
#' at cell [9, 4].
#'
#' @format A dataframe with the following variables:
#'   \itemize{
#'   \item location_ids: Character id of sample locations
#'   at which values are obtained.
#'   \item X2001: Values at time step 1 (i.e. year 2001)
#'   \item X2002: Values at time step 2 (i.e. year 2002)
#'   \item X2003: Values at time step 3 (i.e. year 2003)
#'   \item X2004: Values at time step 4 (i.e. year 2004)
#'   \item X2005: Values at time step 5 (i.e. year 2005)
#'   \item X2006: Values at time step 6 (i.e. year 2006)
#'   \item X2007: Values at time step 7 (i.e. year 2007)
#'   \item X2008: Values at time step 8 (i.e. year 2008)
#'   \item X2009: Values at time step 9 (i.e. year 2009)
#'     }
"traj_wSpaces"
