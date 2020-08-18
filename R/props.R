#' @title Conversion of counts (or rates) to 'Proportion'
#' @description This function converts counts or rates to proportions.
#' @param traj [matrix (numeric)]: longitudinal data. Each row
#' represents an individual trajectory (of observations). The
#' columns show the observations at consecutive time points.
#' @param id_field [numeric or character] Whether the first
#' column of the \code{traj} is a unique (\code{id}) field.
#' Default: \code{FALSE}. If \code{TRUE} the function recognizes
#' the second column as the first time step.
#' @param digits [numeric] Specifying number of digits to
#' approximate the output to. Default: \code{4}.
#' @param scale [numeric] To scale the 'proportion' measures.
#' Default: \code{1}
#' @usage props(traj, id_field = TRUE, scale = 1, digits = 4)
#' @examples
#' traj <- dataImputation(traj, id_field = TRUE, method = 2, replace_with = 1,
#' fill_zeros = FALSE) #filling the missing values
#' traj <- props(traj, id_field = TRUE, scale=1, digits=4)
#' print(traj)
#' @details Given a matrix of observations (counts or rates), this
#' function converts each observation to a proportion equivalent to
#' the sum of each column. In other words, each observation is divided
#' by the sum of the column where it is located, i.e.
#' \code{prop = [a cell value] / sum[corresponding column]}
#' @return A matrix of proportion measures
#' @export

props <- function(traj, id_field = TRUE, scale = 1, digits=4){

  solution <- list()
  dat <- traj
  props_ <- dat

  if(id_field==FALSE){
    for(h in seq_len(ncol(dat))){
      prop <- (as.numeric(as.character(dat[,h]))/
               sum(as.numeric(as.character(dat[,h])))) * scale #
      props_[,h] <- round(prop, digits=digits)
    }
    props_check <- colSums(props_)
    return(props_)
  }

  if(id_field==TRUE){
    for(h in 2:ncol(dat)){ #h<-2
      prop <- (as.numeric(as.character(dat[,h]))/
               sum(as.numeric(as.character(dat[,h])))) * scale
      props_[,h] <- round(prop, digits = digits)
    }
    props_check <- colSums(props_[ ,2:ncol(dat)])
    return(props_)
  }

}


