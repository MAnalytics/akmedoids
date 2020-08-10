
#' @title Whitespaces removal
#' @description This function removes all the leading and the
#' trailing whitespaces in data
#' @param traj [matrix (numeric)]: longitudinal data. Each row
#' represents an individual trajectory (of observations).
#' The columns show the observations at consecutive time points.
#' @usage wSpaces(traj)
#' @details Given a matrix suspected to contain whitespaces,
#' this function removes all the whitespaces and returns a
#' cleaned data. ’Whitespaces’ are white characters often
#' introduced into data during data entry, for instance by
#' wrongly pressing the spacebar. For example, neither " A"
#' nor "A " equates "A" because of the whitespaces that
#' exist in them. They can also result from systematic
#' errors in data recording devices.
#' @return A matrix with all whitespaces (if any) removed.
#' @references \url{https://en.wikipedia.org/wiki/Whitespace_character}
#' @examples
#' traj <- traj
#' wSpaces(traj)
#' @export

wSpaces <- function(traj){

  dat <- traj

  dat_Cleaned <- dat

  coln_ <- colnames(dat_Cleaned)

  count_ <- 0 #keep the count of whitespace removed.

  for(q in 1:seq_len(ncol(dat_Cleaned))){

    vec_Name1 <- trimws(as.vector(dat_Cleaned[,q]), which="right")

    count_ <- count_ + length(which(!vec_Name1%in%dat_Cleaned[,q]))

    vec_Name2 <- trimws(vec_Name1, which="left")

    count_ <- count_ + length(which(!vec_Name2%in%vec_Name1))

    vec_Name2 <- matrix(vec_Name2,,1)

    colnames(vec_Name2) <- coln_[q]

    dat_Cleaned[,q] <- vec_Name2
  }

  flush.console()

  print(paste(count_, "whitespaces found/removed!"))

  return(dat_Cleaned)

}






