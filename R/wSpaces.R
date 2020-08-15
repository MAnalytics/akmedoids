
#' @title Whitespaces removal
#' @description This function removes all the leading and the
#' trailing whitespaces in data
#' @param traj [matrix (numeric)]: longitudinal data. Each row
#' represents an individual trajectory (of observations).
#' The columns show the observations at consecutive time points.
#' @param remove [character]: Type of whitespace to remove.
#' That is, "Left" (leading), (2) "Right" (trailing), or "Both"
#' (both leading and trailing whitespaces). Default: "Both".
#' @usage wSpaces(traj, remove="Both")
#' @details Given a matrix suspected to contain whitespaces,
#' this function removes the type of the whitespaces specified and returns a
#' cleaned data. ’Whitespaces’ are white characters often
#' introduced into data during data entry, for instance by
#' wrongly pressing the spacebar. For example, neither " A"
#' nor "A " is the same as "A" because of the whitespaces that
#' exist in them. They can also result from systematic
#' errors in data recording devices.
#' @return A matrix with all whitespaces (if any) removed.
#' @references \url{https://en.wikipedia.org/wiki/Whitespace_character}
#' @examples
#' traj <- traj
#' wSpaces(traj, remove="Both")
#' @export

wSpaces <- function(traj, remove="Both"){

  dat <- traj

  dat_Cleaned <- dat

  coln_ <- colnames(dat_Cleaned)

  count_trailing <- 0
  count_leading <- 0

  solution <- list()

  for(q in seq_len(ncol(dat_Cleaned))){

    #dat_Cleaned=test.data1

    if(remove=="Right"){
      #remove trailing whitespaces
      vec_Name1 <- trimws(as.vector(dat_Cleaned[,q]), which="right")
      #count the number of trailing whitespaces
      count_trailing <- count_trailing + length(which(!vec_Name1%in%dat_Cleaned[,q]))
    }

    if(remove=="Left"){
      #remove leading whitespaces
      vec_Name1 <- trimws(as.vector(dat_Cleaned[,q]), which="left")
      #count the number of leading whitespaces
      count_leading <- count_leading + length(which(!vec_Name1%in%dat_Cleaned[,q]))
    }

    if(remove=="Both"){
      #remove trailing whitespaces
      vec_Name1 <- trimws(as.vector(dat_Cleaned[,q]), which="right")
      #count the number of trailing whitespaces
      count_trailing <- count_trailing + length(which(!vec_Name1%in%dat_Cleaned[,q]))
      #remove leading whitespaces
      vec_Name2 <- trimws(vec_Name1, which="left")
      #count the number of leading whitespaces
      count_leading <- count_leading + length(which(!vec_Name2%in%vec_Name1))
      vec_Name1 = vec_Name2
    }

    vec_Name1 <- matrix(vec_Name1,,1)

    colnames(vec_Name1) <- coln_[q]

    dat_Cleaned[,q] <- vec_Name1
  }

  flush.console()

  if(remove=="Right"){
    print(paste(count_trailing, "trailing whitespaces found/removed!"))
    Removed <- count_trailing
  }

  if(remove=="Left"){
    print(paste(count_leading, "leading whitespaces found/removed!"))
    Removed <- count_leading
  }

  if(remove=="Both"){
    print(paste((count_leading + count_trailing), "leading whitespaces found/removed!"))
    Removed <- count_leading + count_trailing
  }

  solution <- list(NumberOfWhiteSpacesRemoved=Removed, CleanData=dat_Cleaned)

  return(solution)

}






