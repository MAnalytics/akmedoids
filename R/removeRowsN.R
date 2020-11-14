#' @title Removes rows that contain 'NA' and/or 'Inf' entries
#' @description This function removes any rows in which an 'NA'
#' or an 'Inf' entry is found.The ability to remove records with
#' 'Inf' entries distinguishes this function from the popular
#' 'na.omit()' function.
#'  removal of '' function differs from
#' 'na.omit(data)'
#' @param traj [data.frame (numeric)]: longitudinal data.
#' Each row represents an individual trajectory (of observations).
#' The columns show the observations at consecutive time points.
#' @param id_field [numeric or character] Whether the first column
#' of the \code{traj} is a unique (\code{id}) field.
#' Default: \code{FALSE}. If \code{TRUE} the function recognises
#' the second column as the first time step.
#' @param remove [integer] Type of missing entries to remove.
#' \code{1} for 'NA', \code{2} for 'Inf', and \code{3} for both.
#' Default:\code{1}.
#' @usage removeRowsN(traj, id_field=TRUE, remove=1)
#' @details Given a matrix (or a dataframe) containing an 'NA' or
#' an 'Inf' entry, the function returns only rows with
#' complete observations.
#' @return A matrix with complete observations
#' @examples
#'
#' data(traj)
#'
#' removeRowsN(traj, id_field=TRUE, remove=3)
#' @importFrom utils flush.console
#' @export

removeRowsN <- function(traj, id_field=TRUE, remove=1){

  id_ToremoveNA <- NULL
  id_ToremoveInf <- NULL
  solution <- list()

  if(id_field==TRUE){

    id_title1 <- as.character(colnames(traj))[1]
    id_title_rest <- as.character(colnames(traj))[2:length(colnames(traj))]
    id_field_list <- matrix(traj[,1],,1)
    traj <- traj[,2:ncol(traj)]
  }

  #collate all 'na' & 'Inf' entries
  for(h in seq_len(ncol(traj))){ #h=1


    id_ToremoveNA <- c(id_ToremoveNA,
                       which(is.na(as.numeric(as.character(traj[,h])))))

    id_ToremoveInf <- c(id_ToremoveInf,
                   which(is.infinite(as.numeric(as.character(traj[,h])))))

  }

  if(remove == 1){
    id_Toremove <- id_ToremoveNA

    if(length(unique(id_Toremove))!=0){
      traj <- traj[-unique(id_Toremove),]
      flush.console()
      print(paste("Message:", length(unique(id_Toremove)),
                  "row(s) containing 'NA' entries removed!", sep=" "))
    }
    totalRemoved <- length(unique(id_Toremove))
  }


  if(remove == 2){
    id_Toremove <- id_ToremoveInf

    if(length(unique(id_Toremove))!=0){
      traj <- traj[-unique(id_Toremove),]
      flush.console()
      print(paste("Message:", length(unique(id_Toremove)),
                  "row(s) containing 'Inf' entries removed!", sep=" "))
    }
    totalRemoved  <- length(unique(id_Toremove))
  }

  if(remove == 3){
    id_Toremove <- c(id_ToremoveNA, id_ToremoveInf)

    if(length(unique(id_Toremove))!=0){
      traj <- traj[-unique(id_Toremove),]
      flush.console()
      print(paste("Message:", length(unique(id_Toremove)),
                  "row(s) containing 'NA' or 'Inf' entries removed!", sep=" "))
    }
    totalRemoved <- length(unique(id_Toremove))
  }

  #append id_field
  if(id_field==TRUE && length(unique(id_Toremove))!=0){
    id_field_list <- matrix(id_field_list[-unique(id_Toremove),],,1)
    colnames(id_field_list) <- id_title1
    colnames(traj) <- id_title_rest
    #traj <- traj[-unique(id_Toremove),]
    traj <- cbind(id_field_list, traj)
  }

  if(id_field==TRUE && length(unique(id_Toremove))==0){
    colnames(id_field_list) <- id_title1
    colnames(traj) <- id_title_rest
    #traj <- traj[-unique(id_Toremove),]
    traj <- cbind(id_field_list, traj)
    print("**---No row(s) contains either 'NA' or 'Inf' entries---**")
  }

  solution <- list(totalRowsRemoved=totalRemoved, CleanData=traj)
  return(solution)

}


