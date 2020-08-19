#' @title Conversion of counts to rates
#' @description Calculates rates from 'observed' count and a
#' denominator data
#' @param traj [matrix (numeric)] longitudinal (e.g.
#' observed count) data (\code{m x n}). Each row represents an
#' individual trajectory (of observations). The columns show the
#' observations at consecutive time steps.
#' @param denomin [matrix (numeric)] longitudinal (denominator)
#' data of the same column as `traj` (\code{n}).
#' @param id_field [numeric or character] Default is \code{TRUE}.
#' The first column of both the `traj` and the `denomin` object
#' must be the unique (\code{id}) field. If \code{FALSE}, the function
#' will terminate. The assumption is that columns of both the
#' \code{traj} and \code{denominat} corresponds. That is, column2,
#' column3, ... represent time points 2, 3, ..., respectively, in
#' each object.
#' @param multiplier [numeric] A quantify by which to the ratio
#' \code{traj/denomin} is expressed. Default is \code{100}.
#' @usage rates(traj, denomin, id_field, multiplier)
#' @examples
#'
#' traj2 <- dataImputation(traj, id_field = TRUE, method = 2,
#' replace_with = 1, fill_zeros = FALSE)
#'
#' pop <- population #read denominator data
#'
#' pop2 <- as.data.frame(matrix(0, nrow(population), ncol(traj)))
#'
#' colnames(pop2) <- names(traj2$CompleteData)
#'
#' pop2[,1] <- as.vector(as.character(pop[,1]))
#'
#' pop2[,4] <- as.vector(as.character(pop[,2]))
#'
#' pop2[,8] <- as.vector(as.character(pop[,3]))
#'
#' list_ <- c(2, 3, 5, 6, 7, 9, 10) #vector of missing years
#'
#' #fill the missing fields with 'NA'
#' for(u_ in 1:length(list_)){
#'     pop2[,list_[u_]] <- "NA"
#' }
#'
#' #estimate missing fields
#' pop_imp_result <- dataImputation(pop2, id_field = TRUE, method = 2,
#' replace_with = 1, fill_zeros = FALSE)
#'
#' #calculate rates i.e. crimes per 200 population
#' crime_rates <- rates(traj2$CompleteData, denomin=pop_imp_result$CompleteData,
#' id_field=TRUE, multiplier = 200)
#'
#' @return A matrix of 'rates' measures
#' @export


rates <- function(traj, denomin, id_field=TRUE, multiplier = 100){

  solution <- list()
  dat1 <- traj
  dat2 <- denomin

  #examine the number of columns
  if(ncol(traj) < 3 | ncol(denomin) < 3){
    stop("*---Number of columns of dataset must be greater than 3---*")
  }

  #compare the number of columns
  if(ncol(traj)!=ncol(denomin)){
    stop("*---Number of columns must be the same---*")
  }

  #compare the number of columns
  if(id_field==FALSE){
    stop("*---unique field must be set as 'TRUE'!---*")
  }

  #check uniqueness of the fields
  if(id_field==TRUE){
    n_CL <- colnames(dat1)
    id_names1 <- as.vector(as.character(dat1[,1]))
    id_names2 <- as.vector(as.character(dat2[,1]))

    if(!length(id_names1)==length(unique(id_names1))){
      stop("(: The 'id_field' of the 'traj' object is not a unique field. Function terminated!!! :)")
    }
    if(!length(id_names2)==length(unique(id_names2))){
      stop("(: The 'id_field' of the 'denominator' object is not a unique field. Function terminated!!! :)")
    }
  }

  #----------------------------------------------------------
  #trajectory data
  #----------------------------------------------------------------------
  data1 <- apply(dat1[,2:ncol(dat1)], 2, as.numeric)#head(data1)
  data1 <- cbind(seq_len(nrow(data1)), data1)
  colnames(data1) <- c("ID", seq_len((ncol(data1)-1)))

  #----------------------------------------------------------
  #denominator data
  #----------------------------------------------------------------------
  data2 <- apply(dat2[,2:ncol(dat2)], 2, as.numeric)#head(data2)
  data2 <- cbind(seq_len(nrow(data2)), data2)
  colnames(data2) <- c("ID", seq_len(ncol(data2)-1))

  data_Fresh <- NULL
  keep_names <- NULL
  keep_names_id <- NULL

  #now normalise with population
  for(k in seq_len(length(id_names1))){#k<-1
    pop_cut <- as.numeric(data2[which(id_names2==id_names1[k]), 2:ncol(data2)])
    if(length(pop_cut)!=0){
      data_cut <- as.numeric(data1[k ,2:ncol(data1)])
      data_Pop_per <- (data_cut / pop_cut)*multiplier
      data_Fresh <- rbind(data_Fresh, round(data_Pop_per,digits=2))
      keep_names <- c(keep_names, id_names1[k])
      keep_names_id <- c(keep_names_id, k)
      #data[k,2] <- data_Pop_100[1]
    }
  }

  #list of trajectory ids common to both 'traj' and 'denonm'
  id <- NULL
  common_ids <- keep_names
  common_ids_join <- data.frame(id=keep_names_id, id_name=keep_names)
  noncommon_ids_traj <- as.vector(as.character(traj[,1]))
  noncommon_ids_traj <- noncommon_ids_traj[which(!noncommon_ids_traj%in% keep_names)]

  noncommon_ids_denom <- as.vector(as.character(denomin[,1]))
  noncommon_ids_denom <- noncommon_ids_denom[which(!noncommon_ids_denom%in% keep_names)]

  #rates data
  data_Fresh <- data.frame(cbind(as.factor(keep_names), data_Fresh))

  data_Fresh <- dplyr::left_join(common_ids_join, data_Fresh, by = c("id" = "X1"))
  data_Fresh <- dplyr::select(data_Fresh, -c(id))

  colnames(data_Fresh) <-  n_CL

  solution <- list(common_ids=common_ids,
                  noncommon_ids_from_trajectories=noncommon_ids_traj,
                  noncommon_ids_from_denominator=noncommon_ids_denom,
                  rates_data = data_Fresh)

  return(solution)

} #end of function


