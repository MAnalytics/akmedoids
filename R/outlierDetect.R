
#Outlier detector
#' @title Outlier detection and replacement
#' @description This function identifies outlier observations in the trajectories, and allows users to replace the observations or remove trajectories entirely.
#' @param traj [matrix (numeric)]: longitudinal data. Each row represents an individual trajectory (of observations). The columns show the observations at consecutive time points.
#' @param id_field [numeric or character] Whether the first column of the \code{traj} is a unique (\code{id}) field. Default: \code{FALSE}. If \code{TRUE} the function recognises the second column as the first time step.
#' @param method [integer (numeric)] indicating the method for identifying the outlier. Options are: \code{'1'}: quantile method (\code{default}), and \code{'2'}: manual method. The \code{manual} method requires a user-defined value.
#' @param threshold [numeric] A cut-off value for outliers. If the \code{method} parameter is set as \code{'1'}:quantile, the \code{threshold} should be a numeric vector of probability between \code{[0,1]}, whilst if the \code{method} is set as \code{'2'}: \code{manual}, the \code{threshold} could be any numeric vector.
#' @param count [integer (numeric)] indicating the number of observations (in a trajectory) that must exceed the \code{threshold} in order for the trajectory to be considered an \code{outlier}. Default is \code{1}.
#' @param replace_with [integer (numeric)] indicating the technique to use for calculating a replacement for an outlier observation. The remaining observations on the row or the column in which the outlier observation is located are used to calculate the replacement.
#' The replacement options are: \code{'1'}: Mean value of the column, \code{'2'}: Mean value of the row and \code{'3'}: remove the row (trajectory) completely from the data. Default value is the \code{'1'} option.
#' @usage outlierDetect(traj, id_field = FALSE, method = 1, threshold = 0.95,
#' count = 1, replace_with = 1)
#' @details Given a matrix, this function identifies outliers that exceed the threshold and replaces the outliers with an estimate calculated using the other observations either the rows or the columns in which the outlier observation is located. Option is also provided to remove the trajectories (containing the outlier) from the data.
#' @examples
#' traj <- traj
#' traj <- dataImputation(traj, id_field=TRUE, method = 1, replace_with = 1)
#' traj <- props(traj, id_field=TRUE)#remove this later
#' outlierDetect(traj, id_field = TRUE, method = 1, threshold = 0.95,
#' count = 1, replace_with = 1)
#' outlierDetect(traj, id_field = TRUE, method = 2, threshold = 15,
#' count = 4, replace_with = 3)
#' @return A dataframe with outlier observations replaced or removed.
#' @export

outlierDetect <- function(traj, id_field = FALSE,
                          method = 1, threshold = 0.95,
                          count = 1, replace_with = 1){

dat <- traj

#back up data
b_dat <- dat
#remove the id field
if(id_field ==  TRUE){
  dat  <- dat[,2:ncol(dat)]
  c_name <- colnames(traj)[1]
}

#matrix to track the outlier incidents [TRUE or FALSE]
outlier_mat <- matrix(FALSE, nrow(dat), ncol(dat))

#------------------------------------
#if method: "quantile"
if(method==1){
  #check if the value is in-between [0,1]
  if(threshold < 0 | threshold > 1){
    stop("*--Terminated!!!--*, The 'threshold' value should be between 0 and 1.")
  }
#calculate the cut-off value based on the 'threshold'
thres_ <- as.vector(round(quantile(as.vector(unlist(as.data.frame(dat))),
                                       threshold), digits=5))
id_ <- which(dat > thres_)
  #update the outlier tracker
  outlier_mat[id_] <- "TRUE"
}

# method: "manual"
#------------------------------------
if(method==2){
  id_ <- which(dat > threshold)
  outlier_mat[id_] <- "TRUE"
}

#check if an entire row is ouliers...then terminate.
c_out <- NULL
for(n_ in 1:ncol(outlier_mat)){ #n_<-1
  c_out <- rbind(c_out, cbind(n_, length(which(outlier_mat[,n_]==TRUE))))
}

if(replace_with == 1){
  wc_out <- which(c_out[,2]==nrow(dat))
  if(length(wc_out)!=0){
    stop(paste("*--Function terminated!!!--* All observations on Column(s)",
               wc_out, "are outliers!! Please, set a higher 'threshold' value!!", sep=" "))
  }
}

#check if an entire row is ouliers...then terminate.
r_out <- NULL
for(m_ in 1:nrow(outlier_mat)){ #m_<-1
  r_out <- rbind(r_out, cbind(m_,length(which(outlier_mat[m_,]==TRUE))))
}

if(replace_with == 2){# check which trajectory has 100% outlier observations
  w_out <- which(r_out[,2]==ncol(dat))
  if(length(w_out)!=0){
    stop(paste("*--Function terminated!!!--* All observations on Row(s)",
               w_out, "are outliers!! Please, set a higher 'threshold' value OR manually remove these row(s)!!", sep=" "))
  }
}

#--------------------------------------------------
#identify the rows in where outliers are found
list_traj <- NULL
for(j in 1:nrow(outlier_mat)){ #j<-1
  w_ <- length(which(outlier_mat[j,]==TRUE))
  if(w_ >= count){ #checking the count
    list_traj <- rbind(list_traj, cbind(j,"TRUE"))
  }
}

#to replace the outlier observation,
if(!is.null(list_traj)){

#replace with mean of col
if(replace_with == 1){
  for(k in 1:nrow(list_traj)){ #k<-1
   idd_ <- which(outlier_mat[as.numeric(as.character(list_traj[k,1])),]==TRUE)

  #loop through each column and remove the outlier in
  #them before calculating the value of the mean column value
  for(l_ in 1:length(idd_)){ #l_<-1
    dat[as.numeric(as.character(list_traj[k,1])),idd_[l_]] <-
      round(mean(dat[-which(outlier_mat[,idd_[l_]]==TRUE),idd_[l_]]), digits = 2)
  }
 }
}

#replace with mean of row
if(replace_with == 2){
for(k in 1:nrow(list_traj)){ #k<-1
 #use the non-outlier observation for the calculation
 idd_nonOutlier <- which(outlier_mat[as.numeric(as.character(list_traj[k,1])),]==FALSE)
 idd_Outlier <- which(outlier_mat[as.numeric(as.character(list_traj[k,1])),]==TRUE)
 dat[list_traj[k,1],idd_Outlier] <-  round(mean(as.numeric(as.character(dat[list_traj[k,1],idd_nonOutlier]))), digits = 2)
 }
}

#to remove the outlier trajectory
if(replace_with == 3){
  dat <- dat[-as.numeric(as.character(list_traj[,1])),]  #as.numeric(as.character(
}

dat_ <- dat

if(id_field ==  TRUE & replace_with != 3){
 id <- data.frame(as.vector(b_dat[,1]))
 colnames(id) <- c_name
  b_dat <- cbind(id, dat)
  dat_  <- b_dat
}

if(id_field ==  TRUE & replace_with == 3){
  id <- data.frame(as.vector(b_dat[,1]))
  colnames(id) <- c_name
  #remove the oulier row from the column ids
  id <- id[-as.numeric(as.character(list_traj[,1]))]
  b_dat <-  b_dat[-as.numeric(as.character(list_traj[,1])),] #first remove the outlier observations
  dat_  <- b_dat
}

#if 'replace_with' is 1 or 2
if(replace_with==1|replace_with==2){
  flush.console()
  print(paste(nrow(list_traj), "trajectories were found to contain outlier observations and replaced accordingly!", sep=" "))
  print("Summary:")
  for(u_ in seq_len(nrow(list_traj))){ #u_<-1
    flush.console()
    print(paste("*--Outlier observation(s) was found in trajectory ",
                list_traj[u_,1]," --*", sep=""))
    }
}

#if 'replace_with' is 3
if(replace_with==3){
 flush.console()
 print(paste(nrow(list_traj),
             "trajectory(s) identified as outliers and removed!", sep=" "))
 print("Details:")
 for(u_ in 1:nrow(list_traj)){ #u_<-1
  flush.console()
  print(paste("*----- trajectory ",
              list_traj[u_,1], "removed"), sep=" ")
  }
}
}

#for method 2, in which outlier may not be found
if(is.null(list_traj)){
  dat_ <- b_dat
  print("No outlier(s) found!")
}


return(dat_)

}

