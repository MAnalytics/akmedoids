
#' @title Descriptive (Change) statistics
#' @description This function perform two tasks:
#' (i) it generate the descriptive and change statistics
#' of groups, particularly suited for the outputs form
#' the \code{\link{akclustr}} function, and
#' (ii) generates the plots of the groups (performances).
#' @param ak_object An output of \code{\link{akclustr}} function.
#' The object contains individual trajectories and their cluster
#' solution(s) at the specified values of \code{k}. Also, includes
#' the optimal value of \code{k} based on the criterion specified.
#' at (different) values of \code{k} the \code{traj}.
#' @param k [integer] \code{k} cluster to generate its solution.
#' @param reference [numeric] Specifying the reference line from
#' which the direction of each group is measured. Options are:
#' \code{1}: slope of mean trajectory, \code{2}: slope of medoid
#' trajectory, \code{3}: slope of a horizontal line
#' (i.e. slope = 0). Default: \code{1}.
#' @param n_quant [numeric] Number of equal intervals (quantiles)
#' to create between the reference line \code{(R)} and the medoids
#' \code{(M)} of the most-diverging groups of both sides of
#' \code{(R)}. Default is \code{4} - meaning quartile subdivisions
#' on each side of \code{(R)}. In this scenario, the function
#' returns the quartile in which the medoid of each group falls.
#' This result can be used to further categorize the groups into
#' 'classes'. For example, groups that fall within the \code{1st}
#' quartile may be classified as 'Stable' groups (Adepeju et al. 2021).
#' @param show_plots [TRUE or FALSE] Provides the trajectory group
#' plot. Please, see \code{plot_akstats} function for more
#' plot options.
#' Defaults \code{FALSE}
#'
#' @s3method print_akstats
#' @examples
#'
#' data(traj)
#'
#' trajectry <- data_imputation(traj, id_field = TRUE, method = 1,
#' replace_with = 1, fill_zeros = FALSE)
#'
#' print(trajectry$CompleteData)
#'
#' trajectry <- props(trajectry$CompleteData, id_field = TRUE)
#'
#' aksolution <- akclustr(trajectry, id_field = TRUE,
#'     method = "linear", k = c(3,5), crit='Calinski_Harabasz')
#'
#' print_akstats(aksolution, k = 4, show_plots=FALSE)
#'
#' @details Generates the plot of trajectory groupings.
#' Given an \code{ak_object} class (from
#' the \code{akclustr} function), this function show the
#' plots of cluster groups. The plot component draws from
#' \code{plot_akstats} function.
#' @return A plot showing group membership or sizes (proportion)
#' and statistics.
#' @references \code{1}. Adepeju, M. et al. (2021). Anchored k-medoids:
#' A novel adaptation of k-medoids further refined to measure
#' inequality in the exposure to crime across micro places,
#' doi: 10.1007/s42001-021-00103-1.
#' @references \code{2}. Wickham H. (2016). Elegant graphics for
#' Data Analysis. Spring-Verlag New York (2016).
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @importFrom ggplot2 stat_summary scale_colour_brewer theme_light
#' theme geom_area scale_x_continuous scale_fill_brewer facet_wrap
#'
#' @export
print_akstats<- function(ak_object, k = 3, reference = 1,
                      n_quant = 4,
                      show_plots=FALSE){

  UseMethod('print_akstats')
}

#' @export
print_akstats.default <- function(ak_object, k = 3, reference = 1,
                     n_quant = 4,
                     show_plots=FALSE){

  #first testing that correct values of k is specified.
  #get all values of k..
  all_K <- as.vector(unlist(lapply(ak_object$solutions, attributes)))

  if(!k %in% all_K){
    stop(paste("*----k =", k, "is not applicable!. Print the",
    "'akobject' to see allowed k-values----*", sep=" "))
  }

  # check object type
  if(class(ak_object)[1] != "akobject"){
    stop("*----Object not right type!! 'akclustr' object required!----*")
  }

  #test data type/or class

  #extract variables
  traj <- ak_object$traj
  clustr <- as.vector(ak_object$solutions[[k-2]])
  id_field <- ak_object$id_field

  #testing that data and clusters have equal number of elements
  if(length(clustr)!=nrow(traj)){
    stop("*----Unequal number of clusters elements and trajectories----*")
  }

  #joining the data with clusters
  clustr <- data.frame(cbind(traj, clusters=clustr))

  dat <- traj #back up traj

  n_quant <- round(n_quant, digits = 0)

  if(n_quant < 2 | n_quant > 10){
    stop(paste("*----Please, enter an integer between 2",
    "and 10 for the 'n_quant' argument'!!!----*", sep=" "))
  }

  #test id_field is true
  if(id_field==TRUE){
    dat <- dat[,2:ncol(dat)]
    n_CL <- colnames(clustr)[1]
    col_names <- as.vector(clustr[,1])

    #test if id field  is unique
    if(!length(col_names)==length(unique(col_names))){
      stop(paste("(: The 'id_field' is not a unique field.",
                 "Function terminated!!! :)", sep=" "))
    }
  }

  #test if id_field is excluded for traj
  if(id_field==FALSE){
    clustr <- cbind(seq_len(nrow(clustr)), clustr)
  }

  #collect cluster list
  clusters <- as.vector(clustr[,ncol(clustr)])

  data_subset <- clustr[,seq_len((ncol(clustr))-1)]

  data_subset <- as.data.frame(data_subset)

  colnames(data_subset) <- c("code", seq_len((ncol(data_subset))-1))

  data.subset.melted <- suppressWarnings(melt(data_subset, id="code"))

  #append cluster list with traj
  data.subset.melted <- cbind(data.subset.melted,
                            rep(clusters, ncol(data_subset)-1))
  colnames(data.subset.melted) <- c("id","Year","value", "clusters")

  #----------------------------------------------------
  #preparing the data to generate descriptive statitics
  year_uni <- as.vector(unique(data.subset.melted$Year))
  order_Cluster <- as.vector(unique(data.subset.melted$clusters))
  clusters_uni <-
    order_Cluster[order(as.vector(unique(data.subset.melted$clusters)))]

  change_ave_yr_ALL <- NULL

  for(q in seq_len(length(clusters_uni))){

    all_clust_list <-
      data.subset.melted[which(data.subset.melted$clusters==clusters_uni[q]),]

    ave_yr <- NULL

      for(m in seq_len(length(year_uni))){
        yr_ <-
          all_clust_list[which(as.vector(all_clust_list$Year)==year_uni[m]),]

        ave_yr <- c(ave_yr, sum(yr_$value))
      }

      change_ave_yr_ALL <- rbind(change_ave_yr_ALL,  ave_yr)
  }

  #whether to plot the clusters
    #----------------------------------------------------
    #plotting
    #----------------------------------------------------

    #calling the 'plot_akstats'
  #   options(rgl.useNULL = TRUE)
  #   plt = plot_akstats(ak_object, k = k, type="lines", y_scaling="fixed")
  #
  #   if(show_plots==TRUE){
  #     #plot
  #     #flush.console()
  #     #dev.new(width=3, height=3)
  #     options(rgl.useNULL = TRUE)
  #     print(plt)
  #   }
  #
  #   #do not show plot
  #   if(show_plots==FALSE){
  #     #Do nothing
  #   }
  #
  # #----------------------------------------------------
  # #To generate descriptive statistics
  # all_statistics <- list()
  #
  # #variable for change statistics
  # desc_Stats <- NULL
  # ll_ <- clusters_uni
  # group <- clusters_uni
  #
  # for(n in seq_len(length(ll_))){
  #
  #   #Calculating the number of trajectories
  #   a1 <- length(which(clusters%in%ll_[n]))
  #   a2 <- round((length(which(clusters%in%ll_[n]))/
  #                length(clusters))*100,digits = 1)
  #   a3 <- round((change_ave_yr_ALL[n,1] /
  #                sum(change_ave_yr_ALL[,1]))*100, digits = 1)
  #   a4 <- round((change_ave_yr_ALL[n,ncol(change_ave_yr_ALL)]/
  #     sum(change_ave_yr_ALL[,ncol(change_ave_yr_ALL)]))*100, digits = 1)
  #   a5 <- round((a4-a3), digits=1)
  #   a6 <- round(((a4-a3)/a4)*100, digits=1)
  #   desc_Stats <-  rbind(desc_Stats, cbind(a1, a2, a3, a4, a5, a6))
  # }
  #
  # colnames(desc_Stats) <- c("n","n(%)","%Prop.time1",
  #                           "%Prop.timeT", "Change", "%Change")
  # rownames(desc_Stats) <- seq_len(nrow(desc_Stats))
  # desc_Stats <- as.data.frame(cbind(group, desc_Stats))
  # attrib1 <- c("'n'->size (number of traj.); 'n(%)'->%size;
  #              '%Prop.time1'->% proportion of obs. at time 1;
  #              '%Prop.timeT'-> % proportion of obs. at time T;
  #              'Change'-> absolute change in proportion between time1 and timeT;
  #              '%Change'-> % change in proportion between time 1 and timeT")
  #
  # #----------------------------------------------------
  # #To generate slope statistics
  # #required
  #
  # sl_List <- NULL
  #
  # time <- as.numeric(seq_len(ncol(dat)))
  #
  # for(i in seq_len(nrow(dat))){ #i<-1
  #   b <- coefficients(lm(as.numeric(as.character(dat[i,]))~
  #                     as.numeric(as.character(time))))
  #   sl_List <- rbind(sl_List, cbind(as.numeric(b[1]),
  #                                 as.numeric(b[2])))
  # }
  #
  #
  # sl_List <- as.data.frame(cbind(seq_len(nrow(sl_List)), sl_List))
  # colnames(sl_List) <- c("sn", "intersect","slope")
  #
  # #---------------------------------------------------------------------
  # #Set the reference line
  # #if the reference is citywide average
  #
  # if(reference == 1){
  #   #calculating the citywide trend (mean)
  #   ref_slope <- mean(as.vector(sl_List$slope))
  #   ref_slope <- data.frame(cbind("City",
  #                               round(ref_slope, digits = 8)))
  #   colnames(ref_slope) <- c("gr", "slope")
  # }
  #
  # if(reference == 2){
  #   #calculating the medoid of all slopes
  #   ref_slope <- median(as.vector(sl_List$slope))
  #   ref_slope <- data.frame(cbind("City",
  #                               round(ref_slope, digits = 8)))
  #   colnames(ref_slope) <- c("gr", "slope")
  # }
  #
  # if(reference == 3){
  #   #horizontal line as the reference (slope = 0)
  #   ref_slope <- 0
  #   ref_slope <- data.frame(cbind("City",
  #                               round(ref_slope, digits = 8)))
  #   colnames(ref_slope) <- c("gr", "slope")
  # }
  #
  # #Generate the linear trendlines for all
  # #trajectories (dropping all intersects)
  #
  # dat_slopp<- NULL
  #
  # for(n in seq_len(nrow(sl_List))){ #k<-1
  #   dat_slopp <- rbind(dat_slopp, (0 + (sl_List[n,3]*(seq_len(ncol(dat))))))
  # }
  #
  # change_Stats <- NULL
  #
  # for(d_ in seq_len(length(clusters_uni))){ #d_ <- 1
  #
  #   ids_ <- which(clusters==clusters_uni[d_])
  #
  #   slope_sign_ <- NULL
  #
  #   for(v in seq_len(2)){ #v=1
  #
  #     if(v==1){
  #       all_1 <- round((length(which(dat_slopp[ids_, ncol(dat_slopp)]>
  #                         as.numeric(as.character(ref_slope$slope))))/
  #                         length(ids_))*100, digits = 1)
  #     }
  #
  #   if(v==2){
  #     all_2 <- round((length(which(dat_slopp[ids_, ncol(dat_slopp)]<
  #                         as.numeric(as.character(ref_slope$slope))))/
  #                         length(ids_))*100, digits = 1)
  #     }
  #   }
  #
  #   change_Stats  <- rbind(change_Stats ,
  #                          cbind(d_, all_1, all_2))
  #
  # }
  #
  # colnames(change_Stats) <- c("sn","%+ve Traj.","%-ve Traj.")
  # rownames(change_Stats) <- seq_len(nrow(change_Stats))
  # change_Stats <- as.data.frame(cbind(group, change_Stats))
  # attrib2 <- c("'%+ve Traj.'-> % of trajectories with positive slopes;
  #            '%+ve Traj.'-> % of trajectories with negative slopes")
  #
  # #-----------------------------------------------------------------
  # #create the 'n_quant' intervals on each side of the reference line
  # #get the medoid of each group
  #
  # gr_medoid <- NULL
  #
  # for(h_ in seq_len(length(clusters_uni))){
  # gr_medoid <- rbind(gr_medoid, cbind(clusters_uni[h_],
  # round(median(as.vector(sl_List$slope)[which(clusters==clusters_uni[h_])]),
  # digits=8)))
  # }
  #
  # gr_medoid <- data.frame(gr_medoid)
  # colnames(gr_medoid) <- c("gr","slope")
  #
  # #determine the quantile in which each medoid falls
  # #first, collate all medoids that are falling
  # #relative to the reference line
  #
  # temp_falling_ <-
  # gr_medoid[which(as.numeric(as.character(gr_medoid$slope)) <
  #                   as.numeric(as.character(ref_slope$slope))),]
  #
  # #append the reference slope
  # all_f <- c(as.numeric(as.character(temp_falling_$slope)),
  #          as.numeric(as.character(ref_slope$slope)))
  #
  # #create the intervals
  # interv_ <- as.numeric(as.character(quantile(c(min(all_f), max(all_f)),
  #                                           probs = seq(0, 1, 1/n_quant))))
  #
  # #determine where medoid of each group falls
  # int_V <- findInterval(as.numeric(as.character(temp_falling_$slope)),
  #                     interv_, all.inside = TRUE)
  #
  # intervals_fall <- (n_quant:1)[int_V]
  #
  # #first, collate all medoids that are falling relative to the reference line
  # temp_rising_ <- gr_medoid[which(as.numeric(as.character(gr_medoid$slope))>=
  #                                 as.numeric(as.character(ref_slope$slope))),]
  #
  # #append the reference slope
  # all_r <- c(as.numeric(as.character(ref_slope$slope)),
  #          as.numeric(as.character(temp_rising_$slope)))
  #
  # #create the intervals
  # interv_ <- as.numeric(as.character(quantile(c(min(all_r),
  #                                           max(all_r)) ,
  #                                           probs = seq(0, 1, 1/n_quant))))
  #
  # #determine where medoid of each group falls
  # intervals_rise <- findInterval(as.numeric(as.character(temp_rising_$slope)),
  #                              interv_, all.inside = TRUE)
  #
  # rank_positive <- c("1st (+ve)", "2nd (+ve)", "3rd (+ve)",
  #                  "4th (+ve)", "5th (+ve)", "6th (+ve)",
  #                  "7th (+ve)", "8th (+ve)", "9th (+ve)",
  #                  "10th (+ve)")
  #
  # rank_negative <- c("1st (-ve)", "2nd (-ve)", "3rd (-ve)",
  #                  "4th (-ve)", "5th (-ve)", "6th (-ve)",
  #                  "7th (-ve)", "8th (-ve)", "9th (-ve)",
  #                  "10th (-ve)")
  #
  # class1<-rank_negative[intervals_fall]
  # class2<-rank_positive[intervals_rise]
  # class <- c(class1, class2)
  # class <- data.frame(class)
  # colnames(class) <- c(paste("Qtl:","1st-",n_quant, "th", sep=""))
  #
  # if(n_quant==2){
  #   colnames(class) <- c(paste("Qtl:","1st-",n_quant, "nd", sep=""))
  # }
  #
  # if(n_quant==3){
  #   colnames(class) <- c(paste("Qtl:","1st-",n_quant, "rd", sep=""))
  # }
  #
  # change_Stats <- cbind(change_Stats, class)
  #
  # all_statistics <- list(descriptive_stats = desc_Stats,
  #                      change_stats = change_Stats)
  #
  # #-------------------
  # return(all_statistics)
}





