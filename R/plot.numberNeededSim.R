#' @title Plot of cluster groups.
#' @description Takes the 'ak_object' from the
#' \code{'akclustr'} as input and produce either the 'line' plot
#' or 'stacked' histogram.
#' @param ak_object An output of \code{\link{akclustr}} function.
#' The object contains individual trajectories and their cluster
#' solution(s) at the specified values of \code{k}. Also, includes
#' the optimal value of \code{k} based on the criterion specified.
#' at (different) values of \code{k} the \code{traj}.
#' @param ... Not used at the moment.
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @importFrom utils flush.console
#' @importFrom grDevices dev.new
#' @importFrom ggplot2 stat_summary scale_colour_brewer theme_light
#' @export
plot.numberNeededSim <- function(ak_object, ...){

  k <- 3
  reference <- 1
  n_quant <- 4
  type <- "lines"
  y_scaling <- "fixed"

  #first testing that correct values of k is specified.
  #get all values of k..
  all_K <- as.vector(unlist(lapply(ak_object$solutions, attributes)))

  if(!k %in% all_K){
    stop(paste("*----k =", k, "is not applicable!. Print the",
               "'akobject' to see allowed k-values----*", sep=" "))
  }

  # check object type
  # if(class(ak_object)[1] != "akobject"){
  #   stop("*----Object not right type!! 'akclustr' object required!----*")
  # }

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
  ggplot <- aes <- Year <- value <- id <- geom_line <- facet_wrap <-
    geom_smooth <- theme_minimal <- variable <- group <- NULL

  #plot option 1:
  if(type=="lines"){
    if(y_scaling=="fixed"){
      ggplot(data.subset.melted, aes(x=Year, y=value,
                                             group=id, color=clusters)) +
                geom_line() +
                stat_summary(fun.y=mean, geom="line", aes(group=clusters),
                             color="black", size=1) +
                facet_wrap(~clusters, scales = "fixed") +
                facet_wrap(~clusters) +
                scale_colour_brewer(palette = "Set1") #clusters
    }

    if(y_scaling=="free"){
     ggplot(data.subset.melted, aes(x=Year, y=value,
                                             group=id, color=clusters)) +
                geom_line() +
                stat_summary(fun.y=mean, geom="line", aes(group=clusters),
                             color="black", size=1) +
                facet_wrap(~clusters, scales = "free") +
                facet_wrap(~clusters) +
                scale_colour_brewer(palette = "Set1") +
                theme_light() #clusters
    }
  }

  #----------------------------------------------------
  #plot option 2:
  if(type=="stacked"){
    change_ave_yr_ALL_transpose <- t(change_ave_yr_ALL)
    grp.dat<-data.frame(change_ave_yr_ALL_transpose,
                        row.names=seq_len(nrow(change_ave_yr_ALL_transpose)))
    names(grp.dat)<-clusters_uni
    p.dat<-data.frame(Year=row.names(grp.dat),grp.dat,stringsAsFactors=F)
    p.dat<-melt(p.dat,id='Year')
    p.dat$Year<-as.numeric(p.dat$Year) #head(p.dat)
    class(p.dat$Year)

    ggplot(p.dat,aes(x=Year,y=value)) + theme(legend.position="none")+
              geom_area(aes(fill=variable), colour = "gray30", position='fill') +
              scale_x_continuous(breaks=seq_len(nrow(change_ave_yr_ALL_transpose)),
                                 labels=Year)+
              scale_fill_brewer(palette = "Set1") +
              theme_light()
  }

  #all_plots <- list(cluster_plot = plt)

  #print(plt)
  #-------------------
  #return(all_plots)
}



# @title this
# @description this
# @param x this is
# @param ... Not used at the moment.
# @export
# @importFrom stats runif
# @importFrom ggplot2 geom_bar labs scale_x_discrete
# @export
#plot.numNeededSims <- function(x, ...) {
#   if (!"package:ggplot2" %in% search()) {
#     return(cat("Need to load package ggplot2 in order to plot."))
#   }
#
#   sims <- x$sims
#   # for a good bar-plot, convert numerical vector sims
#   # to a factor with appropriate levels
#   levels <- min(sims):max(sims)
#   sims <- factor(sims, levels = levels)
#
#   df <- data.frame(sims)
#   plotTitle <- paste0("Results of ", length(sims), " Simulations")
#   # in the code below, scale_x_discrete(drop = f) ensures that
#   # even if there are no values in sims for a particular level it
#   # will still appear in the plot as a zero-height bar
#   ggplot(df, aes(x = sims)) + geom_bar() + scale_x_discrete(drop = FALSE) +
#     labs(x = "Number Needed", title = plotTitle)
# }



