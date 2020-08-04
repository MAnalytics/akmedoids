
#' @title Descriptive (Change) statistics and plots
#' @description This function perform two tasks: (i) it generate the descriptive and change statistics of groups, particularly suited for the outputs form the \code{\link{akmedoids.clust}} function, and (ii) generates the plots of the groups (performances).
#' @param clustr [vector (character)] A vector of cluster membership (labels). For instance, the result extracted from the \code{\link{akmedoids.clust}} function.
#' @param traj [matrix (numeric)]: corresponding longitudinal data used to generate \code{clustr} (with rows corresponding to each label of \code{clustr}). For example, the first label of \code{clustr} is the group label of the first row of \code{traj} matrix, and so on.
#' @param id_field [numeric or character] Whether the first column of the \code{traj} is a unique (\code{id}) field. Default: \code{FALSE}. If \code{TRUE} the function recognises the second column as the first time step.
#' @param type [character] plot type. Available options are: \code{"lines"} and \code{"stacked"}.
#' @param y.scaling [character] works only if \code{type="lines"}. \code{y.scaling} set the vertical scales of the cluster panels. Options are: \code{"fixed"}: uses uniform scale for all panels, \code{"free"}: uses variable scales for panels.
#' @param reference [numeric] Specifying the reference line from which the direction of each group is measured. Options are: \code{1}: slope of mean trajectory, \code{2}: slope of medoid trajectory, \code{3}: slope of a horizontal line (i.e. slope = 0). Default: \code{1}.
#' @param N.quant [numeric] Number of equal intervals (quantiles) to create between the reference line \code{(R)} and the medoids \code{(M)} of the most-diverging groups of both sides of \code{(R)}. Default is \code{4} - meaning quartile subdivisions on each side of \code{(R)}. In this scenario, the function returns the quartile in which the medoid of each group falls. This result can be used to further categorize the groups into 'classes'. For example, groups that fall within the \code{1st} quartile may be classified as 'Stable' groups (Adepeju et al. 2019).
#' @param showplots [TRUE or FALSE] To display cluster plots. Defaults \code{TRUE}
#' @examples
#' print(traj)
#' traj <- dataImputation(traj, id_field = TRUE, method = 1, replace_with = 1,
#' fill_zeros = FALSE)
#' print(traj)
#' traj <- props(traj, id_field = TRUE)
#' clustr <- akmedoids.clust(traj, id_field = TRUE, method = "linear", k = 5)
#' clustr <- as.vector(clustr$memberships)
#' print(statPrint(clustr, traj, id_field=TRUE, type="lines", y.scaling="fixed"))
#' print(statPrint(clustr, traj, id_field=TRUE, reference = 1, N.quant = 8, type="stacked"))
#' @details Generates the descriptive and change statistics of the trajectory groupings. Given a vector of group membership (labels) and the corresponding data matrix (or data.frame) indexed in the same order, this function generates all the descriptive and change statistics of all the groups.
#' The function can generate a line and an area stacked plot drawing from the functionalities of the \code{ggplot2} library. For a more customized visualisation, we recommend that users deploy \code{ggplot2} directly (\code{Wickham H. (2016)}).
#' @return A plot showing group membership or sizes (proportion) and statistics.
#' @references \code{1}. Adepeju, M. et al. (2019). Anchored k-medoids: A novel adaptation of k-means further refined to measure inequality in the exposure to crime across micro places (Submitted).
#' @rawNamespace import(reshape2, ggplot2, stats)
#' @references \code{Wickham H. (2016). Elegant graphics for Data Analysis. Spring-Verlag New York (2016)}
#' @export

statPrint <- function(clustr, traj, id_field=TRUE,
                      reference = 1, N.quant = 4,
                      showplots=TRUE, type = "lines",
                      y.scaling="fixed"){

#joining the data with the clusters
clustr <- data.frame(cbind(traj, clusters=clustr))

dat <- traj #backing up the data
N.quant <- round(N.quant, digits = 0)

if(N.quant < 2 | N.quant > 10){
  stop("*-------Please, enter an integer between 2 and 10 for the 'N.quant' argument'!!!-------*")
}

#check if there is id_field
#check if id field  is unique
if(id_field==TRUE){
  dat <- dat[,2:ncol(dat)]
  n_CL <- colnames(clustr)[1]
  col_names <- as.vector(clustr[,1])
  if(!length(col_names)==length(unique(col_names))){
    stop("(: The 'id_field' is not a unique field. Function terminated!!! :)")
  }
}

if(id_field==FALSE){
  clustr <- cbind(1:nrow(clustr), clustr)
}

#cluster list
clusters <- as.vector(clustr[,ncol(clustr)])
data_subset <- clustr[,1:(ncol(clustr)-1)]
data_subset <- as.data.frame(data_subset)
colnames(data_subset) <- c("code", 1:(ncol(data_subset)-1))
data.subset.melted <- suppressWarnings(melt(data_subset, id="code"))
#append cluster list
data.subset.melted <- cbind(data.subset.melted,
                            rep(clusters, ncol(data_subset)-1))
colnames(data.subset.melted) <- c("id","Year","value", "clusters")

#----------------------------------------------------
#preparing the data to generate descriptive statitics
year_uni <- as.vector(unique(data.subset.melted$Year))
order_Cluster <- as.vector(unique(data.subset.melted$clusters))
clusters_uni <- order_Cluster[order(as.vector(unique(data.subset.melted$clusters)))]

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
if(showplots==TRUE){
#----------------------------------------------------
#plotting
#----------------------------------------------------
ggplot <- aes <- Year <- value <- id <- geom_line <- facet_wrap <-
  geom_smooth <- theme_minimal <-
  variable <- NULL

#plot option 1:
if(type=="lines"){
if(y.scaling=="fixed"){
  p <- (ggplot(data.subset.melted, aes(x=Year, y=value,
                                            group=id, color=clusters)) +
                geom_line() +
                stat_summary(fun.y=mean, geom="line", aes(group=clusters),
                             color="black", size=1) +
                facet_wrap(~clusters, scales = "fixed") +
                facet_wrap(~clusters) +
                scale_colour_brewer(palette = "Set1")) #clusters
  }

if(y.scaling=="free"){
  p <- (ggplot(data.subset.melted, aes(x=Year, y=value,
                                         group=id, color=clusters)) +
            geom_line() +
            stat_summary(fun.y=mean, geom="line", aes(group=clusters),
                         color="black", size=1) +
            facet_wrap(~clusters, scales = "free") +
            facet_wrap(~clusters) +
            scale_colour_brewer(palette = "Set1") +
            theme_light()) #clusters
  }
}

#----------------------------------------------------
#plot option 2:
if(type=="stacked"){
  change_ave_yr_ALL_transpose <- t(change_ave_yr_ALL)
  grp.dat<-data.frame(change_ave_yr_ALL_transpose,
                      row.names=1:nrow(change_ave_yr_ALL_transpose))
  names(grp.dat)<-clusters_uni
  p.dat<-data.frame(Year=row.names(grp.dat),grp.dat,stringsAsFactors=F)
  p.dat<-melt(p.dat,id='Year')
  p.dat$Year<-as.numeric(p.dat$Year) #head(p.dat)
  class(p.dat$Year)

  p <- (ggplot(p.dat,aes(x=Year,y=value)) + theme(legend.position="none") +
  geom_area(aes(fill=variable), colour = "gray30", position='fill') +
  scale_x_continuous(breaks=seq_len(nrow(change_ave_yr_ALL_transpose)), labels=Year) +
  scale_fill_brewer(palette = "Set1") +
  theme_light())
}

#plot
flush.console()
dev.new(width=3, height=3)
print(p)
}

#not show plot
if(showplots==FALSE){
 #Do nothing
}

#----------------------------------------------------
#To generate descriptive statistics
all_statistics <- list()

#variable for change statistics
desc_Stats <- NULL
ll_ <- clusters_uni
group <- clusters_uni

for(n in seq_len(length(ll_))){
  #Calculating the number of trajectories
  a1 <- length(which(clusters%in%ll_[n]))
  a2 <- round((length(which(clusters%in%ll_[n]))/
                 length(clusters))*100,digits = 1)
  a3 <- round((change_ave_yr_ALL[n,1] /
                 sum(change_ave_yr_ALL[,1]))*100, digits = 1)
  a4 <- round((change_ave_yr_ALL[n,ncol(change_ave_yr_ALL)]/
                 sum(change_ave_yr_ALL[,ncol(change_ave_yr_ALL)]))*100, digits = 1)
  a5 <- round((a4-a3), digits=1)
  a6 <- round(((a4-a3)/a4)*100, digits=1)
  desc_Stats <-  rbind(desc_Stats, cbind(a1, a2, a3, a4, a5, a6))
}
  colnames(desc_Stats) <- c("n","n(%)","%Prop.time1",
                            "%Prop.timeT", "Change", "%Change")
  rownames(desc_Stats) <- 1:nrow(desc_Stats)
  desc_Stats <- as.data.frame(cbind(group, desc_Stats))
  attrib1 <- c("'n'->size (number of traj.); 'n(%)'->%size;
               '%Prop.time1'->% proportion of obs. at time 1;
               '%Prop.timeT'-> % proportion of obs. at time T;
               'Change'-> absolute change in proportion between time1 and timeT;
               '%Change'-> % change in proportion between time 1 and timeT")

#----------------------------------------------------
#To generate slope statistics
#required
sl_List <- NULL
time <- as.numeric(seq_len(ncol(dat)))
for(i in seq_len(nrow(dat))){ #i<-1
  b=coefficients(lm(as.numeric(as.character(dat[i,]))~
                      as.numeric(as.character(time))))
  sl_List <- rbind(sl_List, cbind(as.numeric(b[1]),
                                  as.numeric(b[2])))
}

sl_List <- as.data.frame(cbind(seq_len(nrow(sl_List)), sl_List))
colnames(sl_List) <- c("sn", "intersect","slope")

#---------------------------------------------------------------------
#Set the reference line
#if the reference is citywide average
if(reference == 1){
  #calculating the citywide trend (mean)
  ref_slope <- mean(as.vector(sl_List$slope))
  ref_slope <- data.frame(cbind("City",
                                round(ref_slope, digits = 8)))
  colnames(ref_slope) <- c("gr", "slope")
}

if(reference == 2){
  #calculating the medoid of all slopes
  ref_slope <- median(as.vector(sl_List$slope))
  ref_slope <- data.frame(cbind("City",
                                round(ref_slope, digits = 8)))
  colnames(ref_slope) <- c("gr", "slope")
}

if(reference == 3){
  #horizontal line as the reference (slope = 0)
  ref_slope <- 0
  ref_slope <- data.frame(cbind("City",
                                round(ref_slope, digits = 8)))
  colnames(ref_slope) <- c("gr", "slope")
}

#Generate the linear trendlines for all
#trajectories (dropping all intersects)
dat_slopp<- NULL
for(n in seq_len(nrow(sl_List))){ #k<-1
  dat_slopp <- rbind(dat_slopp, (0 + (sl_List[n,3]*(1:ncol(dat)))))
}

change_Stats <- NULL
for(d_ in seq_len(length(clusters_uni))){ #d_ <- 1
  ids_ <- which(clusters==clusters_uni[d_])
  slope_sign_ <- NULL
  for(v in 1:2){ #v=1
    if(v==1){
      all_1 <- round((length(which(dat_slopp[ids_, ncol(dat_slopp)]>
                                       as.numeric(as.character(ref_slope$slope))))/
                                        length(ids_))*100, digits = 1)
      }
    if(v==2){
      all_2 <- round((length(which(dat_slopp[ids_, ncol(dat_slopp)]<
                                       as.numeric(as.character(ref_slope$slope))))/
                                        length(ids_))*100, digits = 1)
      }
    }
    change_Stats  <- rbind(change_Stats ,
                           cbind(d_, all_1, all_2))
}
colnames(change_Stats) <- c("sn","%+ve Traj.","%-ve Traj.")
rownames(change_Stats) <- 1:nrow(change_Stats)
change_Stats <- as.data.frame(cbind(group, change_Stats))
attrib2 <- c("'%+ve Traj.'-> % of trajectories with positive slopes;
             '%+ve Traj.'-> % of trajectories with negative slopes")

#-----------------------------------------------------------------
#create the 'N.quant' intervals on each side of the reference line
#get the medoid of each group
gr_medoid <- NULL
for(h_ in seq_len(length(clusters_uni))){
  gr_medoid <- rbind(gr_medoid, cbind(clusters_uni[h_],
                          round(median(as.vector(sl_List$slope)[which(clusters==clusters_uni[h_])]),
                                digits=8)))
}
gr_medoid <- data.frame(gr_medoid)
colnames(gr_medoid) <- c("gr","slope")

#determine the quantile in which each medoid falls
#first, collate all medoids that are falling
#relative to the reference line
temp_falling_ <-
  gr_medoid[which(as.numeric(as.character(gr_medoid$slope)) <
                    as.numeric(as.character(ref_slope$slope))),]
#append the reference slope
all_f <- c(as.numeric(as.character(temp_falling_$slope)),
           as.numeric(as.character(ref_slope$slope)))
#create the intervals
interv_ <- as.numeric(as.character(quantile(c(min(all_f), max(all_f)),
                                            probs = seq(0, 1, 1/N.quant))))
#determine where medoid of each group falls
int_V <- findInterval(as.numeric(as.character(temp_falling_$slope)),
                      interv_, all.inside = TRUE)
intervals_fall <- (N.quant:1)[int_V]

#first, collate all medoids that are falling relative to the reference line
temp_rising_ <- gr_medoid[which(as.numeric(as.character(gr_medoid$slope))>=
                                  as.numeric(as.character(ref_slope$slope))),]
#append the reference slope
all_r <- c(as.numeric(as.character(ref_slope$slope)),
           as.numeric(as.character(temp_rising_$slope)))
#create the intervals
interv_ <- as.numeric(as.character(quantile(c(min(all_r),
                                            max(all_r)) ,
                                            probs = seq(0, 1, 1/N.quant))))
#determine where medoid of each group falls
intervals_rise <- findInterval(as.numeric(as.character(temp_rising_$slope)),
                               interv_, all.inside = TRUE)

rank_positive <- c("1st (+ve)", "2nd (+ve)", "3rd (+ve)",
                   "4th (+ve)", "5th (+ve)", "6th (+ve)",
                   "7th (+ve)", "8th (+ve)", "9th (+ve)",
                   "10th (+ve)")
rank_negative <- c("1st (-ve)", "2nd (-ve)", "3rd (-ve)",
                   "4th (-ve)", "5th (-ve)", "6th (-ve)",
                   "7th (-ve)", "8th (-ve)", "9th (-ve)",
                   "10th (-ve)")

class1<-rank_negative[intervals_fall]
  class2<-rank_positive[intervals_rise]
  class <- c(class1, class2)
  class <- data.frame(class)
  colnames(class) <- c(paste("Qtl:","1st-",N.quant, "th", sep=""))
  if(N.quant==2){
  colnames(class) <- c(paste("Qtl:","1st-",N.quant, "nd", sep=""))
  }
  if(N.quant==3){
  colnames(class) <- c(paste("Qtl:","1st-",N.quant, "rd", sep=""))
  }

change_Stats <- cbind(change_Stats, class)

all_statistics <- list(descriptiveStats = desc_Stats,
                       changeStats = change_Stats)
#-------------------
return(all_statistics)
}





