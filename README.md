## 'akmedoids' package

An R package for analyzing and clustering longitudinal data

### Description

The `akmedoids` package advances the clustering of longitudinal datasets in order to identify clusters of trajectories with similar long-term linear trends over time, providing an improved cluster identification as compared with the classic kmeans algorithm. The package also includes a set of functions for addressing common data issues, such as missing entries and outliers, prior to conducting advance longitudinal data analysis. One of the key objectives of this package is to facilitate easy replication of a recent paper which examined small area inequality in the crime drop (Adepeju et al. 2020). Many of the functions provided in the `akmedoids` package may be applied to longitudinal data in general. 

**For more information and usability, check out details on [CRAN](https://cran.r-project.org/web/packages/akmedoids/index.html).**

### Installation 

```R
#installation from `CRAN`
#From an R console, type:
install.packages("akmedoids")
library(akmedoids)
```
To install the development version of the package, type `remotes::install_github("MAnalytics/akmedoids")`. Please, report any installation problems in the issues.

### Example usage:

Given a longitudinal datasets, the following is an example of how `akmedoids` could be used to extract clusters of trajectories with similar long-term trends over time. 

```R
#simulate some datasets

#function for creating longitudinal noise
noise_fn = function(x=3, time){
  rnorm(length(time), mean=0, x)}

#function for simulating a trajectory group
sim_group <- function(gr_baseline, sd, time){
  intcp_errors <- rgamma(1, shape=2, scale=sd) #intercept error
  mean_traj = gr_baseline + intcp_errors
  traj = mean_traj + noise_fn(intcp_errors, time)
}

#time steps
t_steps = c(0:20)

#increasing group
i_gr = NULL
for(i in 1:50){
i_gr = rbind(i_gr,
             sim_group(gr_baseline=(0.5*t_steps),
                       sd=1, time=t_steps))
}

#stable group
s_gr = NULL
for(i in 1:50){
  s_gr = rbind(s_gr,
               sim_group(gr_baseline=rep(3,length(t_steps)),
                         sd=1, time=t_steps))
}

#decreasing group
d_gr = NULL
for(i in 1:50){
  d_gr = rbind(d_gr,
               sim_group(gr_baseline=(10 - (0.5*t_steps)),
                         sd=1, time=t_steps))
}

#combine groups
all_traj = data.frame(rbind(i_gr, s_gr, d_gr))

#load library
library(akmedoids)

#identify optimal cluster solution
akmedoids.clust(all_traj, k=c(3,10), crit = "Silhouette")

#Optimal number of clusters, k
#---------#
# k = 3   #
#---------#

#get the optimal cluster solution
clustr = akmedoids.clust(all_traj, k=3)
clustr = clustr$memberships

##get group statistics
descrStats = statPrint(clustr, all_traj,
                       id_field = FALSE, showplots=FALSE)

print(descrStats)

```

### Documentation

From an R console type `??akmedoids` for help on the package. The package page on CRAN is [here](https://cran.r-project.org/web/packages/akmedoids/index.html), package reference manual is [here](https://cran.r-project.org/web/packages/akmedoids/akmedoids.pdf), package vignette is [here](https://cran.r-project.org/web/packages/akmedoids/vignettes/akmedoids-vignette.html). 

### Support and Contributions:

For support and bug reports send an email to: monsuur2010@yahoo.com or open an issue [here](https://github.com/MAnalytics/akmedoids/issues). Code contributions to akmedoids are also very welcome.

### References:

Rousseeuw, P. J. 1987. “Silhouettes: A Graphical Aid to the Interpretation and Validation of Cluster Analysis.” Journal of Computational and Applied Mathematics, no. 20: 53–6. [link](https://www.bibsonomy.org/bibtex/bc0f62c7895f91c787354d03f23da976)

Caliński, T., and J. Harabasz. 1974. “A Dendrite Method for Cluster Analysis.” Communications in Statistics-Theory and Methods, 3(1): 1–27. [link](https://www.tandfonline.com/doi/abs/10.1080/03610927408827101)

Adepeju, M., Langton, S. and Bannister, J. 2020. Anchored k-medoids: a novel adaptation of k-means further refined to measure instability in the exposure to crime. Journal of Computational Social Science, (under review).

