## 'akmedoids' package

An R package for analysing and clustering longitudinal data

### Description

The `akmedoids` package advances a set of R-functions for longitudinal clustering of long-term trajectories and determines the optimal solution based on either the `average silhouette width` (Rousseeuw P. J. (1987) or the `Caliński-Harabatz` criterion (Caliński and Harabasz 1974). The package also includes a set of functions for addressing common data issues, such as missing entries and outliers, prior to conducting advance longitudinal data analysis. One of the key objectives of this package is to facilitate easy replication of a recent paper which examined small area inequality in the crime drop (Adepeju et al. 2020). Many of the functions provided in the `akmedoids` package may be applied to longitudinal data in general. 

**For more information and usability, check out details on [CRAN](https://cran.r-project.org/web/packages/akmedoids/index.html).**

### Getting Started 

To install the development version of the package:

```R
remotes::install_github("MAnalytics/akmedoids")
#please report any installation problems in the issues
```
Alternatively, from an R console, type `install.packages("akmedoids")` to install the package. After download is complete type `library("akmdeoids")` to load the package.

### Example usage:

Given a longitudinal datasets, the following is an example of how `akmedoids` could be used to extract clusters of trajectories with similar long-term trends over time. 

```R
#generate some datasets

#A group of escalating trajectories
gr1 =  

#A group of stable trajectories
gr2 = 

#A group of decreasing trajectories

#Combine groups
dat = rbind(gr1, gr2, gr3)

#run clustering function


#get group attributes

stat

```

### Documentation

From an R console type `??akmedoids` for help on the package. The package page on CRAN is [here](https://cran.r-project.org/web/packages/akmedoids/index.html), package reference manual is [here](https://cran.r-project.org/web/packages/akmedoids/akmedoids.pdf), package vignette is [here](https://cran.r-project.org/web/packages/akmedoids/vignettes/akmedoids-vignette.html). 

Support and Contributions:

For support and bug reports send an email to: monsuur2010@yahoo.com or open an issue [here](https://github.com/MAnalytics/akmedoids/issues). Code contributions to akmedoids are also very welcome.

### References:

Rousseeuw, P. J. 1987. “Silhouettes: A Graphical Aid to the Interpretation and Validation of Cluster Analysis.” Journal of Computational and Applied Mathematics, no. 20: 53–6. [link](https://www.bibsonomy.org/bibtex/bc0f62c7895f91c787354d03f23da976)

Caliński, T., and J. Harabasz. 1974. “A Dendrite Method for Cluster Analysis.” Communications in Statistics-Theory and Methods, 3(1): 1–27. [link](https://www.tandfonline.com/doi/abs/10.1080/03610927408827101)

Adepeju, M., Langton, S. and Bannister, J. 2020. Anchored k-medoids: a novel adaptation of k-means further refined to measure instability in the exposure to crime. Journal of Computational Social Science, (under review).

