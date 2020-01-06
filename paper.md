---
title: 'Akmedoids R package for generating directionally-homogeneous clusters of longitudinal data sets'
tags:
  - Anchored k-medoids
  - k-means
  - crime
  - longitudinal clustering
  - long-term trends
authors:
  - name: Monsuru Adepeju
    orcid: 0000-0002-9006-4934
    affiliation: 1
  - name: Sam Langton
    orcid: 0000-0002-1322-1553
    affiliation: 1
  - name: Jon Bannister
    orcid: null
    affiliation: 1
affiliations:
  - index: 1
    name: Big Data Centre, Manchester Metropolitan University, Manchester, M15 6BH
date: 6 January 2020
bibliography: paper.bib
---

# Summary
In social and behavioural sciences, longitudinal clustering is widely used for identifying groups of individual trends that correspond to certain developmental processes over time. Whilst popular clustering techniques, such as k-means and group-based trajectory modelling (GBTM), are suited for identifying spherical clusters [@GenoFali:2010; @Curman:2015],  their malleability provides opportunity for identifying alternative forms of clusters, including those that represent linear growth over time (i.e. directionally-homogeneous clusters). Here, we introduce `Anchored k-medoids`, a package referred to as `Ak-medoids`, which implements a medoid-based expectation maximisation (MEM) procedure within a classical k-mean clustering routine.  The package includes functions to assist in the manipulation of longitudinal data sets prior to the clustering procedure, and the visualisation of solutions post-procedure. The potential application areas of `Ak-medoids` include criminology, transport, epidemiology and brain imaging.

[Source Code:](https://github.com/MAnalytics/akmedoids)
[Information:](https://cran.r-project.org/web/akmedoids/index.html)

# Design and implementation
Previous studies have taken advantage of the various functional characteristics of longitudinal data in order to extract theoretically or empirically interesting clusters of subjects. Examples include using the Fourier basis [@Tarpey:2003] or the coefficients of the B-spline derivative estimates [@Boor:1978; @Schumaker:2007] in order to anchor the clustering routines. Here, we develop an `Anchored k-medoids` (`Akmedoids`) clustering package which employs the ordinary least square (OLS) trend lines of subjects, and a bespoke expectation-maximisation procedure, to capture long-term linear growth. In criminology, identifying such slow-changing trends helps to unravel place-based characteristics that drive crime-related events, such as street and gang violence, across a geographical space [@Griffith:2004]. To date, such studies have deployed existing techniques, namely k-means [@Curman:2015; @Andresen:2017] and GBTM [@Weisburd:2004; @Chavez:2009; @Bannister:2017], which are more suited for spherical clusters [@GenoFali:2010]. Moreover, the sensitivity of these techniques to short-term fluctuations and outliers in longitudinal datasets makes it more difficult to extract clusters based on the underlying long-term trend over time. 
The main clustering function in the `Akmedoids` package  implements a medoid-based expectation maximisation (MEM) procedure by integrating certain key modifications into the classical k-means routine. First, it approximates longitudinal trajectories using OLS regression and second, anchors the initialisation process with medoid observations. It then deploys the medoid observations as new anchors for each iteration of the expectation-maximisation procedure [@Celeux:1992], until convergence. In similar fashion as classical k-means, the routine relies on distance-based similarity between vectors of observations and is scale invariant. This implementation ensures that the impact of short-term fluctuations and outliers are minimised. The final groupings are augmented with the raw trajectories, and visualised, in order to provide a clearer delineation of the long-term linear trends of trajectories. Given an `l` number of iterations, the computational complexity of the clustering routine is the same as that of a classical k-means algorithm, i.e. `O(lkn)`, where `k` is the specified number of clusters and `n`, the number of individual trajectories. The optimal number of clusters for a given data may be determined using the average silhouette [@Rousseeuw:1987] or the Calinski and Harabasz criterion [@Calinski:1974] or. A full demonstration is provided in the package vignette of how to deploy `Akmedoids` to examine long-term relative exposure to crime. We encourage the use of the package outside of criminology, should it be appropriate.
# Clustering and cluster representations
The main clustering function of akmedoids is the `akmedoids.clust`. The function captures directionally homogeneous clusters within any given longitudinal dataset using the procedure detailed above. For crime inequality studies, the package includes the `props` function for converting the absolute (or rate) measures of individual trajectories into relative measure over time. The `statPrint` function draws from the `ggplot2` library [@ggplot2:2016] in order to visualise the resulting clusters in either a line or an areal-stacked graph format, alongside descriptive cluster statistics.

# Acknowledgment
We gratefully acknowledge the Economic and Social Research Council (ESRC), who funded the Understanding Inequalities project (Grant Reference ES/P009301/1) through which this research was conducted.
# References
