---
title: Akmedoids R package for generating directionally-homogeneous clusters of longitudinal data sets
aas-journal: The Journal of Open Source Software
date: "09 January 2020"
bibliography: paper.bib
affiliations:
- index: 1
  name: Crime and Well-being Big Data Centre, Manchester Metropolitan University
authors:
- affiliation: 1
  name: Monsuru Adepeju
  orcid: 0000-0002-9006-4934
- affiliation: 1
  name: Sam Langton
  orcid: 0000-0002-1322-1553
- affiliation: 1
  name: Jon Bannister
  orcid: 0000-0002-1350-510X
tags:
- Anchored k-medoids
- k-means
- crime
- longitudinal clustering
- long-term trends
aas-doi: 10.3847/xxxxx
---

# Summary

In social and behavioural sciences, longitudinal clustering is widely used for identifying groups of individual trends that correspond to certain developmental processes over time. Whilst popular clustering techniques, such as k-means, are suited for identifying spherical clusters [@GenoFali:2010; @Curman:2015], there has been little attempt to modify such methods to identify alternative forms of cluster, such as those that represent linear growth over time (i.e. directionally-homogeneous clusters). To address this shortcoming, we introduce `Anchored k-medoids`, a package referred to as `Ak-medoids`, which implements a medoid-based expectation maximisation (MEM) procedure within a classical k-means clustering framework.  The package includes functions to assist in the manipulation of longitudinal data sets prior to the clustering procedure, and the visualisation of solutions post-procedure. The potential application areas of `Ak-medoids` include criminology, transport, epidemiology and brain imaging.

# Design and implementation

Previous studies have taken advantage of the various functional characteristics of longitudinal data in order to extract theoretically or empirically interesting clusters of subjects. Examples include using the Fourier basis [@Tarpey:2003] or the coefficients of the B-spline derivative estimates [@Boor:1978; @Schumaker:2007] which anchor clustering routines to better capture a presumed developmental process. Here, we develop an `Anchored k-medoids` (`Akmedoids`) clustering package, which employs the ordinary least square (OLS) trend lines of subjects, and a bespoke expectation-maximisation procedure, specifically to capture long-term linear growth. In criminology, identifying such slow-changing trends helps to unravel place-based characteristics that drive crime-related events, such as street gun and homicide, across a geographical space [@Griffith:2004]. To date, explorations of these trends have deployed existing techniques, namely k-means [@Curman:2015; @Andresen:2017] and group-based trajectory modelling [@Weisburd:2004; @Chavez:2009; @Bannister:2017], which are suited for spherical clusters [@GenoFali:2010]. The sensitivity of such techniques to short-term fluctuations and outliers in longitudinal datasets makes it more difficult to extract clusters based on the underlying long-term trends. `Akmdeoids` is tailored for such a scenario.
The main clustering function in the `Akmedoids` package  implements a medoid-based expectation maximisation (MEM) procedure by integrating certain key modifications into the classical k-means routine. First, it approximates longitudinal trajectories using OLS regression and second, anchors the initialisation process with medoid observations. It then deploys the medoid observations as new anchors for each iteration of the expectation-maximisation procedure [@Celeux:1992], until convergence. In a similar fashion to classical k-means, the routine relies on distance-based similarity between vectors of observations and is scale invariant. This implementation ensures that the impact of short-term fluctuations and outliers are minimised. The final groupings are augmented with the raw trajectories, and visualised, in order to provide a clearer delineation of the long-term linear trends of subject trajectories. Given an `l` number of iterations, the computational complexity of the clustering routine is the same as that of a classical k-means algorithm, i.e. `O(lkn)`, where `k` is the specified number of clusters and `n`, the number of individual trajectories. The optimal number of clusters for a given data may be determined using the average silhouette [@Rousseeuw:1987] or the Calinski and Harabasz criterion [@Calinski:1974]. A full demonstration is provided in the package vignette of how to deploy `Akmedoids` to examine long-term relative exposure to crime in `R` [@rManual:2020]. We encourage the use of the package outside of criminology.

# Clustering and cluster representations

The main clustering function of akmedoids is `akmedoids.clust`. The function captures directionally homogeneous clusters within any given longitudinal dataset using the procedure detailed above. For crime inequality studies, the package includes the `props` function for converting the absolute (or rate) measures of individual trajectories into a relative measure over time. The `statPrint` function draws from the `ggplot2` library [@ggplot:2016] in order to visualise the resulting clusters in either a line or an areal-stacked graph format, alongside descriptive cluster statistics.

# Acknowledgment

We gratefully acknowledge the Economic and Social Research Council (ESRC), who funded the Understanding Inequalities project (Grant Reference ES/P009301/1) through which this research was conducted.

# References

