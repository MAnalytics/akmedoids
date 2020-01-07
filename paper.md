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
    orcid: 0000-0002-1350-510X
    affiliation: 1
affiliations:
 - name: Big Data Centre, Manchester Metropolitan University, Manchester, M15 6BH
   index: 1
date: 7 January 2020
bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

# Summary
In social and behavioural sciences, longitudinal clustering is widely used for identifying groups of individual trends that correspond to certain developmental processes over time. Whilst popular clustering techniques, such as k-means and group-based trajectory modelling (GBTM), are suited for identifying spherical clusters [@GenoFali:2010; @Curman:2015],  their malleability provides opportunity for identifying alternative forms of clusters, including those that represent linear growth over time (i.e. directionally-homogeneous clusters). Here, we introduce `Anchored k-medoids`, a package referred to as `Ak-medoids`, which implements a medoid-based expectation maximisation (MEM) procedure within a classical k-mean clustering routine.  The package includes functions to assist in the manipulation of longitudinal data sets prior to the clustering procedure, and the visualisation of solutions post-procedure. The potential application areas of `Ak-medoids` include criminology, transport, epidemiology and brain imaging.

# References

