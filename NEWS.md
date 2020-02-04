---
title: "NEWS.md"
authors: "geoMADE"
date: "9 January 2020"
output: html_document
---


#Issues addressed in previous submission
1. Non-standard files ('paper.md', 'paper.bib', 'cran-comments', 'LICENSE') removed from main package directory before compilation.
2. Author's name ("Rousseeuw"") is spelt correctly. In other words, there is no spellin errors
3. Invalid URL removed 
4. keyword entries removed altogether 


'Akmedoids' package updated (Version: v0.1.5)
### Updates:

1. Added arguments 'digits' and 'scale' to the 'props' function. 
2. Added argument 'crit' to the 'akmedoids.clust' function
3. Added new function 'elbowPoint' for determining the elbow point along a curve

Your faithfully.
Monsuru.


#--------------------------------------
#New update for akmedoids v.0.1.6
#--------------------------------------
#additional function ()
#--------------------------------------------------------------------------------

#update 1: New: showplots parameter for 'statPrints'

#update 2: Function to remove any rows where NA and infinity exists
removeMissingValues = function(x=crime_rate, id_field=TRUE){
  id_Toremove = NULL
  if(id_field==TRUE){
    for(h in 2:ncol(x)){ #h=1
      id_Toremove=c(id_Toremove, which(is.infinite(as.numeric(as.character(x[,h])))))
      id_Toremove=c(id_Toremove, which(is.na(as.numeric(as.character(x[,h])))))
    }
  }
  if(id_field==FALSE){
    for(h in 1:ncol(x)){ #h=1
      id_Toremove=c(id_Toremove, which(is.infinite(as.numeric(as.character(x[,h])))))
      id_Toremove=c(id_Toremove, which(is.na(as.numeric(as.character(x[,h])))))
    }
  }
  
  x = x[-unique(id_Toremove), ]
  flush.console()
  print(paste("Message:", length(unique(id_Toremove)), "row(s) are removed!", sep=" "))
  return(x)
}
