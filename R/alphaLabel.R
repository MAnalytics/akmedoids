#' @title Numerics ids to alphabetical ids
#' @description Function to transform a list of numeric ids to alphabetic ids
#' @param x A vector of numeric ids
#' @usage alphaLabel(x)
#' @details Given a vector of numeric cluster ids, `alphaLabel` converts each id to its corresponding alphabets. It combines alphabets for ids greater than 26.
#' @return A vector of alphabetical ids.
#' @examples
#' ids <- sample(1:100, 10, replace=FALSE)
#' ids_alphab <- alphaLabel(ids)
#' @rawNamespace import(utils)
#' @export

alphaLabel <- function(x){

  combind_A <- LETTERS
  combind <-  combn(LETTERS, m=2, sep="")# combind[1:2,]
  list_Letters <- NULL
  for(cc in 1:ncol(combind)){#cc=1
    list_Letters <-c(list_Letters,  paste(combind[1,cc],   combind[2,cc], sep=""))
  }
  list_Letters <- c(combind_A, list_Letters) #combine
  if(length(unique(x))<=350){
    clust_num <- list_Letters[x]
    return(clust_num)
  }
  if(length(unique(x))>350){
    print("Labels exhausted! specify a vector with fewer elements")
  }
}
