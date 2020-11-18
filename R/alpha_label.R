#' @title Numerics ids to alphabetical ids
#' @description Function to transform a list of numeric ids
#' to alphabetic ids
#' @param x A vector of numeric ids
#' @usage alpha_label(x)
#' @details Given a vector of numeric cluster ids,
#' `alpha_label` converts each id to its corresponding alphabets.
#' It combines alphabets for ids greater than 26.
#'
#' @examples
#'
#' data(TO1Risk)
#'
#' set.seed(1000)
#' #pick 4 random clusters
#' center <- TO1Risk[runif(4,1,nrow(TO1Risk)), ]
#'
#' #Assigning each individual to nearest centre
#' numeric_Labels <- kml::affectIndivC(TO1Risk, center)
#'
#' mode(numeric_Labels)
#'
#' #transform numeric cluster labels to alphabets
#' alphab_Labels <- alpha_label(numeric_Labels)
#'
#' mode(alphab_Labels)
#'
#' @return A vector of alphabetical ids.
#' @importFrom kml affectIndivC
#' @importFrom utils combn
#' @export
#'
alpha_label <- function(x){

  if(is.character(x)){
    stop("*== A vector of numbers is required! ==*")
  }

  combind_A <- LETTERS
  combind <-  combn(LETTERS, m=2, sep="")

  list_Letters <- NULL
  for(cc in seq_len(ncol(combind))){#cc=1
    list_Letters <-c(list_Letters,  paste(combind[1,cc],
                                          combind[2,cc], sep=""))
  }
  list_Letters <- c(combind_A, list_Letters)
  if(length(unique(x))<=350){
    #clust_num <- list(alphabetic_Labels=list_Letters[x])
    clust_num <- list_Letters[x]
    return(clust_num)
  }
  if(length(unique(x))>350){
    stop("Labels exhausted! specify a vector with fewer elements")
  }
}
