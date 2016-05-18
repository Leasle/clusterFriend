#clusters is a list of lists that consist of vectors of elements. Vector contain identificator and vector coordinates
#clusters <- list(list(c("id123", 1, 2, 3)), list(c("id124", 1, 2, 4)))
#distances <- list(list(c("id123", 1, 2, 3)), list(c("id124", 1, 2, 4)), 23)



# Calculate distanse between two elements
distance <- function(x,y,w) {
  squareSumm <- 0
  squareSumm <- sum(squareSumm, w*(x-y)^2);

    return(sqrt(squareSumm))
}

#List of lists with identifiers of elements and distance between them
distanceList <- function(x,w) {
  n <- length(x)

  clustersList <- list()

  for(l in 1:(n-1)) {
    for(j in (l+1):n) {
      clustersList[[length(clustersList)+1]] <- list(first=x[[l]][[1]],
                                                   second=x[[j]][[1]],
                                                   distance=distance(lapply(x[[l]],function(el) as.numeric(el[2:length(el)]))[[1]],
                                                                     lapply(x[[j]],function(el) as.numeric(el[2:length(el)]))[[1]],w))
    }
  }

  return(clustersList)
}

#Pairs of elements and distances between them that less than or equals sigma
sigmaDistanseList <- function(distances, sigma){
  sigmaDistanse <- list()

  for (pairCluster in distances) {
    sigmaDistanse[[length(sigmaDistanse)+1]] <- pairCluster[pairCluster$distance <= sigma]
  }

  sigmaDistanse <- sigmaDistanse[lapply(sigmaDistanse, length) > 0]

  return(sigmaDistanse)
}

#Remove clusters with min distance
removeClusters <- function(clusters, pairClusters) {

  boolClusters <- rep(0, length(clusters))

  for (clusterPair in pairClusters) {
    for (cluster in clusterPair) {
      booleanList <- lapply(clusters,
                            function(x) {
                              equals <- FALSE
                              for (element in x) {
                                if (identical(all.equal(element[1], cluster[1]),TRUE))
                                  equals <- TRUE
                              }

                              return(equals)
                            })

      l <- 1
      while (l <= length(booleanList)) {
        boolClusters[l] <- sum(boolClusters[l], booleanList[[l]][1])
        l <- l + 1
      }
    }
  }

  i <- 1
  while(i <= length(boolClusters)){
    if(boolClusters[i]) {
      clusters[[i]] <- NULL
      boolClusters <- boolClusters[-i]
      i <- i-1
    }
    i <- i+1
  }

  return(clusters)
}

#Append new cluster
addCluster <- function(clusters, pairClusters) {
  newCluster <- list()

  if (is.list(pairClusters[[1]]) && is.list(pairClusters[[2]])) {
    newCluster <- pairClusters[[1]]

    for (cluster in pairClusters[[2]]) {
      newCluster[[length(newCluster)+1]] <- cluster
    }

  }
  else if (is.list(pairClusters[[1]]) && !is.list(pairClusters[[2]])) {
    pairClusters[[1]][[length(pairClusters[[1]])+1]] <- pairClusters[[2]]
    newCluster <- pairClusters[[1]]
  }
  else if (!is.list(pairClusters[[1]]) && is.list(pairClusters[[2]])) {
    pairClusters[[2]][[length(pairClusters[[2]])+1]] <- pairClusters[[1]]
    newCluster <- pairClusters[[2]]
  }
  else newCluster <- list(pairClusters[[1]], pairClusters[[2]])

  clusters[[length(clusters)+1]] <- newCluster

  return(clusters)
}

#Center element of cluster
centerElementCluster <- function(x) {
  clusterMatrix <- sapply(x, function(y){as.numeric(y[2:length(y)])})

  centerElement <- clusterMatrix[,1]

  if (ncol(clusterMatrix) > 1) {
    for (j in 2:ncol(clusterMatrix)) {
      centerElement <- centerElement + clusterMatrix[,j]
    }
  }

  centerElement <- centerElement / ncol(clusterMatrix)

  return(centerElement)
}

#Calculate distance between new cluster and others
distanceUord <- function(x,y,w) {
  xN <- length(x)
  yN <- length(y)

  centerElementX <- centerElementCluster(x)
  centerElementY <- centerElementCluster(y)

  distanceClusters <- xN*yN/sum(xN, yN)*distance(centerElementX, centerElementY, w)^2
    #distance(centerElementX, centerElementY, w)

  return(distanceClusters)
}

#Remove distances between two clusters and others
removeDistances <- function(distances, pairClusters) {

  massIdentificators <- c()
  for (pairIndex in pairClusters) {
    if (is.list(pairIndex)) {
      for (element in pairIndex) {
        massIdentificators <- append(massIdentificators, element[1])
      }
    } else {
      massIdentificators <- append(massIdentificators, pairIndex[1])
    }
  }

  booleanList <- lapply(distances,
                        function(x) {
                          equals <- FALSE
                          for (element in x) {
                            if (is.list(element)) {
                              for (elem in element) {
                                if (elem[1] %in% massIdentificators) {
                                  equals <- TRUE
                                  return(equals)
                                }
                              }
                            } else if (element[1] %in% massIdentificators)
                                      equals <- TRUE
                          }

                          return(equals)
                        }
                        )

  i <- 1
  while(i <= length(booleanList)){
    if(booleanList[[i]]) {
      distances[[i]] <- NULL
      booleanList[[i]] <- NULL
      i <- i-1
    }
    i <- i+1
  }

  return(distances)
}

#Distance between new cluster and others
distanceNewCluster <- function(clusters,distances,w) {
  colClusters <- length(clusters)

  if (colClusters > 1) {
    for (index in 1:(colClusters-1)) {
      distances[[length(distances)+1]] <- list(first=clusters[[index]],
                                               second=clusters[[colClusters]],
                                               distance=distanceUord(clusters[[index]], clusters[[colClusters]],w))
    }
  }

  return(distances)
}

#Read data of elements from JS
readData <- function(json) {
  #clusters <- list(list(c("id123", 1, 2, 3, 4, 9)), list(c("id124", 1, 2, 4, 5, 30)), list(c("id125", 1, 3, 2, 9, 31)),
  #                 list(c("id126", 3, 6, 10, 8, 15)), list(c("id127", 4, 12, 30, 5, 28)), list(c("id128", 2, 10, 7, 3, 26)),
  #                 list(c("id129", 2, 2, 5, 9, 10)), list(c("id130", 3, 5, 4, 9, 15)), list(c("id131", 4, 2, 4, 8, 12)), list(c("id132", 2, 10, 3, 4, 10)))

  library(jsonlite)
  tableJsonClusters <- as.matrix(fromJSON(json))
  View(tableJsonClusters)
  colnames(tableJsonClusters) <- NULL

  clusters <- list()

  for (indexRow in 1:nrow(tableJsonClusters)) {
    clusters[[length(clusters)+1]] <- list(tableJsonClusters[indexRow,])
  }

  return(clusters)
}

#Write data of clusters to JSON
writeData <- function(clusters) {
  #clusters <- list(list(c("id123", 1, 2, 3, 4, 9)), list(c("id124", 1, 2, 4, 5, 30)), list(c("id125", 1, 3, 2, 9, 31)),
  #                 list(c("id126", 3, 6, 10, 8, 15)), list(c("id127", 4, 12, 30, 5, 28)), list(c("id128", 2, 10, 7, 3, 26)),
  #                 list(c("id129", 2, 2, 5, 9, 10)), list(c("id130", 3, 5, 4, 9, 15)), list(c("id131", 4, 2, 4, 8, 12)), list(c("id132", 2, 10, 3, 4, 10)))

  library(jsonlite)

  clustersMatrix <- list()

  for (index in 1:length(clusters)) {
    clustersMatrix[[length(clustersMatrix)+1]] <- t(sapply(clusters[[index]], function(x) x))
  }

  tableJsonClusters <- toJSON(clustersMatrix, pretty = TRUE)

  tableJsonClusters
}

#Save mass of indexes that does not consider
#Or rewrite mass of elements

fastAglomerativeClustering <- function(clusters, weight, sigma){
  iterNum <- 1

  pSigma <- c()

  distances <- distanceList(clusters, weight)

  pairClustersSigma <- sigmaDistanseList(distances, sigma)

  indexPairMaxDistance <- which.max(lapply(distances, function(x) x$distance))
  maxDistance <- distances[[indexPairMaxDistance]]$distance
  clustersMaxDistance <- clusters

  while (TRUE) {

    while (length(pairClustersSigma) == 0) {
      sigma <- sum(sigma, 10)
      pairClustersSigma <- sigmaDistanseList(distances, sigma)
    }

    indexPairMinDistance <- which.min(lapply(pairClustersSigma, function(x) x$distance))
    minDistance <- pairClustersSigma[[indexPairMinDistance]]$distance
    pairClusters <- list(pairClustersSigma[[indexPairMinDistance]]$first, pairClustersSigma[[indexPairMinDistance]]$second)

    #print("pairMinDistance")
    #print(pairClustersSigma[[indexPairMinDistance]])

    indexPairMaxDistanceTemp <- which.max(lapply(distances, function(x) x$distance))
    maxDistanceTemp <- distances[[indexPairMaxDistanceTemp]]$distance

    if (maxDistanceTemp > maxDistance) {
      maxDistance <- maxDistanceTemp
      clustersMaxDistance <- clusters
    }

    clusters <- removeClusters(clusters, pairClusters)
    distances <- removeDistances(distances, pairClusters)

    clusters <- addCluster(clusters, pairClusters)

    if (length(clusters) == 1) {
      break()
    }

    print("clusters ==============================================================")
    print(clusters)

    distances <- distanceNewCluster(clusters, distances, weight)

    pairClustersSigma <- sigmaDistanseList(distances, sigma)
  }

  print("max distance")
  print(maxDistance)
  print(clustersMaxDistance)

  return(clustersMaxDistance)
}

#' @export
#' @param json of users. Required.
main <- function(jsonClusters) {

  #stopifnot(length(jsonClusters) > 2)

  clusters <- readData(jsonClusters)

  weight <- c(1, 1, 1, 1, 1)
    #c(0.3, 0.1, 0.5, 0.05, 0.05)

  sigma <- 5

  cluster <- fastAglomerativeClustering(clusters, weight, sigma)

  print(cluster)

  result <- writeData(cluster)

  return(list(result))
}

#' @export
hello <- function() {
  library(jsonlite)

  hello <- list("hello")

  helloJson <- toJSON(hello, pretty = TRUE)

  return(list(helloJson))
}
