##### This file contains all the user defined functions 

#userDefinedFunctions

# Tests whether row vectors are within a matrix; identifies rows in matrix accordingly 
vecInVecSeq <- function(testVectorMatrix, seqVectorMatrix) {
  r <- rep(FALSE,nrow(seqVectorMatrix))
  for (k in 1:nrow(testVectorMatrix)) {
    testElement = testVectorMatrix[k,]
    r <- r | apply(seqVectorMatrix, 1, function(x, test) isTRUE(all.equal(x, test)), testElement)
  }
  return(r)
}


# create distance matrix
mat2list <- function(D) {
  n = dim(D)[1]
  k <- 1
  e <- matrix(ncol = 3,nrow = n*(n-1)/2)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      e[k,] = c(i,j,D[i,j])
      k<-k+1
    }
  }
  return(e)
}

# Function to create the Minimum Spanning Tree
minSpanTree <- function(){
  #geodesicDist
  # The Geodesic distance is chosen because of its accuracy.
  
  # geodesic distance: Highly accurate estimate of the shortest distance between two points on an ellipsoid (default is WGS84 ellipsoid). The shortest path between two points on an ellipsoid is called the geodesic.
  
  longlat <- data.frame(v$long, v$lat)
  
  mDist <- distm(longlat, fun=distGeo) # in meters
  
  kmDist <- mat2list(mDist/1000) # in km
  kmDist <- kmDist[order(kmDist[,3]),] #sort edges
  
  #MST
  # creating Minimum Spanning Tree (Kruskal's algroithm)
  mst <- getMinimumSpanningTree(v$name, round(kmDist), algorithm = "Kruskal",
                                show.data = TRUE, show.graph = TRUE, check.graph = FALSE)
  # creating a graph of MST
  graph <- graph.data.frame(kmDist[,1:2],directed = FALSE, vertices = v)
  E(graph)$weight <- kmDist[,3]
  mstgraph <- minimum.spanning.tree(graph)
  
  par(mfrow=c(1,2), mar=c(0,1,0.75,0)) 
  plot(graph,vertex.label=nname)
  plot(mstgraph,vertex.shape="none",edge.label=round(E(mstgraph)$weight)) # MST With distances
  # plot(mstgraph,vertex.shape="none")# Cleaner MST Without distances
  
  return(list(mst,mstgraph))
}


# Function to create k clusters in the MST
clusteredMST <- function(k, mstgraph){
  # creating k clusters from full mst graph
  
  e.mst <- get.edges(mstgraph,1:ecount(mstgraph)) # matrix of edges of MST
  e.clust <- get.edges(mstgraph,1:(ecount(mstgraph)-k+1)) # matrix of edges of k cluster
  clust_idx <- vecInVecSeq(e.clust, e.mst) # row indicis cluster in MST
  
  ecol <- rep("grey80", ecount(mstgraph)) # default edge colour
  ecol[clust_idx] <- "red" # colour for clusters
  ew <- rep(2,ecount(mstgraph)) # default edge width
  ew[clust_idx] <- 5 # width for clust edges
  
  par(mfrow=c(1,1), mar=c(0,1,0.75,0)) 
  
  plot(mstgraph, vertex.shape="none",vertex.label.cex=0.85,
       edge.color=ecol, edge.width=ew,edge.label.cex=0.85,
       edge.label=round(E(mstgraph)$weight))
}


# Function to reverse locate a locatino based on its geo Location

loc <- function (longitude, latitude) 
{
  location <- list()
  url <- paste0("http://photon.komoot.de/reverse?lon=", 
                longitude, "&lat=", latitude)
  data <- getURL(url)
  returned_data <- fromJSON(data)
  city <- returned_data$features$properties$city
  country <- returned_data$features$properties$country
  location <- paste(city, country, sep = ", ")
  return(location)
}




