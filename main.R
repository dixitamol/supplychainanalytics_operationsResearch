rm(list=ls())


source('functions.R')
source('pkgs.R')
source('info.R')

installpkgs()

#combinedInfoInVec
v <- data.frame(ids = 1:10,
                name=nname,
                lat=lat,
                long=long)


#CitiesOnMap

# DistributionCentres on a Map
leaflet(data=v[1:10,]) %>% addTiles() %>%
  addMarkers(lng=v$long, lat=v$lat, popup=v$name)

# Linkages between DistributionCentres shown on a Map
leaflet(data=v[1:10,]) %>% addTiles() %>%
  addMarkers(lng=v$long, lat=v$lat, popup=v$name) %>%
  addPolylines(lng=v$long, lat=v$lat, popup=v$name)

#Function Call for MST
minSpan <- minSpanTree()
paste('Min Spanning Tree Distance =', minSpan[[1]]$weight, 'kms', sep=' ')


#Function Call for Clusters
k <- 3 #Number of clusters required as per Objective 
clusteredMST(k, minSpan[[2]])


# WarehouseLocations
# Finding the warehouse locations in Clusters

l <- length(unique(cid))
v <- data.frame(v,cid)
if (l==k) {
  for(i in 1:l){
    vi <- v[v$cid==i,]
    vi$name <- as.character(vi$name)
    cities <- c()
    # print(paste("The cities in Cluster",i,"are : ", sep=' '))
    for(j in 1:nrow(vi)){
      cities <- append(cities, vi[j, "name"])
      # print(vi[j, "name"])
    }
    
    centrei <- COGravity(vi$long,vi$lat)
    
    print(leaflet(data=vi[1:nrow(vi),]) %>% addTiles() %>%
            addMarkers(lng=vi$long, lat=vi$lat, popup=vi$name) %>%
            addAwesomeMarkers(lng=centrei[1], lat=centrei[3], 
                              icon = awesomeIcons(markerColor = "red"),
                              popup=paste("Centre of Cluster ", i)))
    
    location <- loc(longitude=centrei[1], latitude=centrei[3])
    geoLocation <- c(longitude=centrei[1], latitude=centrei[3] )
    print(list(ClusterNumber= i, ClusterCities = cities, GeoLocation = geoLocation, Centroid = location))
    # print(paste("Center of Cluster", i, "is at longitude = ", centrei[1],
    #             "and latitude = ", centrei[3], "which is at ", location))
    if(i<3) readline(prompt="Press review map and then press [Enter] in console area")
     
    }
} else print('Please check the number of clusters and update vector <cid> with cluster information')



