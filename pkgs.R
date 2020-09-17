##### This file contains all packages and their installation

### install packages

# install.packages("igraph")
# install.packages("leaflet")
# install.packages("geosphere")
# install.packages("optrees")
# install.packages("SDMTools")
# install.packages("jsonlite")
# install.packages("dplyr")
# install.packages("RCurl")

installpkgs <- function(){
pkgs <- c("igraph", "leaflet", "geosphere", "optrees", "jsonlite",
          "SDMTools", "dplyr","RCurl","pacman")

#loadingLibs
library(pacman)
pacman::p_load(char=pkgs,install=TRUE,character.only=TRUE)
}