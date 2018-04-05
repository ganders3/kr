# Package downloader from https://stackoverflow.com/questions/17190103/offline-installation-of-r-packages
# Set the packages you want to install as a char vector and the destination directory
packages_to_install <- c("ggplot2", "shiny", "dplyr", "tidyr")
dest_dir <- "C:/Users/joeha/Downloads/r_packages"

getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(
      packs
      , available.packages()
      , which=c("Depends", "Imports")
      , recursive=TRUE
    )
  )
  packages <- union(packs, packages)
  packages
}

Packages <- getPackages(packages_to_install)

download.packages(
  pkgs=Packages
  , destdir=dest_dir
  , type="win.binary")
