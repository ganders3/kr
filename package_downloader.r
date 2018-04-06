# Package downloader from https://stackoverflow.com/questions/17190103/offline-installation-of-r-packages
# Set the packages you want to install as a char vector and the destination directory

#=========================================================
PACKAGES_TO_INSTALL = c('ggplot2', 'shiny', 'dplyr', 'tidyr')
DEST_DIR = '/destination/to/download' #/home/gregory/Downloads'
#=========================================================

getPackages = function(packageList){
  dependentPackages = unlist(
    tools::package_dependencies(
      packageList,
      available.packages(),
      which = c('Depends', 'Imports'),
      recursive = TRUE
    )
  )
  packages = union(packageList, dependentPackages)
  packages
}

p = getPackages(PACKAGES_TO_INSTALL)

download.packages(
  pkgs = p,
  destdir = DEST_DIR,
  type = 'win.binary'
  )