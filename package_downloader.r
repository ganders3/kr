# Package downloader from https://stackoverflow.com/questions/17190103/offline-installation-of-r-packages
# Set the packages you want to install as a char vector and the destination directory

#=========================================================
PACKAGES_TO_INSTALL = c('list', 'package', 'names') #c('ggplot2', 'shiny', 'dplyr', 'tidyr')
DEST_DIR = '/destination/to/save/downloads/in' #/home/gregory/Downloads'
#=========================================================

downloadPackages = function(packageList, downloadDirectory){
  dependentPackages = unlist(
    tools::package_dependencies(
      packageList,
      available.packages(),
      which = c('Depends', 'Imports'),
      recursive = TRUE
    )
  )
  packages = union(packageList, dependentPackages)

  try(download.packages(
      pkgs = packages,
      destdir = downloadDirectory,
      type = 'win.binary'
      ))
}