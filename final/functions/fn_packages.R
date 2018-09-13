# Package downloader from https://stackoverflow.com/questions/17190103/offline-installation-of-r-packages
# Set the packages you want to install as a char vector and the destination directory
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


# Installs all packages from their binary files, stored in a user-defined directory
installPackages = function(directory) {
    files = dir(directory)
    for (file in files) {
        if (tolower(tools::file_ext(file)) == 'zip') {
            print(paste0('Attempting to install package from ', file, '...'))
            try(install.packages(paste0(directory, '/', file), repos = NULL, type = 'source'))
        }
    }
}