library(tools)

#===========================================================
PACKAGE_DIR = 'directory/containing/package/zip/files'
#===========================================================

installPackages = function(directory) {
    files = dir(directory)
    for (file in files) {
        if (tolower(file_ext(file)) == 'zip') {
            print(paste0('Attempting to install package from ', file, '...'))
            try(install.packages(paste0(PACKAGE_DIR, '/', file), repos = NULL, type = 'source'))
        }
    }
}