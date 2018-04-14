#=========================================================
source('/home/gregory/kr/functions/fn_packages.R')

PACKAGES_TO_INSTALL = c('tcltk2')
DOWNLOAD_DIR = '/home/gregory/Downloads'
#=========================================================

downloadPackages(PACKAGES_TO_INSTALL, DOWNLOAD_DIR)
installPackages(DOWNLOAD_DIR)