rm(list = ls())
library(tcltk)

folderPath = tclvalue(tkchooseDirectory())
writeLines(folderPath, 'path.txt')

checkPath = readLines('path.txt')