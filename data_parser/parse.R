library(readr)
library(stringr)
library(dplyr)

rm(list = ls())

#===================================================================='
FILE_PATH = '/home/gregory/kr/data_parser'
LINEBREAK = '\n'
DELIM = ','

jobsKey = read.csv(paste0(FILE_PATH, '/', 'jobs_key.csv'), header = T, stringsAsFactors =  F)
peopleKey = read.csv(paste0(FILE_PATH, '/', 'people_key.csv'), header = T, stringsAsFactors = F)
#===================================================================='

# content = read_file(paste0(FILE_PATH, '/', 'data.csv'))
# lines = unlist(strsplit(content, LINEBREAK))
lines = readLines(paste0(FILE_PATH, '/', 'data.csv'))

findDataRows = function(header, dataLines) {
    startRow = lapply(dataLines, function(line) {
        lapply(header, function(h) {
            grepl(h, line)
        }) %>%
            unlist() %>%
            all()
    }) %>%
        unlist() %>%
        which()
    
    emptyRows = grep('^,*$', dataLines)
    endRow = min(emptyRows[emptyRows > startRow]) - 1
    if (endRow == Inf) {endRow = length(lines)}
        
    return(c(startRow, endRow))
}


jobsRows = findDataRows(jobsKey$field, lines)
peopleRows = findDataRows(peopleKey$field, lines)

jobsData = lines[jobsRows[1]:jobsRows[2]]
peopleData = lines[peopleRows[1]:peopleRows[2]]

# df <- data.frame(matrix(unlist(l), nrow=132, byrow=T))
# data.frame(t(sapply(mylistlist,c)))

createDataFrame = function(dataLines, dataKey) {
    
}

# createDataFrame = function()
header = unlist(strsplit(jobsData[1], ','))

jobsData = jobsData[-1]
s1 = strsplit(jobsData, ',')
s2 = unlist(s1)
s3 = matrix(s2, nrow = length(s1), byrow = T)
s4 = data.frame(s3, stringsAsFactors = F)

validCols = which(header != '')
header = header[validCols]
s4 = s4[, validCols]
colnames(s4) = make.names(header)

jobsKey$field = make.names(jobsKey$field)