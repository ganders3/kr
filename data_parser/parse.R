library(readr)
library(stringr)
library(dplyr)

rm(list = ls())

#===================================================================='
FILE_PATH = '/home/gregory/Desktop/data_parser/data.csv'
LINEBREAK = '\n'
DELIM = ','

jobsKey = read.csv('/home/gregory/Desktop/data_parser/jobs_key.csv', header = T, stringsAsFactors =  F)
peopleKey = read.csv('/home/gregory/Desktop/data_parser/people_key.csv', header = T, stringsAsFactors = F)
#===================================================================='

content = read_file(FILE_PATH)
lines = unlist(strsplit(content, LINEBREAK))

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