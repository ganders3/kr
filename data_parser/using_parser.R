#===================================================================='
source('/home/gregory/kr/functions/fn_parser.R')
library(dplyr)

FILE_PATH = '/home/gregory/kr/data_parser'
LINEBREAK = '\n'
DELIM = ','

jobsKey = read.csv(paste0(FILE_PATH, '/', 'jobs_key.csv'), header = T, stringsAsFactors =  F)
peopleKey = read.csv(paste0(FILE_PATH, '/', 'people_key.csv'), header = T, stringsAsFactors = F)
#===================================================================='

lines = readLines(paste0(FILE_PATH, '/', 'data.csv'))

people = findDataLines('People data', lines) %>% createDataFrame(DELIM, peopleKey, '%Y%m%d')
jobs = findDataLines('Jobs data', lines) %>% createDataFrame(DELIM, jobsKey, '%m/%d/%Y')