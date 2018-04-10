#===================================================================='
source('/home/gregory/kr/functions/fn_parser.R')

FILE_PATH = '/home/gregory/kr/data_parser'
LINEBREAK = '\n'
DELIM = ','

jobsKey = read.csv(paste0(FILE_PATH, '/', 'jobs_key.csv'), header = T, stringsAsFactors =  F)
peopleKey = read.csv(paste0(FILE_PATH, '/', 'people_key.csv'), header = T, stringsAsFactors = F)
#===================================================================='

lines = readLines(paste0(FILE_PATH, '/', 'data.csv'))

jobsLines = findDataLines('Jobs data', lines)
peopleLines = findDataLines('People data', lines)

jobs = createDataFrame(jobsLines, DELIM, jobsKey)
people = createDataFrame(peopleLines, DELIM, peopleKey)