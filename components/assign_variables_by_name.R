library(stringr)

# Save variables by name
table_names = c(
    'data_cheese' = 'TABLE OF CHEESE DATA',
    'data_sacks' = 'MASSIVE SACKAGE',
    'data_beans' = 'O BEANY BAGS',
    'data_suh' = 'SUH DUDE DATA'
)

for (dataVar in names(table_names)) {
    print(paste0('Find the table header ', table_names[dataVar], ' in the data lines'))
    assign(dataVar, paste0('The data from ', table_names[dataVar]))
    print(paste0('Save the variable ', dataVar, ' using: write.csv(get(dataVar), file = paste0(dataVar, ', '\'.csv\')'))
}


# Read variables by name
# Use dir() to get all of these files
data_files = c(
    'data_greg.csv',
    'data_joe.csv',
    'data_bill.csv',
    'data_bo.csv',
    'data_baggins.csv'
)

for (file in data_files) {
    data_table_name = str_extract(file, '\\w+')
    print(paste0('assign(', data_table_name ,', read.csv(', file, '))'))
}