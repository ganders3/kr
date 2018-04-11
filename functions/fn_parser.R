findDataLines = function(tableName, lines) {

    #=====================================================================
    TABLE_NAMES = c(
        'Jobs data',
        'People data'
    )
    OFFSET_HEADER = 4
    #=====================================================================
    
    tableNames = paste(TABLE_NAMES, collapse = '|')
    allTableRows = grep(tableNames, lines)
    thisTableRow = grep(tableName, lines)
    startRow = thisTableRow + OFFSET_HEADER
    endRow = c(
        allTableRows[allTableRows > thisTableRow] - 1,
        length(lines)
    ) %>% min
    
    return(lines[startRow:endRow])
}



createDataFrame = function(lines, delim, key, dateFormat) {
    
    #============================================================================
    PATTERNS_TO_REMOVE = c(
        paste0('^[', delim, '-]+$')
    )
    #============================================================================
    
    patternsToRemove = paste(PATTERNS_TO_REMOVE, collapse = '|')
    
    header = lines[1] %>%
        strsplit(delim) %>%
        unlist()
    keepCols = !grepl('^$', header)
    header = header[keepCols]

    keepRows = !grepl(patternsToRemove, lines)
    keepRows[1] = F

    data = lines[keepRows]
    nRows = length(data)
    data = data %>%
        strsplit(delim) %>%
        unlist() %>%
        matrix(nrow = nRows, byrow = T) %>%
        data.frame(stringsAsFactors = F) %>%
        .[, keepCols]
    colnames(data) = make.names(header)
    
    #-------Use the key to set field types---------
    # Convert key fields to valid names
    key$field = make.names(key$field)
    # Convert columns to their appropriate data types
    data[key$field[key$class == 'factor']] = lapply(data[key$field[key$class == 'factor']], as.factor)
    data[key$field[key$class == 'integer']] = lapply(data[key$field[key$class == 'integer']], as.integer)
    data[key$field[key$class == 'numeric']] = lapply(data[key$field[key$class == 'numeric']], as.numeric)
    data[key$field[key$class == 'character']] = lapply(data[key$field[key$class == 'character']], as.character)
    
    data[key$field[key$class == 'Date']] = lapply(data[key$field[key$class == 'Date']], function(x) as.POSIXct(x, format = dateFormat))
    
    return(data)
}