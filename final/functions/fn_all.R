readFromDatabase = function(path, dataPattern, whichData = '') {
  print(strrep('=', 100))
  print('Reading files from database:')
  for (i in whichData) {
    msg = paste0(dataPattern, i, ': ')
    filePath = paste0(path, '\\', dataPattern, i, '.csv')
    if (file.exists(filePath)) {
      msg = paste0(msg, 'reading from ', filePath)
      data = read.csv(filePath, header = T)
      assign(paste0(dataPattern, i), data, envir = .GlobalEnv)
    } else {
      msg = paste0(msg, 'no file found in database.')
    }
    print(msg)
  }
}

# Read in all keys in the key path
readKeys = function(keyPath = PATH_DATA_KEYS) {
  for (file in dir(PATH_DATA_KEYS)) {
    if (tools::file_ext(file) %>% tolower() == 'csv') {
      assign(file %>% str_extract('\\w+'),
             read.csv(paste0(PATH_DATA_KEYS, '\\', file), header = F),
             envir = .GlobalEnv)
    }
  }
}


# Load keys into the working environment
initializeKeys = function(keyPattern, whichData) {
  for (i in whichData) {
    keyName = paste0(keyPattern, i)
    if (!exists(keyName)) {
      assign(keyName, matrix(nrow = 0, ncol = 2) %>% data.frame(),
             envir = .GlobalEnv)
    }
  }
}


initializeTempDataFrames = function(dfPattern, keyPattern, whichData) {
  for (i in whichData) {
    # Find the key, or create an empty one
    keyName = paste0(keyPattern, i)
    if (exists(keyName)) {
      key = get(dfName)
    } else {
      key = matrix(nrow = 0, ncol = 2) %>% data.frame()
    }
    
    # Find the data frame, or create an empty one
    dfName = paste0(dfPattern, i)
    if (exists(dfName)) {
      tempData = get(dfName)
    } else {
      tempData = matrix(nrow = 0, ncol = nrow(key)) %>% data.frame()
    }
    tempData = setDataFrameNames(tempData, key)
    assign(paste0('temp_', dfName), tempData, envir = .GlobalEnv)
  }
}


# Write to each ASTAB as a csv
writeDataFrames = function(path, dfPattern, whichData) {
  print(strrep('=', 100))
  for (i in whichData) {
    dfName = paste0(dfPattern, i)
    if (exists(dfName)) {
      print(paste0('Saving ', dfName, '.csv'))
      write.csv(get(dfName), file = paste0(path, '\\', dfName, '.csv'), row.names = F)
    } else {
      print(paste0('Variable ', dfName, ' not found'))
    }
  }
}

readFilesRead = function(path = PATH_CONFIG, fileName) {
  filePath = paste0(path, '\\', fileName, '.txt')
  if (file.exists(filePath)) {
    return(readLines(filePath))
  } else {
    return(c())
  }
}

writeFilesRead = function(filesRead, path = PATH_CONFIG, fileName = 'files_read') {
  print(strrep('=', 100))
  filePath = paste0(path, '\\', fileName, '.txt')
  print(paste0('Updating ', fileName, ': saving to ', filePath))
  writeLines(filesRead, sep = '\n', con = filePath)
}

assignTempToDf = function(tempPattern, dfPattern, whichData) {
  for (i in whichData) {
    tempName = paste0(tempPattern, i)
    dfName = paste0(dfPattern, i)
    if (exists(tempName)) {
      assign(dfName, get(tempName), envir = .GlobalEnv)
    }
  }
}


#dataFrame, dataKey = NULL, dateFormat = NULL, coordCols = c(), coordFormat = NULL
createRDataFiles = function(pathDatabase, dfPattern, whichData, keyPattern, dateFormat, coordCols, coordFormat) {
  for (i in whichData) {
    dfName = paste0(dfPattern, i)
    keyName = paste0(keyPattern, i)
    if (exists(dfName) & exists(keyName)) {
      key = get(keyName)
      data = get(dfName) %>% setDataFrameTypes(key, dateFormat, coordCols, coordFormat)
      print(paste0('Saving RData file for ', dfName))
      save(list = ls(pattern = '^data$'), file = paste0(pathDatabase, '\\', 'r_', dfName, '.RData'))
    }
  }
}

loadRDataFiles = function(path, dfPattern, whichData) {
  for (i in whichData) {
    dfName = paste0(dfPattern, i)
    rDataPath = paste0(path, '\\', dfName, '.RData')
    if (file.exists(rDataPath)) {
      print(paste0('Loading RData for ', dfName))
      data = load(rDataPath)
      workingDfName = paste0('r_', dfName)
      assign(workingDfName, data)
    } else {
      print(paste0('No RData found for ', dfName))
    }
  }
}

readNewDataFiles = function(path, filesRead, dfPattern, keyPattern, whichData, fileType) {
  print(strrep('=', 100))
  print('This might take a while...go get a beer.')
  print(paste0('Reading raw data from ', path))
  
  nFilesRead = 0
  nNewFiles = dir(path) %>% match(files_read) %>% is.na() %>% which() %>% length()
  for (file in dir(path)) {
    print(strrep('=', 100))
    print(paste0('Reading file ', file))
    
    fileType = tolower(fileType)
    if (fileType != 'astab' $ fileType != 'jquad') {fileType = 'jquad'}
    
    # If the file has not yet been read...
    if (check == T) {
      # Read the data lines from the file location
      allLines = readLines(paste0(path, '\\', file))
      
      for (i in whichData) {
        print(strrep('-', 100))
        print(paste0('Building ', dfPattern, i))
        
        tempName = paste0('temp_', dfPattern, i)
        key = get(paste0(keyPattern, i))
        
        
        tempData = get(tempName)
        print(paste0('Current data frame has ', nrow(tempData), ' rows.'))
        
        if (fileType == 'astab') {
          newData = allLines %>% findDataLinesAstab(i) %>% createDataFrameAstab()
        } else if (fileType = 'jquad') {
          newData = allLines %>% findDataLinesJQ(ALL_JQ_TABLES[[i]]) %>% createDataFrameJQ()
        }
        
        print(paste0('New file has ', nrow(newData), ' data rows.'))
        
        assign(paste0('a_', dfPattern, i), newData, envir = .GlobalEnv)
        if (nrow(newData) > 0) {
          if (ncol(tempData) == ncol(newData)) {
            if (!all(colnames(tempData) == colnames(newData))) {
              print('Warning: Column names do not match. Coercing them to be the same.')
              colnames(newData) = colNames(tempData)
            }
            tempData = rbind(tempData, newData)
            assign('checktemp', tempData)
            nDup = nrow(tempData) - nrow(unique(tempData))
            print(paste0('Removing ', nDup, ' duplicate rows.'))
            tempData = unique(tempData) %>% setDataFrameNames(key)
          } else {
            print('Mismatched number of columns. Not appending this file.')
          }
        }
        tempData = setDataFrameNames(tempData, key)
        assign(tempName, tempData, envir = .GlobalEnv)
      }
      
      # Append the list of files that have been read
      files_read = c(files_read, file)
      # and increment the number of files read
      nFilesRead = nFilesRead + 1
    } else {
      print(paste0('File has previously been read.'))
    }
    
    assignTempToDf(paste0('temp_', dfPattern), dfPattern, whichData)
  }
  print(strrep('=', 100))
  print(paste0('New files read: ', nFilesRead))
  assign('files_read', files_read, envir = .GlobalEnv)
  
  rm(list = ls(pattern = 'temp_'))
}

# Output a similarly unformatted list of lines containing only the lines of interest
fileDataLinesAstab = function(lines, astabNumber) {
  
  #=====================================================
  # The pattern containing the table name - looks something like:
  ASTAB_PATTERN = 'pattern containing table'
  NUM_ASTAB = 28
  #=====================================================
  
  # Create a regex pattern to find the ASTAB number of interest
  thisTablePattern = ASTAB_PATTERN %>% str_replace('ASTAB_NUM', as.character(astabNumber))
  # Create regex patterns for any ASTAB number, and combine all of these patterns with | (the regex way of designating "or")
  allTablePatterns = ASTAB_PATTERN %>% rep(NUM_ASTAB) %>% str_replace('ASTAB_NUM', as.character(1:NUM_ASTAB)) %>% paste(collapse = '|')
  
  # Find all rows containing the table name line, for the astabNumber of interest
  thisTableRows = grep(thisTablePattern, lines)
  # The start row is the first row containing these lines
  startRow = thisTableRows %>% min()
  
  # Find all rows containing any table name lines
  allTableRows = grep(allTablePatterns, lines)
  # The end row is the minimum of...
  endRow = min(
    # ... the rows containing a table name line which are AFTER the last line of the table of interest, or...
    c(allTableRows[allTableRows > max(thisTableRows)] - 1,
      # ...if there is no table after the table of interest, the length of the data lines (this applies to the last ASTAB in the data)
      length(lines))
  )
  
  # If the end row is greater than the start row (at least 1 line of data was found)...
  if (startRow < endRow) {
    # Return these data lines
    return(lines[startRow:endRow])
  } else {
    # Otherwise return an empty list
    return(list())
  }
}

findDataLinesJQ = function(lines, tableName) {
  #===========================================================
  # The pattern containing the line widths
  PATTERN_WIDTHS = '^[\t=-]+$'
  # The number of rows to offset from the table name to where the header starts
  OFFSET_TO_HEADER = 2
  #===========================================================
  
  # Create a regex pattern containing all table names
  allTableNames = paste(ALL_JQUAD_TABLES, collapse = '|')
  
  # Find all rows containing a table name
  allTableRows = grep(allTableNames, lines)
  # Find the row containing the tableName of interest
  thisTableRow = grep(tableName, lines)
  
  # The start row is the row where the header begins
  startRow = thisTableRow
  # Set the end row to the minimum of either...
  endRow = min(
    # ...the row before the NEXT table starts, or...
    c(allTableRows[allTableRows > thisTableRow] - 1,
      # ...the last row of the data (this applies when the table of interest is the last table - there will be no next table to find)
      length(lines))
  )
  
  # If any data is found...
  if (startRow < endRow) {
    # Return those lines
    return(lines[startRow:endRow])
  } else {
    # Otherwise return an empty list
    return(list())
  }
}

# From the trimmed data lines containing an ASTAB of interest, create a data frame
createDataFrameAstab = function(lines) {
  #========================================================
  PATTERN_TABLE_NAME = 'pattern containing table name'
  # A line of only spaces, tabs, =, and -
  PATTERN_WIDTHS = '^[[:blank:]=-]+$'
  
  # Patterns that should be removed from the data lines
  PATTERNS_TO_REMOVE = c(
    'list', 'of', 'patterns', 'that', 'are', 'not', 'data'
  )
  #========================================================
  
  # Find all rows containing the table name
  tableNameRows = grep(PATTERN_TABLE_NAME, lines)
  # Find all lines containing the width delimiter lines
  widthsRows = grep(PATTERN_WIDTHS, lines)
  # Determine how many rows of offset from the table name to the width row
  offsetToWidths = widthsRows[1] - tableNameRows[1]
  
  # Save the line of width delimiters as a single string
  widthsLine = lines[1 + offsetToWidths]
  # Split the widths line, turn it into a vector, take the number of characters of each width, and add 1 (for the spaces)
  # These widths will be used to determine at which substring indices each data field can be found
  widths = widthsLine %>% strsplit(' ') %>% unlist() %>% nchar() + 1
  # The end position is the cumulative sum of the widths
  end = cumsum(widths)
  # To get the start position, move back from the end, by the width, and add 1
  start = end - widths + 1
  
  # Game time, side, view
  # Initialize each list
  gameTimeList = rep('', length(lines))
  sideList = rep('', length(lines))
  viewList = rep('', length(lines))
  # For each table name row...
  for (i in tableNameRows) {
    # ...extract the game time, side, and view
    gameTime = 'pattern containing game time'
    side = 'pattern containing side'
    view = 'pattern containing view'
    # Determine the last row on which the current table name row applies
    iEnd = c(tableNameRows[tableNameRows > i], length(lines)) %>% min()
    # Set this portion of the lists to the game time, side, and view that was found at the table row we are currently looking at
    gameTimeList[i:iEnd] = gameTime
    sideList[i:iEnd] = side
    viewList[i:iEnd] = view
  }
  
  # Take the header line
  headerLine = lines[offsetToWidths]
  # Extract the substrings designated by the start and end vectors
  header = headerLine %>% str_sub(start, end) %>% unlist() %>% trimws()
  # To the headers, append the field names for game time, side, and view
  header = c(header, 'GAME.TIME', 'SIDE', 'VIEW')
  # Make a regex pattern for all patterns that should be removed
  patternsToRemove = paste(c(PATTERNS_TO_REMOVE, PATTERN_TABLE_NAME, PATTERN_WIDTHS), collapse = '|')
  
  # Determine which rows should be kept/removed
  # Initialize the rows to be kept
  keepRows = rep(T, length(lines))
  # Find all rows that contain...
  matches = union(
    # ...the patterns that should be removed,...
    grep(patternsToRemove, lines),
    # ...and all lines between the header and the offset
    lapply(lines[1:offsetToWidths], function(x) grep(x, lines, fixed = T)) %>% unlist()
  )
  # Update the list of rows to be kept, removing those that were matched
  keepRows[matches] = F
  
  # Extract the lines to be kept, in both the data, and...
  data = lines[keepRows]
  # ...in the lists of game time, side, and view
  gameTimeList = gameTimeList[keepRows]
  sideList = sideList[keepRows]
  viewList = viewList[keepRows]
  # Save the number of rows in the new data
  nRows = length(data)
  # If any data was found...
  if (nRows > 0) {
    # ...set the data:
    # Extract the appropriate substrings from the data lines, unlist to convert to a vector, and trim white space
    # This will make a list of length (number of rows)*(number of data field columns)
    lapply(function(x) str_sub(x, start, end)) %>% unlist() %>% trimws() %>%
      # Convert it to a matrix that contains the same number of rows as the original data
      matrix(nrow = nRows, byrow = T) %>%
      # And convert this to a data frame
      data.frame(stringsAsFactors = T) %>%
      # Bind to this data frame the lists of game time, side, and view
      cbind(gameTimeList, sideList, viewList)
    # If no data was found,...
  } else {
    # ...create an empty data frame with the same number of columns as the header
    data = matrix(nrow = 0, ncol = length(header)) %>% data.frame()
  }
  # Reformat the column names
  if (ncol(data) > 0) {
    if (length(header) == ncol(data)) {
      columnNames = makeUniqueNames(header)
    } else {
      columnNames = 1:ncol(data) %>% make.names(unique = T)
    }
    colnames(data) = columnNames
  }
  return(data)
}

# Creates a data frame from a list of lines
# The user provides the delimiter and a vector of regex patterns that should be excluded from the data
createDataFrameJQ = function(lines) {
  #====================================================================
  PATTERN_WIDTHS = '^[\t=-]+$'
  DELIM = '\t'
  PATTERNS_TO_REMOVE = c(
    paste0('^[', DELIM, '-]+$'),
    '^$',
    'other', 'patterns', 'of', 'fields', 'to', 'remove'
  )
  #====================================================================
  
  # If there are no data lines, return an empty data frame
  if (length(lines) == 0) {
    return(matrix(nrow = 0, ncol = 0) %>% data.frame())
  }
  
  # Find all lines containing the width delimiter lines
  widthsRows = grep(PATTERN_WIDTHS, lines)
  thisWidthRow = min(widthRows, length(lines))
  # Determine how manyu rows to offset from the table name to the width row
  offsetToWidths = thisWidthRow - 1
  
  # Save the line of width delimiters, as a single string
  widthsLine = lines[1 + offsetToWidths]
  
  # If no widths line is found, there is no data. Return an empty data frame
  if (grepl(PATTERN_WIDTHS, widthsLine) == F) {
    return(matrix(nrow = 0, ncol = 0) %>% data.frame())
  }
  # Take the header line
  headerLine = lines[offsetToWidths]
  
  # Create a regex pattern of rows to remove
  patternsToRemove = c(
    headerLine,
    PATTERNS_TO_REMOVE,
    PATTERN_WIDTHS,
    ALL_JQ_TABLES,
    ALL_JQ_TABLES %>% lapply(function(x) x %>% str_replace_all(' ', ''))
  )
  patternsToRemove = paste(patternsToRemove, collapse = '|')
  
  # Set the header as the first row of data
  header = headerLine %>%
    # Split it by the delimiter, convert to a vector, and trim white space
    strsplit(DELIM) %>% unlist() %>% trimws()
  # Keep columns that are not blank
  keepCols = !grepl('^$', header)
  # Set the header based on these columns
  header = header[keepCols]
  # Determine which rows not to keep:
  keepRows =
    # Those that contain unwanted patterns, and...
    !grepl(patternsToRemove, lines) &
    # ...lines that are not the length of the header
    (lines %>% lapply(function(x) %>% strsplit(DELIM) %>% unlist() %>% length() == length(header)) %>% unlist())
  # Also remove the first row since it's the header
  keepRows[1] = F
  
  data = lines[keepRows]
  # Save the number of rows in the new data
  nRows = length(data)
  
  # If data was found, set the data:
  if (nRows > 0) {
    data = data %>%
      # Split on the delimiter, convert to a vector, and trim white space
      strsplit(DELIM) %>% unlist() %>% trimws() %>%
      # Convert to a matrix with the proper number of rows, and convert to a data frame
      matrix(nrow = nRows, byrow = T) %>% data.frame(stringsAsFactors = T) %>%
      # Keep only the appropriate columns
      .[, keepCols]
  } else {
    # If no data was found, create an empty data frame
    data = matrix(nrow = 0, ncol = 0) %>% data.frame()
  }
  if (ncol(data) > 0) {
    # Reformat the column names
    # If the data has the same number of columns as the header, set the data header names to the header
    if (length(header) == ncol(data)) {
      columnNames = makeUniqueNames(header)
    } else {
      # If the data and header don't have the same number of columns, create unique names for the data columns
      columnNames = 1:ncol(data) %>% make.names(unique = T)
    }
    colnames(data) = columnNames
  }
  return(data)
}


setDataFrameNames = function(dataFrame, dataKey) {
  colnames(dataKey) = c('field', 'class')
  dataKey$field = makeUniqueNames(dataKey$field)
  
  if (nrow(dataKey) != ncol(dataFrame)) {
    print('Data frame and key do not have the same number of columns.')
    columnNames = 1:ncol(dataFrame) %>% make.names(unique = T)
  } else {
    columnNames = dataKey$field
  }
  colnames(dataFrame) = columnNames
  return(dataFrame)
}


setDataFrameTypes = function(dataFrame, dataKey = NULL, dateFormat = NULL, coordCols = c(), coordFormat = NULL) {
  
  if (nrow(dataFrame) == 0) {return(dataFrame)}
  
  if (is.null(dataKey) | ncol(dataKey) != 2) {
    dataKey = matrix(nrow = 0, ncol = 2) %>% data.frame()
  }
  colnames(dataKey) = c('field', 'class')
  dataKey$field = makeUniqueNames(dataKey$field)
  
  colsCharacter = whichColumns(dataFrame, dataKey, 'character')
  colsDate = whichColumns(dataFrame, dataKey, 'Date')
  colsFactor = whichColumns(dataFrame, dataKey, 'factor')
  colsInteger = whichColumns(dataFrame, dataKey, 'integer')
  colsLogical = whichColumns(dataFrame, dataKey, 'logical')
  colsNumeric = whichColumns(dataFrame, dataKey, 'numeric')
  
  coordCols = coordCols %>% tolower() %>% paste(collapse = '|')
  coordCols = dataKey$field[dataKey$field %>% lapply(function(x) x %>% tolower() %>% str_detect(coordCols)) %>% unlist()]
  
  dataFrame[colsCharacter] = dataFrame[colsCharacter] %>% lapply(as.character)
  dataFrame[colsFactor] = dataFrame[colsFactor] %>% lapply(as.factor)
  dataFrame[colsInteger] = dataFrame[colsInteger] %>% lapply(as.integer)
  dataFrame[colsLogical] = dataFrame[colsLogical] %>% lapply(as.logical)
  dataFrame[colsNumeric] = dataFrame[colsNumeric] %>% lapply(as.numeric)
  # Converting to date is not as straightforward as using as.Date
  # Need to convert to as.POSIXct and provide the date format
  dataFrame[colsDate] = dataFrame[colsDate] %>% lapply(function(x) x %>% trimws() %>% as.POSIXct(format = dateFormat))
  # To convert coordinates to a standard format, use the user-created function asCoordinate
  dataFrame[colsCoord] = dataFrame[colsCoord] %>% lapply(function(x) x %>% trimws() %>% asCoordinate(coordFormat) %>% as.numeric)
 
  return(dataFrame) 
}

# Makes a vector of unique variable names
makeUniqueNames = function(names) {
  names = names %>%
    trimws() %>%
    make.names(unique = T) %>%
    # Replace multiple .'s with only a single .
    str_replace('\\.+', '.') %>%
    # Remove .'s at the end of the variable name
    str_replace('\\.$', '')
  return(names)
}