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
  
}












































