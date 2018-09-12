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
  
}






















