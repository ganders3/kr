library(stringr)

asCoordinate = function(string, stringFormat) {
    h = 'h'
    m = 'mm'
    s = 'ss'
    numbersLength = string %>% str_extract_all('[0-9]') %>% lapply(function(x) length(x))
    d = ifelse(numbersLength == 7,
               'ddd',
               ifelse(numbersLength == 6,
                      'dd',
                      ''))
    
    patternH = stringFormat %>% str_replace('%h', h) %>%
        str_replace('%d', strrep('x', nchar(d))) %>%
        str_replace('%m', strrep('x', nchar(m))) %>%
        str_replace('%s', strrep('x', nchar(s)))
    patternD = stringFormat %>% str_replace('%d', d) %>%
        str_replace('%h', strrep('x', nchar(h))) %>%
        str_replace('%m', strrep('x', nchar(m))) %>%
        str_replace('%s', strrep('x', nchar(s)))
    patternM = stringFormat %>% str_replace('%m', m) %>%
        str_replace('%d', strrep('x', nchar(d))) %>%
        str_replace('%h', strrep('x', nchar(h))) %>%
        str_replace('%s', strrep('x', nchar(s)))
    patternS = stringFormat %>% str_replace('%s', s) %>%
        str_replace('%d', strrep('x', nchar(d))) %>%
        str_replace('%m', strrep('x', nchar(m))) %>%
        str_replace('%h', strrep('x', nchar(h)))
    
    hemisphere = string %>% str_sub(patternH %>% str_locate(h)) %>% tolower()
    degree = string %>% str_sub(patternD %>% str_locate(d)) %>% as.numeric()
    minute = string %>% str_sub(patternM %>% str_locate(m)) %>% as.numeric
    second = string %>% str_sub(patternS %>% str_locate(s)) %>% as.numeric()
    print(paste0(string, ':: ', 'ph: ', patternH, ', pd: ', patternD, ', pm: ', patternM, ', ps: ', patternS, 
                  d, ', deg: ', degree, ', min: ', minute, ', sec: ',
                 second, ', hem: ', hemisphere))
    sign = ifelse(hemisphere %in% c('s', 'w'), -1, 1)
    output = sign*(degree + (minute/60) + (second/3600))
    return(output)
}

replaceNot = function(string, findNot, replace) {
    beginningToFindNot = string %>%
        str_extract(paste0('^.*', findNot)) %>%
        str_replace(findNot, '') %>%
        str_replace_all('.', replace)
    
    findNotToEnd = string %>%
        str_extract(paste0(findNot, '.*$')) %>%
        str_replace(findNot, '') %>%
        str_replace_all('.', replace)
    
    output = ifelse((is.na(beginningToFindNot) | is.na(findNotToEnd)),
                    NA,
                    paste0(beginningToFindNot, findNot, findNotToEnd)
    )
    return(output)
}

n = 100
coords = paste0(
    round(180*runif(n)) %>% str_pad(width = 2, side = 'left', pad = '0'),
    '-',
    round(60*runif(n)) %>% str_pad(width = 2, side = 'left', pad = '0'),
    '-',
    round(60*runif(n)) %>% str_pad(width = 2, side = 'left', pad = '0'),
    sample(c('N', 'S', 'E', 'W'), n, replace = T)
    
) %>% matrix(nrow = n) %>% data.frame()
f = '%d-%m-%s%h'

coords = cbind(coords, coords)
coords[2] = coords[2] %>% lapply(function(x) asCoordinate(x, f))


