library(stringr)

asCoordinate = function(string, stringFormat) {
    h = 'h'
    m = 'mm'
    s = 'ss'
    numbersLength = string %>% str_extract_all('[0-9]') %>% lapply(function(x) length(x))
    d = ifelse(numbersLength == 7, 'ddd',
               ifelse(numbersLength == 6, 'dd', ''))
    print(d)
    patternH = stringFormat %>% str_replace_all('[^(%h)]', 'x') %>% str_replace('(%h)', h)
    patternD = stringFormat %>% str_replace_all('[^(%d)]', 'x') %>% str_replace('(%d)', d)
    patternM = stringFormat %>% str_replace_all('[^(%m)]', 'x') %>% str_replace('(%m)', m)
    patternS = stringFormat %>% str_replace_all('[^(%s)]', 'x') %>% str_replace('(%s)', s)

    hemisphere = string %>% str_sub(patternH %>% str_locate(h)) %>% tolower()
    degree = string %>% str_sub(patternD %>% str_locate(d)) %>% as.numeric()
    minute = string %>% str_sub(patternM %>% str_locate(m)) %>% as.numeric
    second = string %>% str_sub(patternS %>% str_locate(s)) %>% as.numeric()
    
    sign = ifelse(hemisphere %in% c('s', 'w'), -1, 1)
    output = sign*(degree + (minute/60) + (second/3600))
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
