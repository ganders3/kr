asCoordinate = function(string, stringFormat) {
    h = 'h'
    m = 'mm'
    s = 'ss'
    numbersLength = string %>% str_extract_all('[0-9]') %>% unlist() %>% length()
    if (numbersLength == 7) {
        d = 'ddd'
    } else {
        d = 'dd'
    }
    
    pattern = stringFormat %>%
        str_replace('%h', h) %>%
        str_replace('%d', d) %>%
        str_replace('%m', m) %>%
        str_replace('%s', s)
    
    patternH = pattern %>% str_replace_all('[^h]', 'x')
    patternD = pattern %>% str_replace_all('[^d]', 'x')
    patternM = pattern %>% str_replace_all('[^m]', 'x')
    patternS = pattern %>% str_replace_all('[^s]', 'x')
    
    hemisphere = string %>% str_sub(patternH %>% str_locate(h)) %>% tolower()
    degree = string %>% str_sub(patternD %>% str_locate(d)) %>% as.numeric()
    minute = string %>% str_sub(patternM %>% str_locate(m)) %>% as.numeric
    second = string %>% str_sub(patternS %>% str_locate(s)) %>% as.numeric()
    
    output = degree + (minute/60) + (second/3600)
    if (hemisphere %in% c('s', 'w')) {output = -1*output}
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

coords = cbind(sample(c('a', 'b', 'c'), n, replace = T), coords, coords)
coords[2:3] = coords[2:3] %>% apply(c(1,2), function(x) asCoordinate(x, f))
