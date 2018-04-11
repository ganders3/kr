coordDMS2Dec = function(coordDMS) {
    hemisphere = regmatches(coordDMS, regexpr('[NnSsEeWw]{1}', coordDMS)) %>% tolower()
    coord = regmatches(coordDMS, regexpr('[0-9]+', coordDMS))
    
    if (nchar(coord) %>% between(6, 7)) {
        output = 
            stringr::str_sub(coord, -2, -1) %>% as.numeric() / 3600 +
            stringr::str_sub(coord, -4, -3) %>% as.numeric() / 60 +
            stringr::str_sub(coord, -7, -5) %>% as.numeric()
    } else {
        output = 0
    }
    if (hemisphere %in% c('s', 'w')) {output = -1*output}
    return(output)
}