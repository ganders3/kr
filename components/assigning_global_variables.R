rm(list = ls())

makeGlobal = function(x) {
    for (i in x) {
        assign(paste0('var', i), runif(1), envir = .GlobalEnv)
    }    
}


x = c(1,2,3)

makeGlobal(x)

for (i in x) {
    assign(paste0('var', i), runif(1))
}    