library(dplyr)
library(stringr)

line = '============ ======= === = === ====== === ---------- ----- ----- ====='

widths = line %>% strsplit(' ') %>% unlist() %>% nchar() + 1
end = cumsum(widths)
start = end - widths

columns = str_sub(line, start, end) %>% unlist() %>% trimws()