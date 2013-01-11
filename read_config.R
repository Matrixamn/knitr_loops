config_source <- suppressWarnings(read.table(file='C:/Users/ctobin/Documents/R/knitr functions/Config.txt', sep="]", 
                    strip.white = TRUE, blank.lines.skip=TRUE, 
                    col.names = c('Parameter', 'Value'), stringsAsFactors = FALSE))
config_source[, 1] <- sapply(config_source[, 1], substring, 2)
config <- as.list(structure(config_source$Value, .Names=(config_source$Parameter)))