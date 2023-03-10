getFactor <- function(x) {ifelse(x >= 25, 1,
                                 ifelse(x >= 10, 0.5,
                                        ifelse(x <= 10, 0)))}
