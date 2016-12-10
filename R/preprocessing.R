library(tidyverse)

ukdata <- read_tsv("dev/McGann2014/ukdata_3.txt") %>% 
    mutate(cutpoint = 1,
           variable_cp = paste(variable, cutpoint, sep="_gt"),
           ccode = 1,
           tcode = as.integer(year - min(year) + 1),
           qcode = as.numeric(factor(variable, levels = unique(variable))),
           rcode = as.numeric(factor(variable_cp, levels = unique(variable_cp))),
           y_r = round(leftp/100 * n),
           ktcode = (ccode-1)*max(tcode)+tcode) %>%
    arrange(ccode, tcode, qcode, rcode)
